(ns hyperfiddle.navigator6.search
  (:require [clojure.string :as str]
            [dustingetz.str :as strx]
            [geoffreygaillard.debug-tools :refer [debug-exceptions]]
            [hyperfiddle.electric-forms5 :as forms]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.ct.pair :refer [snd]]
            [hyperfiddle.ct.cofree :refer [unwrap]]
            [hyperfiddle.hfql2 :as hfql]
            [hyperfiddle.navigator6.state :as state]
            [hyperfiddle.navigator6.rendering :refer [*server-pretty]]
            [hyperfiddle.navigator6.utils :as utils]
            [hyperfiddle.navigator6.generator :refer [generator yield]]
            [dustingetz.loader :refer [Offload]]
            [missionary.core :as m]
            [contrib.missionary-contrib :as mx]
            )
  #?(:cljs (:require-macros [hyperfiddle.navigator6.search :refer [with-local-search]]))
  )

(e/declare ^:dynamic *local-search)

(e/defn WithLocalSearch [state-key Body]
  (e/client
    (binding [*local-search (get state/*local-state state-key)]
      (binding [e/*bindings* (e/server (assoc e/*bindings* (find-var `*local-search) *local-search))]
        (->> (Body)
          (utils/InterceptCommand `SetSearch! (e/fn [[_ search]] [`state/UpdateState! assoc state-key search])))))))

(defmacro with-local-search [state-key & body]
  `(WithLocalSearch ~state-key (e/fn [] ~@body)))

(e/defn SearchInput! [disabled?]
  (e/client
    (->> (forms/Input! ::search *local-search :type :search :maxlength 100 :disabled disabled? :style {:grid-area "search"})
      (forms/Parse (e/fn [{search ::search}] [`SetSearch! search])))))



(e/defn Filtered
  [pred rows-count rowTs columnTs Body & {::keys [limit] :or {limit 10000}}]
  (e/client
    (let [filtered (e/server (e/Reconcile (cond
                                            (e/Reconcile (and (not (hfql/filtered? rowTs)) (< rows-count limit)))
                                            (Offload #(pred *server-pretty rowTs columnTs *local-search) {:label 'in-memory-search})
                                            :else
                                            (with-meta (take limit rowTs) (meta rowTs)))))]
      (Body filtered))))



(defmulti search-matches (fn [object _needle & [_pretty-print-map]] (type object)))
(defmethod search-matches :default [object needle & [pretty-print-map]]
  (if-let [pretty-print (get pretty-print-map (type object))]
    (strx/includes-str? (pretty-print object) needle)
    (strx/any-matches? [object] needle)))

(defn- prefix-filterer
  "Take a filtering predicate pred :: String -> Any -> Bool and return a function f :: String -> [Any] -> [Any].

  The returned function `f` will filter the given collection like regular
  clojure.core/filter. However, given we know the filter criteria is a String,
  we leverage the fact that if the previous search happened on a prefix of the
  current search (e.g. \"hell\" ⊏ \"hello\"), then we only filter the last result
  instead of the entire original resultset.
  E.g.:
   - (f \"\" xs)   : nothing to search on, return xs – 𝓞(1)
   - (f \"h\" xs)  : filter xs looking for \"h\" – 𝓞(n)
   - (f \"he\" xs) : \"h\"  ⊏ \"he\" ⇒ filter last result already filtered by \"h\" – 𝓞(∆n).
   - (f \"he\" xs) : \"he\" ⊑ \"he\" ⇒ return already filtered result – 𝓞(1).
   - (f \"h\" xs)  : \"h\"  ⋢ \"he\" ⇒ discard last result and filter xs looking for \"h\" – 𝓞(n).

  Therefore:
  - \"h\" ⟶ \"he\" ⟶ \"hel\" is 𝓞(n) ⟶ 𝓞(∆n) ⟶ 𝓞(∆∆n)
  - \"hel\" ⟶ \"he\" ⟶ \"h\" is 𝓞(n) ⟶ 𝓞(n) ⟶ 𝓞(n)

  "
  [pred]
  (let [!last-needle (volatile! "")
        !last-filtered (volatile! ())]
    (fn [needle pulled]
      (let [last-needle @!last-needle
            do-filter (partial pred needle)]
        (vreset! !last-needle needle)
        (cond
          (empty? needle) (vreset! !last-filtered pulled)
          (and (not (str/blank? last-needle))
            (str/starts-with? needle last-needle)) (vreset! !last-filtered (filter do-filter @!last-filtered))
          (and (not (str/blank? last-needle))
            (= needle last-needle)) @!last-filtered
          :else (vreset! !last-filtered (filter do-filter pulled)))))))

(defn run-tasks
  ([flow] (run-tasks 1 flow))
  ([par flow] (m/ap (m/? (m/?> par flow)))))

#?(:clj
   (defn resequence-by
     ([kf] (resequence-by kf (.. Runtime getRuntime availableProcessors)))
     ([kf par]
      (fn [rf]
        (let [^longs !head-pointer (long-array 1)
              init-cap (int (Math/ceil (/ (double (max 1 par)) 0.75)))
              ^java.util.HashMap buffer (java.util.HashMap. init-cap)]
          (aset !head-pointer 0 0)
          (fn
            ([] (rf))
            ([res] (rf res))
            ([res value]
             (let [current-head-pointer (aget !head-pointer 0)
                   current-index (long (kf value))]
               (if (== current-index current-head-pointer)
                 (loop [head-pointer (unchecked-inc current-index)
                        res (rf res value)]
                   (if-some [value (.remove buffer head-pointer)]
                     (recur (unchecked-inc head-pointer) (rf res value))
                     (do
                       (aset !head-pointer 0 head-pointer)
                       res)))
                 (do (.put buffer current-index value)
                     res))))))))))

#?(:clj (defn resequence [] (resequence-by #(nth % 0))))

#?(:clj
   (defn pfilter
     ([pred flow]
      (pfilter pred (.. Runtime getRuntime availableProcessors) 64 flow))
     ([pred par chunk-size flow]
      (->> flow
        (m/eduction (map-indexed vector)
          (partition-all chunk-size)
          (map (fn [chunk] (m/via m/blk (mapv (fn [[idx x]] [idx (boolean (pred x)) x]) chunk)))))
        (run-tasks par)
        (m/eduction cat (resequence) (filter #(nth % 1)) (map #(nth % 2)))))))

#?(:clj
   (defn eager-search-BIG-O-N-SQUARE< [pretty-print-map rowTs columnTs needle]
     (pfilter
       (fn [rowT]
         (when-not (.isInterrupted (Thread/currentThread))
           (let [rowT+ (hfql/augment-with-suggestions! rowT)
                 cellTs (unwrap rowT+)]
             (loop [columnTs columnTs]
               (if (seq columnTs)
                 (if-let [cellT (hfql/find-cell-by-column* (first columnTs) cellTs)]
                   (when-not (.isInterrupted (Thread/currentThread))
                     (if (search-matches (hfql/nav cellT) needle pretty-print-map)
                       true
                       (recur (rest columnTs))))
                   (recur (rest columnTs)))
                 false)))))
       (m/seed rowTs))))

#?(:clj
   (defn eager-search-BIG-O-N-SQUARE []
     (fn [pretty-print-map rowTs columnTs needle]
       (cond
         (str/blank? needle) rowTs
         (empty? columnTs) ()
         :else
         (m/? (m/reduce conj [] (eager-search-BIG-O-N-SQUARE< pretty-print-map rowTs columnTs needle)))))))


#?(:clj
   (defn eager-search-BIG-O-N-SQUARE-streamy []
     (fn [pretty-print-map rowTs columnTs needle]
       (m/relieve {}
         (mx/throttle 100
           (m/reductions {} [0 ()]
             (cond
               (str/blank? needle) (m/ap [(bounded-count 10000 rowTs) (with-meta (take 10000 rowTs) (meta rowTs))])
               (empty? columnTs) (m/ap [0 ()])
               :else
               (->> (eager-search-BIG-O-N-SQUARE< pretty-print-map rowTs columnTs needle)
                 (m/eduction (take 10000))
                 (m/reductions (fn [[num-rows rows] row] [(inc num-rows) (conj rows row)]) [0 []])))))))))


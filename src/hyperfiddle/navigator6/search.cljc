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
   (defn resequence []
     (fn [rf]
       (let [buffer (java.util.HashMap.)
             !next (volatile! 0)]
         (fn
           ([] (rf))
           ([res] (rf res))
           ([res [i pass? x]]
            (.put buffer i [pass? x])
            (loop []
              (if-some [[pass? x] (.remove buffer @!next)]
                (do
                  (when pass? (rf res x))
                  (vswap! !next inc)
                  (recur))
                res))))))))

#?(:clj
   (defn pfilter
     ([pred flow]
      (pfilter pred (.. Runtime getRuntime availableProcessors) flow))
     ([pred par flow]
      (->> flow
        (m/eduction (map-indexed (fn [i x] (m/via m/blk [i (boolean (pred x)) x]))))
        (run-tasks par)
        (m/eduction (resequence))))))

#?(:clj
   (defn eager-search-BIG-O-N-SQUARE-2 []
     (fn [pretty-print-map rowTs columnTs needle]
       (m/? (m/reduce conj [] (pfilter
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
                                (m/seed rowTs)))))))

(comment
  #?(:clj
     (defn eager-search-BIG-O-N-SQUARE []
       ;; TODO move to parallel search using missionary
       (fn [pretty-print-map rowTs columnTs needle]
         (cond
           (str/blank? needle) rowTs
           (empty? columnTs) ()
           :else
           (try
             (doall
               (generator
                 (loop [[rowT & rowTs] rowTs]
                   (when rowT
                     (when-not (.isInterrupted (Thread/currentThread))
                       (let [rowT+ (hfql/augment-with-suggestions! rowT)
                             cellTs (unwrap rowT+)]
                         (loop [[columnT & columnTs] columnTs]
                           (when columnT
                             (if-let [cellT (hfql/find-cell-by-column* columnT cellTs)]
                               (when-not (.isInterrupted (Thread/currentThread))
                                 (let [cell-value (hfql/nav cellT)]
                                   (if (search-matches cell-value needle pretty-print-map)
                                     (yield rowT)
                                     (recur columnTs))))
                               (recur columnTs)))))
                       (recur rowTs))))))
             (catch Throwable t
               (prn 'search-BIG-O-N-SQUARE (ex-message t) #_t))))))))

;; #?(:clj
;;    (defn eager-search-kvs-BIG-O-N-SQUARE []
;;      (let [_do-filter (prefix-filterer (fn [needle pair] (search-matches (snd pair) needle)))]
;;        (fn [columnsT rowsT _hfql-bindings search _sort-spec]
;;          (try
;;            (debug-exceptions `eager-search-kvs-BIG-O-N-SQUARE
;;              (if (not-empty search)
;;                (let [kvs (for [rowT rowsT
;;                                columnT columnsT
;;                                :let [cell-value (delay (hfql/nav (hfql/seed (hfql/scope rowT) columnT)))]
;;                                :when (or (search-matches (hfql/symbolic-edge columnT) search)
;;                                        (search-matches (force cell-value) search))]
;;                            [rowT columnT])]
;;                  [(distinct (map second kvs))
;;                   (dedupe (map first kvs))])
;;                [columnsT rowsT]))
;;            (catch Throwable t (prn 'eager-pull-search-sort (ex-message t) #_t)
;;                   [columnsT rowsT]))))))

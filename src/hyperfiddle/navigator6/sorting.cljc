(ns hyperfiddle.navigator6.sorting
  (:require #?(:clj [clojure.tools.logging :as log])
            [dustingetz.data]
            [geoffreygaillard.debug-tools :refer [debug-exceptions]]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-forms5 :as forms]
            [hyperfiddle.ct.functor :refer [fmap]]
            [hyperfiddle.ct.bifunctor :refer [bimap]]
            [hyperfiddle.ct.cofree :refer [unwrap]]
            [hyperfiddle.ct.comonad :refer [extract]]
            [hyperfiddle.ct.monad :refer [join]]
            [hyperfiddle.ct.monoid :refer [mappend]]
            [hyperfiddle.ct.either :refer [from-right]]
            [hyperfiddle.hfql2 :as hfql]
            [hyperfiddle.ct.experiments.thunk :as thunks]
            [hyperfiddle.rcf :refer [tests]]
            [dustingetz.loader :refer [Offload]]
            [missionary.core :as m]
            )
  #?(:cljs (:require-macros [hyperfiddle.navigator6.sorting :refer [with-local-sort]])))

(defn- compare-nils-last "Wrap a comparator such that nils are always sorted last."
  ([comparator] (partial compare-nils-last comparator))
  ([comparator a b]
   (cond
     (and (nil? a) (nil? b)) 0
     (nil? a) 1    ; nil after non-nil
     (nil? b) -1   ; non-nil before nil
     :else (comparator a b))))

(defn- force-cell-value! "Equivalent to hfql/nav, but log a TRACE-level message if the cell wasn't realized already. Because we want to know if sorting causes unexpected IO."
  [cellT]
  (when cellT
    (let [thunk (extract cellT)]
      (when-not (realized? thunk)
        #?(:clj (log/trace "Running IO for sort over " (hfql/symbolic-edge cellT))))
      (thunks/force-thunk thunk) ; TODO would be nice if force-thunk was retrocompatible with Delay
      )))

(def ^:private compare-cells "Take a comparator and transform it such that it receives comparable cell values instead of raw CofreeT objects."
  ((join bimap) (fmap hfql/comparable force-cell-value!)))

(defn- compare-fallback-to-identity
  "Take a comparator and transform it such that if comparison throws, comparison
  is tried again onto objects identities. If compared objects do not implement
  Identifiable, the thrown error is propagated."
  [comparator]
  (fn [a b]
    (try
      (comparator a b)
      (catch #?(:clj Throwable, :cljs :default) ex
        (if (and (hfql/identifiable? a) (hfql/identifiable? b))
          (comparator (hfql/identify a) (hfql/identify b))
          (throw ex))))))

(defn- directional-comparator
  "Take '. , '< or '> and return a comparator comparing any two values in resp. natural order, ascending or descending order.
  Note < and > functions (not quoted) only work on numbers and so can only be used as a comparator for numbers.
  The comparator returned by the current function sorts based on the sign of the return value of `compare`."
  [order]
  {:pre [(or (= '. order) (= '< order) (= '> order))]}
  (case order
    . (constantly 0)
    < compare
    > (fn [a b] (compare b a))))

(comment
  (map hfql/pull (sort-by (hfql/find-cell-by-column _ageT) (compare-cells (compare-nils-last (directional-comparator '>))) _rowsT))
  )

(comment ; scratch test from navigator4, ->sort-comparator was replaced by sort-rows-by-columns
  (def _unwrap unwrap)
  (def _treeT (hfql/seed {'% [{:name "Alice", :age 30}
                              {:name "Bob", :age 32}
                              {:name "Charlie", :age 31}
                              {:name "Charlie2", :age 31}
                              {:name "John", :age nil}]}
                (hfql/hfql {* [:name :age]})))
  (hfql/pull _treeT)
  (def _rowsT (-> _treeT _unwrap))
  (def _ageT (-> _rowsT first _unwrap second))
  (extract (extract _ageT))
  )

(defn- normalize-comparable-nil
  "Either-wrapped nils become [\"0:right\" nil] after `comparable` — a non-nil
  vector that hides the nil from `compare-nils-last`. Detect nil in the value
  slot and replace the whole vector with bare nil."
  [comparator]
  (let [nil-val? (fn [x] (and (vector? x) (nil? (peek x))))]
    (fn [a b]
      (comparator
        (if (nil-val? a) nil a)
        (if (nil-val? b) nil b)))))

(defn- interruptible-comparator [cmp]
  #?(:cljs cmp
     :clj (fn [a b] (m/!) (cmp a b))))

(def COLLECTION-LIMIT 10000)

(defn sort-rows-by-columns "TODO support multi-column sort." [columnT-specs rowsT]
  ;; (def _args [columnT-specs rowsT])
  (cond
    (>= (bounded-count COLLECTION-LIMIT rowsT) COLLECTION-LIMIT) rowsT
    :else
    (try
      (let [[columnT order] (first columnT-specs)] ; TODO support multi-column sort
        (case order
          nil rowsT ; no sort, or default sort, bypass.
          .   rowsT ; natural order, bypass as an optimization: avoid worst case of sort time complexity: an already sorted collection.
          (sort-by (hfql/find-cell-by-column columnT)
            (interruptible-comparator
              (compare-cells
                (compare-fallback-to-identity
                  (normalize-comparable-nil
                    (compare-nils-last
                      (directional-comparator order))))))
            (map hfql/augment-with-suggestions! rowsT)))) ; TODO only augment with sorted column(s), if not already present
      #?(:clj (catch missionary.Cancelled ex (throw ex)))
      (catch #?(:clj Throwable, :cljs :default) ex
        ;; Keep fallback-to-unsorted — better to show unordered data than crash.
        ;; Logging (not println) so it's filterable and visible in prod.
        #?(:clj  (log/warn "Failed to sort, returning unsorted" (ex-message ex))
           :cljs (.warn js/console "Failed to sort, returning unsorted" (ex-message ex)))
        rowsT))))


(comment
  (require '[clj-async-profiler.core :as prof])
  (prof/serve-ui 8082)
  (time
    (do (apply sort-rows-by-columns _args) nil))
)

(e/defn DefaultSort [rowsT] [[(first (unwrap (first rowsT))) '<]])

(e/declare ^:dynamic *disabled?*)
(e/declare ^:dynamic *sort-spec*)

(e/defn Sorted
  "Pure sort transform — reads *sort-spec* from with-local-sort binding,
   *disabled?* is bound at Navigate* level."
  [rowsT Body]
  (let [sort-spec *sort-spec*]
    (Body (e/server (Offload #(sort-rows-by-columns sort-spec rowsT) {:label 'sort})))))

(e/defn WithLocalSort
  "Manages sort state and intercepts Sort! commands. Sort spec uses a local
   atom (not route state) because it contains HFQL tree nodes (non-serializable)."
  [Body]
  (let [!sort-spec (e/server (atom []))
        sort-spec (e/server (e/watch !sort-spec))]
    (binding [*sort-spec* sort-spec]
      (binding [e/*bindings* (e/server (assoc e/*bindings* (find-var `*sort-spec*) *sort-spec*))]
        (->> (Body)
          (forms/Interpreter {`Sort! (e/fn [Getter direction]
                                       ;; HACK must touch both args on server, otherwise the `case` do not run.
                                       (e/server ((constantly nil) direction Getter))
                                       (case (e/server (reset! !sort-spec [[(Getter) direction]]) nil)
                                         [::forms/ok]))}))))))

(defmacro with-local-sort [& body]
  `(WithLocalSort (e/fn [] ~@body)))

(defn- -fsym [sexpr]
  (when (seq? sexpr)
    (if (= 'quote (first sexpr))
      (-fsym (second sexpr))
      (first sexpr))))

(defn- sexpr-comparator [a b]
  (debug-exceptions `sexpr-comparator
    (cond
      (nil? a) -1
      (nil? b) 1
      (and (keyword? a) (symbol? b)) (compare a (keyword b))
      (and (symbol? a) (keyword? b)) (compare (keyword a) b)
      (seq? a) (sexpr-comparator (-fsym a) b)
      (seq? b) (sexpr-comparator a (-fsym b))
      :else (compare a b))))

(defn- ?sort-by [keyfn v*]
  (try (debug-exceptions `?sort-by
         (sort-by keyfn sexpr-comparator v*))
       (catch #?(:clj Throwable :cljs :default) e
         (let [log-message (str "Skipped sort: some elements are not comparable." (type e) " " (ex-message e))]
           #?(:clj (log/info log-message)
              :cljs (.info js/console log-message)))
         v*)))


#?(:clj
   (defn- sort-children-by-ordered-keyset
     "Sort a map structure by keys. Static selected keys (from stepT) have
  priority and are laid out in order, as in an ordered map. Extra keys comes later and are sorted by
  `comparator`. Return a seq of map entries."
     [stepT childrenT]
     {:pre [(hfql/pull? stepT) (every? hfql/symbolic-edge childrenT)]
      :post [(every? hfql/symbolic-edge childrenT) #_(= (count %) (count childrenT))]}
     (let [static-keys (distinct (map hfql/symbolic-edge childrenT))
           static-keyset (set static-keys)
           [static-children dynamic-children] (dustingetz.data/group-by-pred (comp boolean static-keyset hfql/symbolic-edge) childrenT)
           static-map (into {} (map (juxt hfql/symbolic-edge identity)) static-children)]
       #_(prn {'static-keys static-keys
             'static-kvs static-children
             'dynamic-kvs dynamic-children})
       (mappend (keep (fn [pullspec-key] (get static-map pullspec-key)) static-keys)
         (?sort-by hfql/symbolic-edge (remove (comp some? static-keyset hfql/symbolic-edge) dynamic-children))))))

(defn- toggle-column-sort [sort-spec for-col]
  (when-some [[col asc?] (first sort-spec)]
    [[for-col (if (= col for-col) (not asc?) true)]]))



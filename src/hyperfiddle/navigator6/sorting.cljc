(ns hyperfiddle.navigator6.sorting
  (:require #?(:clj [clojure.tools.logging :as log])
            [contrib.data]
            [geoffreygaillard.debug-tools :refer [debug-exceptions]]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-forms5 :as forms]
            [hyperfiddle.ct.functor :refer [fmap]]
            [hyperfiddle.ct.bifunctor :refer [bimap]]
            [hyperfiddle.ct.cofree :refer [unwrap]]
            [hyperfiddle.ct.comonad :refer [extract]]
            [hyperfiddle.ct.monad :refer [join]]
            [hyperfiddle.ct.monoid :refer [mappend]]
            [hyperfiddle.hfql2.analyzer :as ana]
            [hyperfiddle.hfql2 :as hfql]
            [hyperfiddle.ct.experiments.thunk :as thunks]
            [hyperfiddle.rcf :refer [tests]]
            [dustingetz.loader :refer [Offload]]
            [missionary.core :as m]
            ))

(defmulti comparable type)
(defmethod comparable :default [x] x)
#?(:clj (defmethod comparable java.lang.Class [^Class x] (.getName x)))
(defmethod comparable #?(:clj clojure.lang.Symbol :cljs cljs.core.Symbol) [sym] (str sym))
(defmethod comparable #?(:clj clojure.lang.Keyword :cljs cljs.core.Keyword) [kw] (str (namespace kw) "/" (name kw))) ; drop ":"

(defn- compare-nils-last "Wrap a comparator such that nils are always sorted last."
  ([comparator] (partial compare-nils-last comparator))
  ([comparator a b]
   (cond
     (nil? a) false
     (nil? b) true
     :else (comparator a b))))

(defn- force-cell-value! "Equivalent to hfql/nav, but log a TRACE-level message if the cell wasn't realized already. Because we want to know if sorting causes unexpected IO."
  [cellT]
  (when cellT
    (let [thunk (extract cellT)]
      (when-not (realized? thunk)
        #?(:clj (log/trace "Running IO for sort over " (hfql/symbolic-edge cellT))))
      (thunks/force-thunk thunk) ; TODO would be nice if force-thunk was retrocompatible with Delay
      )))

(def compare-cells "Take a comparator and transform it such that it receives comparable cell values instead of raw CofreeT objects."
  ((join bimap) (fmap comparable force-cell-value!)))

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

(defn directional-comparator
  "Take '< or '> and return a comparator comparing any two values in resp. ascending or descending order.
  Note < and > functions (not quoted) only work on numbers and so can only be used as a comparator for numbers.
  The comparator returned by the current function sorts based on the sign of the return value of `compare`."
  [direction]
  {:pre [(or (= '< direction) (= '> direction))]}
  (fmap (case direction < neg? > pos?) compare))

(comment
  (map hfql/pull (sort-by (hfql/find-cell-by-column _ageT) (compare-cells (compare-nils-last (directional-comparator '>))) _rowsT))
  )

(tests
  (println)
  (def _unwrap unwrap)
  (def _treeT (hfql/seed {'% [{:name "Alice", :age 30}
                              {:name "Bob", :age 32}
                              {:name "Charlie", :age 31}
                              {:name "Charlie2", :age 31}
                              {:name "John", :age nil}]}
                (hfql/hfql {* [:name :age]})))
  (hfql/pull _treeT)
  ;; (map hfql/nav (unwrap _treeT))
  (println)
  (def _rowsT (-> _treeT _unwrap))
  (def _ageT (-> _rowsT first _unwrap second))
  (extract (extract _ageT))
  (println)
  (def _sorted-rowsT-asc (sort (->sort-comparator [[_ageT true]]) _rowsT))
  (extract (extract (second (_unwrap (first _sorted-rowsT-asc)))))
  (map hfql/values _sorted-rowsT-asc)
  := '({:name "Alice", :age 30} {:name "Charlie", :age 31} {:name "Bob", :age 32})

  #_(def _sorted-rowsT-desc (sort (->sort-comparator [[_ageT false]]) _rowsT))
  #_(map hfql/nav _sorted-rowsT-desc)
  #_#_:= '({:name "Bob", :age 32} {:name "Charlie", :age 31} {:name "Alice", :age 30})

  )

(defn interruptible-comparator [cmp]
  #?(:cljs cmp
     :clj (fn [a b] (m/!) (cmp a b))))

(def COLLECTION-LIMIT 10000)

(defn sort-rows-by-columns "TODO support multi-column sort." [columnT-specs rowsT]
  (if (>= (bounded-count COLLECTION-LIMIT rowsT) COLLECTION-LIMIT)
    rowsT
    (try
      (let [[columnT direction] (first columnT-specs)] ; TODO support multi-column sort
        (if (nil? direction)
          rowsT ; no sort, or default sort, bypass.
          (sort-by (hfql/find-cell-by-column columnT) (interruptible-comparator (compare-cells (compare-fallback-to-identity (compare-nils-last (directional-comparator direction)))))
            (map hfql/augment-with-suggestions! rowsT)))) ; TODO only augment with sorted column(s), if not already present
      #?(:clj (catch missionary.Cancelled ex (throw ex)))
      (catch #?(:clj Throwable, :cljs :default) ex
        (println " Failed to sort" (ex-message ex) #_ex)
        rowsT))))

(e/defn DefaultSort [rowsT] [[(first (unwrap (first rowsT))) '<]])

(e/defn Sorted [rowsT Body]
  (e/client
    (let [!sort-spec (e/server (atom [])) ; (e/Snapshot (DefaultSort rowsT)) ; will eagerly compute all cells in first column
          sort-spec (e/server (e/watch !sort-spec))]
      #_(e/server (prn "sort-spec" sort-spec) nil)
      (->> (Body (e/server (Offload #(sort-rows-by-columns sort-spec rowsT) {:label 'sort})))
        (forms/Interpreter {`Sort! (e/fn [Getter direction]
                                     (e/server ((constantly nil) direction Getter)) ; HACK must touch both args on server, otherwise the `case` do not run.
                                     (case (e/server (reset! !sort-spec [[(Getter) direction]]) nil)
                                       [::forms/ok]))})))))

(defn -fsym [sexpr]
  (when (seq? sexpr)
    (if (= 'quote (first sexpr))
      (-fsym (second sexpr))
      (first sexpr))))

(defn sexpr-comparator [a b]
  (debug-exceptions `sexpr-comparator
    (cond
      (nil? a) -1
      (nil? b) 1
      (and (keyword? a) (symbol? b)) (compare a (keyword b))
      (and (symbol? a) (keyword? b)) (compare (keyword a) b)
      (seq? a) (sexpr-comparator (-fsym a) b)
      (seq? b) (sexpr-comparator a (-fsym b))
      :else (compare a b))))

(defn ?sort-by [keyfn v*]
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
     {:pre [(= ::ana/pull (::ana/type (hfql/metas stepT))) (every? hfql/symbolic-edge childrenT)]
      :post [(every? hfql/symbolic-edge childrenT) #_(= (count %) (count childrenT))]}
     (let [static-keys (distinct (map hfql/symbolic-edge childrenT))
           static-keyset (set static-keys)
           [static-children dynamic-children] (contrib.data/group-by-pred (comp boolean static-keyset hfql/symbolic-edge) childrenT)
           static-map (into {} (map (juxt hfql/symbolic-edge identity)) static-children)]
       #_(prn {'static-keys static-keys
             'static-kvs static-children
             'dynamic-kvs dynamic-children})
       (mappend (keep (fn [pullspec-key] (get static-map pullspec-key)) static-keys)
         (?sort-by hfql/symbolic-edge (remove (comp some? static-keyset hfql/symbolic-edge) dynamic-children))))))

(defn toggle-column-sort [sort-spec for-col]
  (when-some [[col asc?] (first sort-spec)]
    [[for-col (if (= col for-col) (not asc?) true)]]))



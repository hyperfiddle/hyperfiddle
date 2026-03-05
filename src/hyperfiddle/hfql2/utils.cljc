(ns hyperfiddle.hfql2.utils
  (:require
   [hyperfiddle.rcf :refer [tests]]))

(defn has-meta? [object]
  #?(:clj (instance? clojure.lang.IObj object)
     :cljs (satisfies? IMeta object)))

(defn- ordered-map
  "A simple, naive ordered map impl preserving order of keys given at creation
  time, and behaving as a sorted-map on keys assoced after creation.
  Performances aside, one can always re-build the map to freeze the keyset
  order."
  [& keyvals]
  (let [order (into {} (comp (take-nth 2) (map-indexed #(vector %2 [%1 %2])))
                keyvals)
        max-static-key (count order)
        position #(get order % [max-static-key %])]
    (into (sorted-map-by #(compare (position %1) (position %2)))
      (map vec) ; odd but required: partition returns pairs as lazy seqs and `conj` on a map expects vector-pairs.
      (partition 2 keyvals))))

(tests
  (keys (conj (apply ordered-map
                (interleave
                  (map str "abcdefghijklmnop")
                  (range 9) ; larger than clojure.lang.PersistentArrayMap/HASHTABLE_THRESHOLD
                  ))
         [:z 'whatever]
         [:y 'whatever]))
   := '("a" "b" "c" "d" "e" "f" "g" "h" "i" :y :z)
  )

(defn- distinct-by
  "Returns a lazy sequence of the elements of coll with duplicates removed by key function kf.
  Returns a stateful transducer when no collection is provided.
  (distinct-by identity) ≣ (clojure.core/distinct)."
  ([kf]
   (fn [rf]
     (let [seen (volatile! #{})]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [key (kf input)]
            (if (contains? @seen key)
              result
              (do (vswap! seen conj key)
                  (rf result input)))))))))
  ([kf coll]
   (sequence (distinct-by kf) coll)))

(tests
  (distinct-by identity [1 2 1 2 3]) := '(1 2 3)
  (distinct-by :key [{:key 1}, {:key 2}, {:key 1} {:key 3}]) := '({:key 1} {:key 2} {:key 3})
  )

(defn- shift-right
  "A transducer cons-ing a value onto the head of an eduction. Use it to shift an eduction right by one."
  [value]
  (fn [rf]
    (let [!first? (volatile! true)]
      (fn
        ([] (rf))
        ([res] (rf res))
        ([res x]
         (if (deref !first?)
           (do (vswap! !first? (constantly false))
               (-> res (rf (force value)) (rf x)))
           (rf res x)))))))

(tests
  (into [] (shift-right nil) [1 2 3])
   := [nil 1 2 3]
  )

(defn imap? [x]
  #?(:clj (instance? java.util.Map x)
     :cljs (map? x) ; checks for cljs.core/IMap proto
     ))

(defn original-object "Return the value associated with `:hyperfiddle.hfql2.protocols/obj` in obj's metadata, if any, or return obj." [obj]
  (if (has-meta? obj)
    (:hyperfiddle.hfql2.protocols/obj (meta obj) obj)
    obj))
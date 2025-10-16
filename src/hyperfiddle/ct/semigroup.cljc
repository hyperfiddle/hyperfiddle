(ns hyperfiddle.ct.semigroup)

(defprotocol Semigroup
  (-<> [a b] "<> :: a -> a -> a. aka binop – a binary operation."))

(defn <> [a b] (-<> a b))

(extend-protocol Semigroup
  ;; String
  #?(:clj java.lang.String, :cljs string)
  (-<> [a b] (str a b))

  ;; List
  #?(:clj clojure.lang.ISeq, :cljs ISeq)
  (-<> [a b] (concat a b))

  #?(:clj clojure.lang.PersistentList$EmptyList :cljs EmptyList)
  (-<> [_a b] b)

  #?(:clj clojure.lang.Cons :cljs Cons)
  (-<> [a b] (concat a b) #_(cons (a->b (first fa)) (fmap a->b (next fa))))

  ;; Vector
  #?(:clj clojure.lang.IPersistentVector, :cljs PersistentVector)
  (-<> [a b] (into a b))

  ;; Map
  #?(:clj clojure.lang.IPersistentMap :cljs IMap)
  (-<> [a b] (merge a b))

  ;; Set
  #?(:clj clojure.lang.IPersistentSet :cljs ISet)
  (-<> [a b] (into a b))

  ;; MapEntry
  #?(:clj clojure.lang.AMapEntry :cljs MapEntry)
  (-<> [a b]
    (assert (satisfies? Semigroup (key a)))
    (assert (satisfies? Semigroup (val a)))
    (assert (satisfies? Semigroup (key b)))
    (assert (satisfies? Semigroup (val b)))
    (first {(-<> (key a) (key b)) (-<> (val a) (val b))}))

  #?(:clj clojure.lang.Delay, :cljs Delay)
  (-<> [a b] (delay (-<> (force a) (force b))))


  ;; TODO
  ;; Sum where <> is +
  ;; Product where <> is *

  )

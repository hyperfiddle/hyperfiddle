(ns hyperfiddle.hfql1.monoid
  (:require [hyperfiddle.hfql1.semigroup :refer [<>]]
            [hyperfiddle.rcf :refer [tests]]))

(defprotocol Monoid
  (mempty [a] "mempty :: a"))

(defprotocol MonoidConcat
  "Optional: implement if you want to overload mconcat"
  (-mconcat [_ as] "mconcat :: [a] -> a. Fold a list using the monoid. First argument is the `first` of the collection `as`, used for dispatch and is ignored."))

(defn mappend "mappend :: a -> a -> a" 
  ([a] (partial mappend a))
  ([a b] (<> a b)))

(defn mconcat "mconcat :: [a] -> a. Fold a list using the monoid." [as]
  (if (satisfies? MonoidConcat (first as))
    (-mconcat (first as) as)
    (reduce mappend (mempty (first as)) as)))

(extend-protocol Monoid

  ;; String
  #?(:clj java.lang.String, :cljs string)
  (mempty [a] "")

  ;; List
  #?(:clj clojure.lang.ISeq, :cljs ISeq)
  (mempty [a] (empty a))

  #?(:clj clojure.lang.PersistentList$EmptyList :cljs EmptyList)
  (mempty [a] a)

  #?(:clj clojure.lang.Cons :cljs Cons)
  (mempty [a] (empty a))

  ;; Vector
  #?(:clj clojure.lang.IPersistentVector, :cljs PersistentVector)
  (mempty [a] (empty a))

  ;; Map
  #?(:clj clojure.lang.IPersistentMap :cljs IMap)
  (mempty [a] (empty a))

  ;; Set
  #?(:clj clojure.lang.IPersistentSet :cljs ISet)
  (mempty [a] (empty a))

  ;; Pair
  #?(:clj clojure.lang.AMapEntry :cljs MapEntry)
  (mempty [a]
    (assert (satisfies? Monoid (key a)))
    (assert (satisfies? Monoid (val a)))
    (first {(mempty (key a)) (mempty (val a))}))

  #?(:clj clojure.lang.Delay, :cljs Delay)
  (mempty [a] (delay (mempty (force a))))

  ;; TODO
  ;; Function
  ;; Sum where mempty is 0
  ;; Product where mempty is 1

  )

(extend-protocol MonoidConcat
  ;; Efficient string concat
  #?(:clj java.lang.String, :cljs string)
  (-mconcat [_ as] (clojure.string/join as)))


;;; laws

(tests
  (mappend '(1 2) (mappend '(3 4) '(5 6)))
  := (mappend (mappend '(1 2) '(3 4)) '(5 6))
  )

;;; ----

(tests
  (mappend "" "")
  := ""
  (mappend "hello" "world")
  := "helloworld"

  (mappend () ())
  := ()
  (mappend () '(1 2))
  := '(1 2)

  (mappend '(1 2) ())
  := '(1 2)

  (mappend [] [])
  := []

  (mappend [] [1])
  := [1]

  (mappend [1] [])
  := [1]

  (mappend [1 2] [3 4])
  := [1 2 3 4]

  (mappend (range 3) (range 3 6))
  := '(0 1 2 3 4 5)

  (mappend #{1 2} #{2 3})
  := #{1 3 2}

  (mappend (first {:a 1}) (first {:b 2})) :throws #?(:clj AssertionError, :cljs js/Error)
  (mappend (first {[1] ["a"]}) (first {[2] ["b"]}))
  := [[1 2] ["a" "b"]])

(tests
  (mconcat ["hello" " " "world" "!"])
  := "hello world!"

  (mconcat [[1] [2]])
  := [1 2])
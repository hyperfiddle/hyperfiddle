(ns hyperfiddle.ct.pair
  "A pair is a tuple. A Clojure MapEntry is a valid pair. Vectors are not pairs,
  even of size 2. This namespace:
  - defines the stock MapEntry type as a pair,
  - provides a `(pair a b)` constructor, producing a pair supporting metadatas¹
  and, in practice², fully retrocompatible and interchangeable with MapEntry in
  both Clojure and ClojureScript.

  Pairs serialize to EDN as vectors, just like MapEntry.

  ¹ In the context of building DSLs, a pair type like MapEntry is useful to
  model explicit associations. Modeling associations as single-entry maps or
  size-2 vectors is brittle. The stock MapEntry type however, is an
  almost-perfect fit. It defines an explicit association between a `key` slot
  and a `value` slot. It also looks like, behaves, prints and quacks like a
  vector of two values. But contrary to size-2 vectors and single-entry maps,
  the MapEntry type doesn't support metadata. This design decision makes sense
  for the use case it has been designed for. Unfortunately, treating size-2
  vectors as pairs quickly ends up impractical, as one must excerpt extreme
  precaution to always keep the ability to distinguish a genuine size-2 vector
  from a MapEntry contender.

  ² Pair and MapEntry are fully interchangeable in Clojure. In ClojureScript
  regular users can consider them interchangeable too. Compiler developers, e.g.
  people interacting with the cljs compiler or core library must be aware that
  the MapEntry type may be hardcoded. Therefore, one can use a Pair anywhere one
  could use a MapEntry, except when interacting with ClojureScript internals.
  E.g. injecting a Pair into a PersistentHashMap using a low-level API, if it
  exists at all.

  Examples:
  (map-entry? (first {:a 1})) := true
  (pair? (first {:a 1})) := true
  (= (pair :a 1) (first {:a 1})) := true
  (= (first {:a 1}) (pair :a 1)) := true ; commutativity is respected – even with .equals.

  (pr-str (first {:a 1})) := \"[:a 1]\"
  (pr-str (pair :a 1)) := \"[:a 1]\"
  "
  (:require [hyperfiddle.ct.functor :refer [Functor]]
            [hyperfiddle.ct.bifunctor :refer [Bifunctor]]
            [hyperfiddle.rcf :refer [tests]]))

(defprotocol IPair
  (fst [_] "fst :: (a, b) -> a.  Extract the first component of a pair.")
  (snd [_] "snd :: (a, c) -> b. Extract the second component of a pair."))

(extend-type #?(:clj clojure.lang.AMapEntry :cljs MapEntry)
  IPair
  (fst [entry] (key entry))
  (snd [entry] (val entry)))

#?(:cljs ; cljs only, clojure uses proxy
   ; Copied and adapted from cljs.core
   (deftype Pair [key val meta ^:mutable __hash]
     IPair
     (fst [_] key)
     (snd [_] val)

     IMeta
     (-meta [_] meta)
     IWithMeta
     (-with-meta [_ m] (Pair. key val m nil))

     Functor
     (-fmap [_ f] (Pair. key (f val) meta nil))

     Bifunctor
     (-bimap [_ f g] (Pair. (f key) (g val) meta nil))

     ;; Rest is copied over from cljs.core.MapEntry
     Object
     (indexOf [coll x]
       (-indexOf coll x 0))
     (indexOf [coll x start]
       (-indexOf coll x start))
     (lastIndexOf [coll x]
       (-lastIndexOf coll x (count coll)))
     (lastIndexOf [coll x start]
       (-lastIndexOf coll x start))

     IMapEntry
     (-key [node] key)
     (-val [node] val)

     IHash
     (-hash [coll] (caching-hash coll hash-ordered-coll __hash))

     IEquiv
     (-equiv [coll other] (equiv-sequential coll other))

     IStack
     (-peek [node] val)

     (-pop [node] [key])

     ICollection
     (-conj [node o] [key val o])

     IEmptyableCollection
     (-empty [node] nil)

     ISequential
     ISeqable
     (-seq [node] (IndexedSeq. #js [key val] 0 nil))

     IReversible
     (-rseq [node] (IndexedSeq. #js [val key] 0 nil))

     ICounted
     (-count [node] 2)

     IIndexed
     (-nth [node n]
       (cond (== n 0) key
             (== n 1) val
             :else    (throw (js/Error. "Index out of bounds"))))

     (-nth [node n not-found]
       (cond (== n 0) key
             (== n 1) val
             :else    not-found))

     ILookup
     (-lookup [node k] (-nth node k nil))
     (-lookup [node k not-found] (-nth node k not-found))

     IAssociative
     (-assoc [node k v]
       (assoc [key val] k v))
     (-contains-key? [node k]
       (or (== k 0) (== k 1)))

     IFind
     (-find [node k]
       (case k
         0 (MapEntry. 0 key nil)
         1 (MapEntry. 1 val nil)
         nil))

     IVector
     (-assoc-n [node n v]
       (-assoc-n [key val] n v))

     IReduce
     (-reduce [node f]
       (ci-reduce node f))

     (-reduce [node f start]
       (ci-reduce node f start))

     IFn
     (-invoke [node k]
       (-nth node k))

     (-invoke [node k not-found]
       (-nth node k not-found))))

#?(:cljs (extend-protocol IPrintWithWriter ; copied from cljs.core
           Pair
           (-pr-writer [coll writer opts] (pr-sequential-writer writer pr-writer "[" " " "]" opts coll))))

#?(:cljs (extend-protocol IComparable ; copied from cljs.core
           Pair
           (-compare [x y]
             (if (vector? y)
               (compare-indexed x y)
               (throw (js/Error. (str "Cannot compare " x " to " y)))))))

#?(:clj (defn pair* [a b metas]
          (proxy [clojure.lang.MapEntry clojure.lang.IMeta clojure.lang.IObj] [a b]
            (meta [] metas)
            (withMeta [m] (pair* a b m))))
   :cljs (defn pair* [a b metas]
           (->Pair a b metas nil)))

(defn pair [a b] (pair* a b nil))
(defn pair? [object] (map-entry? object))

(defn- has-meta? [object] #?(:clj (instance? clojure.lang.IObj object) :cljs (satisfies? IMeta object)))
(defn to-pair [object]
  (let [p (pair (first object) (second object))]
    (if (has-meta? object)
      (with-meta p (meta object))
      p)))

(defn- map-entry [a b] (first {a b}))

#?(:clj
   (tests
     ".equals with MapEntry" ; shows this is proper equality. No hack!
     ;; These two tests are critical. They are the cornerstone of the feature. They must always pass.
     ;; If they don't the entire feature becomes unpractical and becomes trash.
     (.equals (pair 1 2) (map-entry 1 2)) := true
     (.equals (map-entry 1 2) (pair 1 2)) := true
     )
   :cljs nil ; no need for a cljs check, js doesn't have `.equals` and we'll check with `=`
   )

(tests ; clj and cljs
  "Equality with MapEntry"
  (= (pair 1 2) (map-entry 1 2)) := true
  (= (map-entry 1 2) (pair 1 2)) := true
  )

(tests
  "Uniform API with MapEntry"
  (fst (pair 1 2)) := 1
  (snd (pair 1 2)) := 2
  (fst (map-entry 1 2)) := 1
  (snd (map-entry 1 2)) := 2

  (key (pair :a 1)) := :a
  (val (pair :a 2)) := 2
  (key (map-entry :a 1)) := :a
  (val (map-entry :a 2)) := 2
  )

(tests
  "Retrocompat with MapEntry"
  (pair? (pair 1 2)) := true
  (map-entry? (map-entry 1 2)) := true
  (pair? (map-entry 1 2)) := true
  (map-entry? (pair 1 2)) := true
  )

(tests
  "Respects MapEntry downcast"
  ;; IVector
  (vector? (map-entry 1 2)) := true
  (vector? (pair 1 2)) := true
  ;; ICounted
  (count (pair 1 2)) := 2
  ;; ISeq
  (seq (pair 1 2)) := '(1 2)
  ;; IStack
  (peek (pair 1 2)) := 2
  (pop (pair 1 2)) := [1]
  ;; ICollection
  (conj (pair 1 2) 3) := [1 2 3]
  (cons 0 (pair 1 2)) := '(0 1 2)
  ;; Rseq
  (reverse (pair 1 2)) := '(2 1)
  ;; Empty
  (empty (pair 1 2)) := nil
  ;; Reducible
  (reduce + (pair 1 2)) := 3
  )

(tests
  "Associative"
  (get (pair 1 2) 0) := 1
  (get (pair 1 2) 1) := 2
  (get (pair 1 2) 2) := nil
  (get (pair 1 2) 2 :not-found) := :not-found

  (assoc (pair :a 1) 0 :b) := (pair :b 1)
  (assoc (pair :a 1) 1 2) := (pair :a 2)
  )

(tests
  "Lookup"
  (find (pair :a "a") 0) := [0 :a]
  (find (pair :a "a") 1) := [1 "a"])

(tests
  "IFn"
  ((pair :a 1) 0) := :a
  ((pair :a 1) 1) := 1)

(defn swap "swap :: (a,b) -> (b, a). Swap the components of a pair." [p] (pair (snd p) (fst p)))

(tests
  (fst (swap (pair 1 2))) := 2
  (snd (swap (pair 1 2))) := 1)

(defn curry "curry :: ((a, b) -> c) -> a -> b -> c. Convert an uncurried function to a curried function."
  ([f] (partial curry f))
  ([f x] (partial curry f x))
  ([f x y] (f (pair x y))))

(defn uncurry "uncurry :: (a -> b -> c) -> ((a, b) -> c). Converts a curried function to a function on pairs."
  ([f] (partial uncurry f))
  ([f pair]
   (f (fst pair) (snd pair))))

(tests
  (((curry fst) 1) 2)
  := 1
  ((uncurry +) (pair 1 2))
  := 3

  ((uncurry str) (swap (pair 1 2)))
  := "21")

(extend-protocol Bifunctor
  ;; Redefine bimap to preserve metadata, if supported.
  ;; Bifunctor on MapEntry is already defined in the `Bifunctor` namespace.
  #?(:clj clojure.lang.AMapEntry :cljs MapEntry)
  (-bimap [entry f g]
    (if (has-meta? entry)
      (pair* (f (fst entry)) (g (snd entry)) (meta entry))
      (map-entry (f (fst entry)) (g (snd entry))))))

(defn &&& "Like `juxt`, but takes exactly two functions and return a concrete pair instead of a generic vector.
  &&& :: (a -> b) -> (a -> c) -> a -> (b, c).
  Given two functions, apply both to a single argument to form a pair. A specialised version Control.Arrow.&&&."
  ;; In haskell, this is defined in Data.Tuple.Extra as a specialized version of Control.Arrow.&&&.
  ;; I decided to put it here because I didn't want to mimic haskell
  ;; namespaces just for the sake of it. Also this specialized version is
  ;; specific to tuples and functions, and I decided to define pairs (tuples) here
  ;; so it makes sense to keep this fn next to it.
  ;; It can be moved elsewhere if needed.
  ([f] (partial &&& f))
  ([f g] (partial &&& f g))
  ([f g a]
   ;; future work: implement in term of arrows/category as
   ;; f &&& g = arr (\b -> (b, b)) >>> f *** g
   ;; Where:
   ;; - arr is the arrow lifter
   ;; - >>> is g ∘ f
   ;; - *** is bimap
   ;; https://hackage-content.haskell.org/package/extra-1.8/docs/Data-Tuple-Extra.html#v:-38--38--38-
   (pair (f a) (g a))))
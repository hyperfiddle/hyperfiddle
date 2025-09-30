(ns hyperfiddle.hfql1.bifunctor
  "https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Bifunctor.html"
  (:refer-clojure :exclude [first second #?(:cljs -first) #?(:cljs -second)])
  (:require [hyperfiddle.hfql1.function :refer [id]]
            [hyperfiddle.hfql1.functor :refer [Functor fmap]]
            [hyperfiddle.rcf :refer [tests]]))

(defprotocol Bifunctor
  (-bimap [p f g] "bimap :: (a -> b) -> (c -> d) -> p a c -> p b d"))

(defprotocol BifunctorFirstSecond
  "Optional: implement only if you choose to define `first` and `second` instead of `bimap`."
  (-first [p f] "first :: (a -> b) -> p a c -> p b c")
  (-second [p f] "second :: (a -> b) -> p a b -> p a c"))

(declare bimap)
(defn first "first :: (a -> b) -> p a c -> p b c .  Map covariantly over the first argument."
  ([f] (partial first f))
  ([f p]
   (if (satisfies? BifunctorFirstSecond p)
     (-first p f)
     (bimap f id p))))

(defn second "second :: (a -> b) -> p a b -> p a c . Map covariantly over the second argument."
  ([f] (partial second f))
  ([f p]
   (if (satisfies? BifunctorFirstSecond p)
     (-second p f)
     (bimap id f p))))

(defn bimap "bimap :: (a -> b) -> (c -> d) -> p a c -> p b d . Map over both arguments at the same time."
  ([f] (partial bimap f))
  ([f g] (partial bimap f g))
  ([f g p]
   (if (satisfies? Bifunctor p)
     (-bimap p f g)
     (if (satisfies? BifunctorFirstSecond p)
       (first f (second g p))
       (throw (ex-info "Object is not an instance of Bifunctor" {:object p}))))))

(defprotocol IPair
  (fst [pair] "fst :: (a, b) -> a.  Extract the first component of a pair.")
  (snd [pair] "snd :: (a, c) -> b. Extract the second component of a pair."))

(defn pair [a b] (clojure.core/first {a b}))
(defn pair? [p] (satisfies? IPair p))

(defn &&& "Like `juxt`, but takes exactly two functions and return a concrete pair instead of a generic vector.
  &&& :: (a -> b) -> (a -> c) -> a -> (b, c).
  Given two functions, apply both to a single argument to form a pair. A specialised version Control.Arrow.&&&."
  ;; In haskell, this is defined in Data.Tuple.Extra as a specialized version of Control.Arrow.&&&.
  ;; I decided to put it in Bifunctor because I didn't want to mimic haskell
  ;; namespaces just for the sake of it. Also this specialized version is
  ;; specific to tuples and functions, and I decided to define pairs in
  ;; Bifunctor, so it makes sense to keep this fn next to it.
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

(extend-type #?(:clj clojure.lang.AMapEntry :cljs MapEntry)
  IPair
  (fst [entry] (key entry))
  (snd [entry] (val entry))
  Bifunctor
  (-bimap [entry f g] (clojure.core/first {(f (fst entry)) (g (snd entry))})))

(defrecord PairWithMeta [a b]
  IPair
  (fst [_] a)
  (snd [_] b)
  Bifunctor
  (-bimap [pair f g] (PairWithMeta. (f (fst pair)) (g (snd pair)))))

(defn pair-with-meta [a b] (->PairWithMeta a b))

#_(extend-type #?(:clj clojure.lang.IPersistentVector, :cljs PersistentVector)
  IPair
  (fst [vector] (clojure.core/first vector))
  (snd [vector] (clojure.core/second vector))
  Bifunctor
  (-bimap [vector f g] (-> vector (update 0 f) (update 1 g))))

(tests
  "second ≡ fmap"
  (second inc (pair nil 1))
  := (fmap inc (pair nil 1)))

(tests
  "second id ≡ id"
  (second id (pair nil 1))
  := (pair nil 1))

(tests
  "bimap id id ≡ id"
  (bimap id id (pair 1 2))
  := (pair 1 2))

(tests
  "first id ≡ id, second id ≡ id"
  (first id (pair "a" 1))
  := (pair "a" 1)

  (second id (pair "a" 1))
  := (pair "a" 1))

(tests
  "bimap f g ≡ first f ∘ second g"
  (bimap inc str (pair 1 2))
  := (first inc (second str (pair 1 2))))

(tests
  "bimap (f ∘ g) (h ∘ i) ≡ bimap f h ∘ bimap g i"
  (bimap (comp str inc) (comp str inc) (pair 1 2))
  := (bimap str str (bimap inc inc (pair 1 2))))

(tests
  "first (f ∘ g) ≡ first f ∘ first g"
  (first (comp str inc) (pair 1 2))
  := (first str (first inc (pair 1 2))))

(tests
  "second (f ∘ g) ≡ second f ∘ second g"
  (second (comp str inc) (pair 1 2))
  := (second str (second inc (pair 1 2))))

(defn swap "swap :: (a,b) -> (b, a). Swap the components of a pair." [p] (pair (snd p) (fst p)))

(tests
  (fst (pair 1 2)) := 1
  (snd (pair 1 2)) := 2
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

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
  [f p]
  (if (satisfies? BifunctorFirstSecond p)
    (-first p f)
    (bimap f id p)))

(defn second "second :: (a -> b) -> p a b -> p a c . Map covariantly over the second argument."
  [f p]
  (if (satisfies? BifunctorFirstSecond p)
    (-second p f)
    (bimap id f p)))

(defn bimap "bimap :: (a -> b) -> (c -> d) -> p a c -> p b d . Map over both arguments at the same time."
  [f g p]
  (if (satisfies? Bifunctor p)
    (-bimap p f g)
    (first f (second g p))))

(defrecord Pair [fst snd]
  Functor
  (-fmap [p f] (second f p))
  Bifunctor
  (-bimap [_ f g] (Pair. (f fst) (g snd))))

(tests
  "second ≡ fmap"
  (second inc (->Pair nil 1))
  := (fmap inc (->Pair nil 1)))

(tests
  "second id ≡ id"
  (second id (->Pair nil 1))
  := (->Pair nil 1))

(tests
  "bimap id id ≡ id"
  (bimap id id (->Pair 1 2))
  := (->Pair 1 2))

(tests
  "first id ≡ id, second id ≡ id"
  (first id (->Pair "a" 1))
  := (->Pair "a" 1)

  (second id (->Pair "a" 1))
  := (->Pair "a" 1))

(tests
  "bimap f g ≡ first f ∘ second g"
  (bimap inc str (->Pair 1 2))
  := (first inc (second str (->Pair 1 2))))

(tests
  "bimap (f ∘ g) (h ∘ i) ≡ bimap f h ∘ bimap g i"
  (bimap (comp str inc) (comp str inc) (->Pair 1 2))
  := (bimap str str (bimap inc inc (->Pair 1 2))))

(tests
  "first (f ∘ g) ≡ first f ∘ first g"
  (first (comp str inc) (->Pair 1 2))
  := (first str (first inc (->Pair 1 2))))

(tests
  "second (f ∘ g) ≡ second f ∘ second g"
  (second (comp str inc) (->Pair 1 2))
  := (second str (second inc (->Pair 1 2))))

(defn fst "fst :: (a, b) -> a.  Extract the first component of a pair." [pair] (:fst pair))
(defn snd "snd :: (a, b) -> b. Extract the second component of a pair." [pair] (:snd pair))
(defn swap "swap :: (a,b) -> (b, a). Swap the components of a pair." [pair] (->Pair (snd pair) (fst pair)))

(tests
  (fst (->Pair 1 2)) := 1
  (snd (->Pair 1 2)) := 2
  (fst (swap (->Pair 1 2))) := 2
  (snd (swap (->Pair 1 2))) := 1)

(defn curry "curry :: ((a, b) -> c) -> a -> b -> c. Convert an uncurried function to a curried function."
  ([f] (partial curry f))
  ([f x] (partial curry f x))
  ([f x y] (f (->Pair x y))))

(defn uncurry "uncurry :: (a -> b -> c) -> ((a, b) -> c). Converts a curried function to a function on pairs."
  ([f] (partial uncurry f))
  ([f pair]
   (f (fst pair) (snd pair))))

(tests
  (((curry fst) 1) 2)
  := 1
  ((uncurry +) (->Pair 1 2))
  := 3

  ((uncurry str) (swap (->Pair 1 2)))
  := "21")

(ns hyperfiddle.ct.store
  (:refer-clojure :exclude [peek -peek extend])
  (:require [hyperfiddle.ct.functor :refer [Functor fmap]]
            [hyperfiddle.ct.comonad :refer [Comonad extend extract duplicate comonad-law-1 comonad-law-2 comonad-law-3]]
            [hyperfiddle.ct.pair :refer [pair &&&]]
            [hyperfiddle.rcf :refer [tests]]
            #?(:clj [clojure.tools.logging])
            ))

(defn- run-once
  "Take a function `f` and return a function taking `x`, running `(f x)` once, and
  caching the result. Subsequent calls will ignore `x` and return the first
  run's cached result. Use case: The store comonad pairs an accessor (focus)
  with a value. The accessor is supposed to be pure and the value is tied to the
  Store comonad instance. Extracting a value out of a Store will always call
  accessor with the same stored value. Changing the value will instantiate a new
  Store. There is no point running the accessor more than once for a given instance."
  [f]
  (let [sentinel #?(:clj (Object.) :cljs (js-obj))
        !cache (volatile! sentinel)]
    (fn run-once [x]
      (let [cached @!cache]
        (if (identical? sentinel cached)
          (vreset! !cache (f x))
          cached)))))

(defprotocol ComonadStore
  (-pos [wa] "pos :: w a -> s. Read the stored value")
  (-peek [s wa] "peek :: s -> w a -> a. Peek at what the current focus would be for a different stored value."))

(defrecord Store [raw-accessor cached-accessor value]
  Functor
  (-fmap [fa f] (let [accessor (comp f raw-accessor)]
                  (Store. accessor (run-once accessor) value)))
  Comonad
  (-extract [_wa] (cached-accessor value))
  (-duplicate [_wa] (let [accessor (fn [s] (Store. raw-accessor cached-accessor s))]
                      (Store. accessor (run-once accessor) value)))
  (-extend [wa k] (fmap k (duplicate wa)))
  ComonadStore
  (-pos [_wa] value)
  (-peek [_wa s] (raw-accessor s)))



(defn store "store :: (s -> a) -> s -> Store s a" [accessor value] (->Store accessor (run-once accessor) value))

(defn pos "pos :: w a -> s. Read the stored value" [wa] (-pos wa))
(defn peek "peek :: s -> w a -> a. Peek at what the current focus would be for a different stored value" [s wa] (-peek wa s))
(defn peeks "peeks :: (s -> s) -> w a -> a.
  Peek at what the current focus would be if the stored value was modified by some function."
  ([f] (partial peeks f))
  ([f wa]
   (peek (f (pos wa)) wa)))

(defn seek "seek :: s -> w a -> w a. Set the stored value." [s wa]
  (if (instance? Store wa)
    (store (:raw-accessor wa) s)
    (extend (fn [w] (peek s w)) wa)))

(defn seeks "seeks :: (s -> s) -> w a -> w a. Modify the stored value."
  ([f] (partial seeks f))
  ([f wa] (seek (f (pos wa)) wa)))

(defn experiment "experiment :: Functor f => (s -> f s) -> w a -> f a.
  Applies a functor-valued function to the stored value, and then uses the new accessor to read the resulting focus."
  [s->fs wa]
  (fmap (fn [s] (peek s wa)) (s->fs (pos wa))))


(tests
  (comonad-law-1 (store identity 1))
  := true
  )

(tests
  (comonad-law-2 (comp inc extract) (store identity 1))
  := true
  )

(tests
  (comonad-law-3 (comp str extract) (comp inc extract) (store identity 1))
  := true
  )

(tests
  (pos (store identity 1))
  := 1
  (pos (store inc 1))
  := 1
  )

(tests
  (peek 1 (store (comp inc force) (delay (throw (ex-info "ignore me" {})))))
  := 2
  )

(tests
  (peeks inc (store inc 1))
  := 3
  )

(tests
  (extract (seek 2 (store identity nil)))
  := 2
  )

(tests
  (extract (seeks inc (store identity 1)))
  := 2
  (extract (seeks (constantly 2) (store identity 1)))
  := 2
  )

(tests
  (experiment list (store inc 1))
  := '(2)

  (experiment (&&& identity identity) (store inc 1))
  := [1 2]
  )
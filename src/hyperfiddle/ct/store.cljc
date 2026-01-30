(ns hyperfiddle.ct.store
  (:refer-clojure :exclude [peek -peek extend])
  (:require [hyperfiddle.ct.functor :refer [Functor fmap]]
            [hyperfiddle.ct.comonad :refer [Comonad extend extract duplicate comonad-law-1 comonad-law-2 comonad-law-3]]
            [hyperfiddle.ct.pair :refer [pair &&&]]
            [hyperfiddle.rcf :refer [tests]]
            #?(:clj [clojure.tools.logging])
            ))

(defprotocol ComonadStore
  (-pos [wa] "pos :: w a -> s. Read the stored value")
  (-peek [s wa] "peek :: s -> w a -> a. Peek at what the current focus would be for a different stored value."))

(defrecord Store [accessor value]
  Functor
  (-fmap [fa f] (Store. (comp f accessor) value))
  Comonad
  (-extract [_wa] (accessor value))
  (-duplicate [_wa] (Store. (fn [s] (Store. accessor s)) value))
  (-extend [wa k] (fmap k (duplicate wa)))
  ComonadStore
  (-pos [_wa] value)
  (-peek [_wa s] (accessor s)))



(defn memoize-1 [f]
  (let [sentinel #?(:clj (Object.) :cljs (js-obj))
        state (volatile! [sentinel sentinel])] ; [args ret] ; using volatile for thread safety, prevents "args updated but ret not updated"
    (fn [& args]
      (let [[prev-args prev-ret] @state]
        (if (= args prev-args) ; TODO is this = allowed? Can it blow up on cursed objects?
          prev-ret
          (do
            #?(:clj (clojure.tools.logging/trace 'memoize-1 '≠ {:prev-args prev-args, :next-args args}))
            (let [ret (apply f args)]
              (vreset! state [args ret]) ; retain BOTH args ret AFTER computation. Ensures ret is always in sync with args under concurrency.
              ret)))))))



(defn store "store :: (s -> a) -> s -> Store s a" [accessor value] (->Store (memoize-1 accessor) value))

(defn pos "pos :: w a -> s. Read the stored value" [wa] (-pos wa))
(defn peek "peek :: s -> w a -> a. Peek at what the current focus would be for a different stored value" [s wa] (-peek wa s))
(defn peeks "peeks :: (s -> s) -> w a -> a.
  Peek at what the current focus would be if the stored value was modified by some function."
  ([f] (partial peeks f))
  ([f wa]
   (peek (f (pos wa)) wa)))

(defn seek "seek :: s -> w a -> w a. Set the stored value." [s wa]
  (if (instance? Store wa)
    (store (:accessor wa) s)
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
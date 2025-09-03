(ns hyperfiddle.hfql1.comonad
  (:refer-clojure :exclude [extend])
  (:require [hyperfiddle.hfql1.functor :refer [->Identity runIdentity fmap #?(:cljs Identity)]])
  #?(:clj (:import [hyperfiddle.hfql1.functor Identity])))

(defprotocol Comonad
  (-extract [Wa] "extract :: w a -> a")
  (-duplicate [Wa] "duplicate :: w a -> w (w a)")
  (-extend [Wa wa->b] "extend :: (w a -> b) -> w a -> w b"))

(defn extract "Return the value wrapped in the comonadic context." [Wa]  (-extract Wa))

(defn duplicate "Turn a structure `w a` into the tree of all its sub-contexts: each node’s
  label becomes the subtree rooted at that node, preserving shape.

  Dual of monadic join.
  duplicate :: w a -> w (w a)
  join      :: m (m a) -> m a
  Here specialised to the Cofree Comonad:
  duplicate :: Cofree f a -> Cofree f (Cofree f a) "
  [Wa] (-duplicate Wa))

(defn extend "Given a function f :: (w a -> b), return a structure with the same shape where each position is annotated by `(f context-at-that-position)`.
  A.k.a. cobind – dual of monadic bind >>=.
  extend :: (w a -> b) -> w a -> w b -- W stands for Comonad, as an upside down M.
  bind   :: m a -> (a -> m b) -> m b "
  ([wa->b] (partial extend wa->b))
  ([wa->b Wa] (-extend Wa wa->b)))

(defn liftW
  ([a->b] (extend (comp a->b extract)))
  ([a->b Wa] (extend (comp a->b extract) Wa)))


;;; Example implementations

(extend-type Identity ; NOTE Identity is also a monad and forms a bimonad
  Comonad
  (-extract [IDa] (runIdentity IDa))
  (-duplicate [IDa] (->Identity IDa))
  (-extend [IDa f] (fmap  f (duplicate IDa))))

(extend-type #?(:clj clojure.lang.Delay, :cljs Delay) ; NOTE Delay is also a monad and forms a bimonad.
  Comonad
  (-extract [D] (force D))
  (-duplicate [D] (delay D))
  (-extend [D f] (delay (f D))))



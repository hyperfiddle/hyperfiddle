(ns hyperfiddle.ct.applicative
  (:require [hyperfiddle.ct.function :refer [id const $ flip]]
            [hyperfiddle.ct.functor :refer [fmap <$ runIdentity #?(:cljs Identity)]]
            [hyperfiddle.rcf :refer [tests]])
  #?(:clj (:import [hyperfiddle.ct.functor Identity]))
  #?(:cljs (:require-macros [hyperfiddle.ct.applicative :refer [with-context]])))

(defprotocol Applicative
  (-fapply [fa->b fa] "fapply aka <*> :: f (a -> b) -> f a -> f b")
  (-pure [_ a] "pure :: a -> f a"))

(defprotocol ApplicativeLiftA2
  "Optional. Implement only if you need to overload liftA2"
  (-liftA2 [fa a->b->c fb] "liftA2 :: (a -> b -> c) -> f a -> f b -> f c"))

(defn fapply "<*> :: f (a -> b) -> f a -> f b" [fa->b fa]
  {:pre [(satisfies? Applicative fa)]
   :post [(satisfies? Applicative %) #_(= (type fa) (type %))]}
  (-fapply fa->b fa))

(def <*> "<*> aka fapply :: f (a -> b) -> f a -> f b" fapply)
(def <**> "<**> aka flipped fapply :: f a -> f (a -> b) -> f b" (flip <*>))

(defn liftA2 "liftA2 :: (a -> b -> c) -> f a -> f b -> f c"
  ([a->b->c] (partial liftA2 a->b->c))
  ([a->b->c fa] (partial liftA2 a->b->c fa))
  ([a->b->c fa fb]
   (if (satisfies? ApplicativeLiftA2 fa)
     (-liftA2 fa a->b->c fb)
     (fapply (fmap #(partial a->b->c %) fa) fb))))

(def ^:dynamic *context*)

(defmacro with-context [context & body]
  `(binding [*context* ~context]
     ~@body))

(defn pure "pure :: a -> f a" [a]
  (if #?(:clj (bound? #'*context*) :cljs (some? *context*))
    (-pure *context* a)
    (throw (ex-info "Missing applicative context calling pure" {:value a}))))

(defn *> "f a -> f b -> f b – Sequence actions, discarding the value of the first argument." [fa fb]
  (<*> (<$ id fa) fb))

(defn <* "f a -> f b -> f a – Sequence actions, discarding the value of the second argument." [fa fb]
  (liftA2 const fa fb))

(defn liftA "(a -> b) -> f a -> f b "
  ([f] (partial liftA f))
  ([f a] (<*> (pure f) a)))

;;; Example implementations

(extend-type Identity
  Applicative
  (-pure [_ a] (Identity. a))
  (-fapply [IDf IDa]
    (binding [*context* IDf]
      (pure ((runIdentity IDf) (runIdentity IDa)))))
  ApplicativeLiftA2
  (-liftA2 [fa a->b->c fb] ; example impl
    (binding [*context* fa]
      (fapply (fmap #(partial a->b->c %) fa) fb))))

(extend-type #?(:clj clojure.lang.Delay, :cljs Delay)
  Applicative
  (-pure [_ x] (delay x))
  (-fapply [Df Da] (delay ((force Df) (force Da)))))

(extend-type #?(:clj clojure.lang.Fn, :cljs function)
  Applicative
  (-pure [_ a] (constantly a))
  (-fapply [Ff Fa] (fn [b] ((Ff b) (Fa b)))))

(tests
  (fapply (Identity. inc) (Identity. 1))
  := (Identity. 2)
  ((liftA2 +) (Identity. 1) (Identity. 2))
  := (Identity. 3)
  )

;;; Laws

(tests
  "identity – pure id <*> v = v"
  (binding [*context* (Identity. nil)]
    (let [v (pure 1)]
      (<*> (pure identity) v)
      := v))


  "composition – pure (.) <*> u <*> v <*> w = u <*> (v <*> w)"
  (binding [*context* (Identity. nil)]
    (let [dot (partial partial comp) ; (.)
          u (pure str)
          v (pure inc)
          w (pure 1)]
      (-> (pure dot) (<*> u) (<*> v) (<*> w))
      := (<*> u (<*> v w))))

  "homomorphism – pure f <*> pure x = pure (f x)"

  (binding [*context* (Identity. nil)]
    (let [f inc
          x 1]
      (<*> (pure f) (pure x))
      := (pure (f x))))

  "interchange – u <*> pure y = pure ($ y) <*> u"
  (binding [*context* (Identity. nil)]
    (let [u (pure inc)
          y 1]
      (<*> u (pure y))
      := (<*> (pure ($ y)) u)))

  "fmap f x = pure f <*> x"
  (binding [*context* (Identity. nil)]
    (fmap inc (pure 1))
    := (<*> (pure inc) (pure 1)))

  )

(ns hyperfiddle.hfql1.monad
  (:require [hyperfiddle.hfql1.function :refer [const id]]
            [hyperfiddle.hfql1.functor :refer [fmap runIdentity #?(:cljs Identity)]]
            [hyperfiddle.hfql1.applicative :as A :refer [fapply pure *>]]
            [hyperfiddle.rcf :refer [tests]])
  #?(:clj (:import [hyperfiddle.hfql1.functor Identity]))
  #?(:cljs (:require-macros [hyperfiddle.hfql1.monad :refer [with-context]])))

(defprotocol Monad
  (-bind [ma a->ma] ">>= :: (a -> m b) -> m a -> m b"))

(defprotocol MonadJoin
  "Optional. Implement only if you need to overload join."
  (-join [mma] "join :: (Monad m) => m (m a) -> m a"))

(defprotocol MonadDiscard
  "Optional. Implement only if you need to overload >>."
  (_>> [ma mb] "(>>) :: :: forall a b. m a -> m b -> m b "))

(def ^:dynamic *context*)

(defmacro with-context [context & body]
  `(binding [*context* ~context]
     (A/with-context *context*
       ~@body)))

(defn bind [ma a->mb]
  {:pre [(satisfies? Monad ma)]
   :post [(satisfies? Monad %) (= (type ma) (type %))]}
  (binding [*context* ma]
    (-bind ma (fn [a]
                (binding [*context* ma]
                  (a->mb a))))))

(def >>= bind)

(defn >> ">> :: :: forall a b. m a -> m b -> m b " [ma mb]
  (if (satisfies? MonadDiscard ma)
    (_>> ma mb)
    (*> ma mb)))

(defn join "join :: (Monad m) => m (m a) -> m a" [mma]
  (if (satisfies? MonadJoin mma)
    (-join mma)
    (>>= mma id)))

(defn return "return = pure :: a -> " [a]
  (if #?(:clj (bound? #'*context*) :cljs (some? *context*))
    (binding [A/*context* *context*]
      (pure a))
    (throw (ex-info "Missing monadic context calling return" {:value a}))))

(defn fapplyT "Default implementation of fapply for any monad" [mf ma]
  (>>= mf (fn [f] (fmap f ma))))

(def ap fapply)

;;; Example implementations

(extend-type Identity
  Monad
  (-bind [ma a->mb]
    (binding [*context* ma]
      (a->mb (runIdentity ma)))))

(extend-type #?(:clj clojure.lang.Delay, :cljs Delay)
  Monad
  (-bind [ma a->mb] (delay (force (a->mb (force ma))))))

;;; Laws

(tests
  "return a >>= k  =  k a"

  (binding [*context* (Identity. nil)]
    (let [k (comp return inc)]
      (>>= (return 1) k)
      :=
      (k 1)
      ))

  "m >>= return  =  m"

  (binding [*context* (Identity. nil)]
    (let [m (Identity. 1)]
      (>>= m return)
      := m))


  "m >>= (\\x -> k x >>= h)  =  (m >>= k) >>= h"

  (let [m (Identity. 1)
        k (comp return inc)
        h (comp return str)
        fx (fn [x] (>>= (k x) h))
        ]
    (binding [*context* m]
      (>>= m (fn [x] (>>= (k x) h)))
      := (>>= (>>= m k) h)))


  "fmap f xs  =  xs >>= return . f"

  (binding [*context* (Identity. nil)]
    (fmap inc (Identity. 1))
    := (>>= (Identity. 1) (comp return inc)))
  )

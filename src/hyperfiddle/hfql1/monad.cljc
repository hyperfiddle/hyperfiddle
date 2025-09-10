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

(defmulti valid-bind-return-value?
  "Monadic bind is supposed to produce the same monad as the one passed in.
  But given clojure shenanigans we also allow subtypes. E.g.
  - MetaFn <: Fn – is a subtype of
  - (type +) ≠ (type -) in clojure, but true in cljs
  - PersistentArrayMap ≮: PersistentHashMap and vice-versa and accepting common ancestors is brittle."
  (fn [input output] (type input)))

(defmethod valid-bind-return-value? :default [input output] (isa? (type output) (type input)))

(defn bind [ma a->mb]
  {:pre [(satisfies? Monad ma)]
   :post [(satisfies? Monad %) (valid-bind-return-value? ma %) #_(= (type ma) (type %))]}
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

;;; Extensions

(extend-type #?(:clj clojure.lang.Fn, :cljs function)
  ;; Functions are monads: https://hackage.haskell.org/package/ghc-internal-9.1201.0/docs/src/GHC.Internal.Base.html#line-1539
  Monad
  (-bind [Ff k] (fn bound [a] ((k (partial Ff a)) a))))

(defmethod valid-bind-return-value? #?(:clj clojure.lang.Fn, :cljs js/Function) [input output]
  (and (fn? input) (fn? output)))


(tests
  ((join +) 2) := 4  ; join on a function duplicates arguments
  ((join *) 5) := 25
  ((join +) 2)
  := (#(+ % %) 2)

  ((join +) 2)
  := ((comp (partial apply +) (juxt identity identity)) 2)
  )

(tests
  ((bind id (fn [xf] #(+ (xf) %))) ; ⚠ note `id` has been curried and must be forced. Clojure doesn't have proper auto-currying.
   4)
  := 8)
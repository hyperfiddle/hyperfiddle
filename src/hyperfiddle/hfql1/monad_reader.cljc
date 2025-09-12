(ns hyperfiddle.hfql1.monad-reader
  (:require
   [hyperfiddle.hfql1.function :refer [const flip id dot]]
   [hyperfiddle.hfql1.functor :refer [Functor fmap]]
   [hyperfiddle.hfql1.applicative :refer [Applicative pure fapply]]
   [hyperfiddle.hfql1.monad :refer [>>= Monad return *context* with-context]]
   [hyperfiddle.rcf :refer [tests]]))

(declare runReader)

(defprotocol MonadReader
  (-reader [_ r->a] "reader :: (r -> a) -> m a")
  (-local [ma r->r] "local :: (r -> r) -> m a -> m a"))

(defrecord Reader [r->a]
  Functor
  (-fmap [ma a->b] (Reader. (comp a->b r->a)))
  Applicative
  (-fapply [mf mx] (Reader. (fn [r]
                              (let [f (runReader mf r)
                                    x (runReader mx r)]
                                (f x)))))
  (-pure [_ r->a] (Reader. (constantly r->a)))
  Monad
  (-bind [ma k] (Reader. (fn [r] (runReader (k (runReader ma r)) r))))
  MonadReader
  (-local [ma r->r] (Reader. (fn [r] (runReader ma (r->r r)))))
  (-reader [_ r->a] (Reader. r->a)))

(defn local "local :: (r -> r) -> Reader r a -> Reader r a" 
  ([r->r] (partial local r->r))
  ([r->r Ra] (-local Ra r->r)))
(defn reader [r->a] (-reader (->Reader id) r->a))
(defn ask "ask :: Reader r r" [] (reader id))

(defn asks "asks : (r -> a) -> Reader r a" [r->a] (reader r->a))

(defn runReader "runReader :: Reader r a -> r -> a"
  ([Ra] (:r->a Ra))
  ([Ra r] ((runReader Ra) r)))

(tests
  (runReader (ask) 1)
  := 1
  )


;; Laws

(tests
  "Functor identity"
  (runReader (fmap id (reader inc)) 1)
  :=
  (runReader (reader inc) 1)

  "Functor composition"
  (runReader (fmap (comp str inc) (reader id)) 1)
  :=
  (runReader (fmap str (fmap inc (reader id))) 1))

(tests
  "Applicative identity"
  (with-context (reader id)
    (runReader (fapply (pure id) (reader #(+ % 10))) 3))
  :=
  (runReader (reader #(+ % 10)) 3)

  "Applicative homomorphism"
  (with-context (reader id)
    (runReader (fapply (pure inc) (pure 41)) 0)
    :=
    (runReader (pure 42) 0))

  "Applicative interchange"
  (with-context (reader id)
    (runReader (fapply (reader (fn [r] (fn [x] (+ x r)))) (pure 2)) 5)
    :=
    (runReader (fapply (pure #(% 2)) (reader (fn [r] (fn [x] (+ x r))))) 5))

  "Applicative composition"
  (with-context (reader id)
    (runReader
      (-> (pure dot)
        (fapply (reader (fn [r] (fn [x] (+ x r)))))
        (fapply (reader (fn [_] inc)))
        (fapply (reader (fn [_] 1))))
      3)
    :=
    (runReader
      (fapply (reader (fn [r] (fn [x] (+ x r))))
        (fapply (reader (fn [_] inc))
          (reader (fn [_] 1))))
      3))
  )

(tests
  "Monad left identity"
  (with-context (reader id)
    (runReader (>>= (return 7) (fn [x] (reader (fn [r] (+ x r))))) 3))
  :=
  (runReader ((fn [x] (reader (fn [r] (+ x r)))) 7) 3)

  "Monad right identity"
  (with-context (reader id)
    (runReader (>>= (reader #(+ % 3)) return) 10))
  :=
  (runReader (reader #(+ % 3)) 10)

  "Monad associativity"
  (runReader
    (>>= (>>= (reader #(+ % 3))
           (fn [x] (reader (fn [r] (+ x r)))))
      (fn [y] (reader (fn [r] (- y r)))))
    4)
  :=
  (runReader
    (>>= (reader #(+ % 3))
      (fn [x]
        (>>= (reader (fn [r] (+ x r)))
          (fn [y] (reader (fn [r] (- y r)))))))
    4))

(tests
  "MonadReader ask"
  (runReader (ask) 9)
  :=
  9

  "MonadReader local"
  (runReader (local inc (reader #(* % 2))) 10)
  :=
  (runReader (reader #(* (inc %) 2)) 10)

  "MonadReader reader"
  (runReader (reader #(+ % 5)) 3)
  :=
  8)


(ns hyperfiddle.hfql1.functor
  (:require [hyperfiddle.rcf :refer [tests]]
            [hyperfiddle.hfql1.function :refer [flip const]]))

(defprotocol Functor
  (-fmap [fa a->b] "fmap :: Functor f => (a -> b) -> f a -> f b"))

(defprotocol FunctorDiscard
  "Optional. Implement if you need to overload <$."
  (-<$ [fb a] "Discard right. <$ :: Functor f => a -> f b -> f a"))

(defn functor? [f] (satisfies? Functor f))

(defn fmap "fmap :: (a -> b) -> f a -> f b"
  ([a->b] (partial fmap a->b))
  ([a->b fa] (-fmap fa a->b)))

(defn <$ "Discard right. <$ :: Functor f => a -> f b -> f a"
  ([a] (partial <$ a))
  ([a fb]
   (if (satisfies? FunctorDiscard fb)
     (-<$ fb a)
     (fmap (const a) fb))))

(defn $> "Discard left. $> :: Functor f => f a -> b -> f b"
  ([fa] (partial $> fa))
  ([fa b] ((flip <$) fa b)))

(def <$>"Alias for `fmap`" fmap)

(defn <&> "Flipped `<$>` a.k.a. `fmap`. <&> :: Functor f => f a -> (a -> b) -> f b"
  ([fa] (partial <&> fa))
  ([fa a->b] ((flip <$>) fa a->b)))

(def unzip "unzip :: Functor f => f (a, b) -> (f a, f b)" (juxt (fmap first) (fmap second)))
(def void "void :: Functor f => f a -> f ()" (<$ ()))

;;; Laws

(defn identity-law [fa] (= (fmap identity fa) fa))
(defn composition-law [f g fa] (= (fmap (comp f g) fa) (fmap f (fmap g fa))))

;;; Example implementations

(defrecord Identity [a] ; useful for composition
  Functor
  (-fmap [_fa a->b] (Identity. (a->b a))))

(defn runIdentity "The Identity F-Algebra on Identity" [Id_a] (:a Id_a))

;; Question: do we want to implement Functor (and Applicative/Monad/ etc.) onto the implicit clojure-baked Identity context?
;; If so `(fmap inc 1)` would be strictly equivalent to `(inc 1)`. Comonad's extract would be identity, etc.
;; It's easy to do: implement it on Object/:default.
;; But is it a good idea?

;; FIXME cljs can't extend a protocol onto another protocol (e.g. ISeq)
;; Solution (todo implement):
;; - list all concrete types
;; - or:
;; #?(:cljs (extend-protocol Functor
;;            default
;;            (-<$ [x]
;;              (if (satisfies? Protocol x)
;;                ;; one impl for all instances
;;                (impl x)
;;                (throw (js/Error. (str "FunctorDiscard/<$ not supported for type: " (type x))))))))
(extend-protocol Functor
  ;; List
  #?(:clj clojure.lang.ISeq, :cljs ISeq)
  (-fmap [fa a->b] (map a->b fa)) ; TODO carry metadata over

  #?(:clj clojure.lang.PersistentList$EmptyList :cljs EmptyList)
  (-fmap [fa _a->b] fa)

  #?(:clj clojure.lang.Cons :cljs Cons)
  (-fmap [fa a->b] (cons (a->b (first fa)) (lazy-seq (fmap a->b (rest fa)))))

  ;; Vector
  #?(:clj clojure.lang.IPersistentVector, :cljs PersistentVector)
  (-fmap [fa a->b] (mapv a->b fa))

  ;; Map
  #?(:clj clojure.lang.IPersistentMap :cljs IMap)
  (-fmap [fa a->b] (update-vals fa a->b))

  ;; Set
  #?(:clj clojure.lang.IPersistentSet :cljs ISet)
  (-fmap [fa a->b] (into (empty fa) (fmap a->b (seq fa))))

  ;; MapEntry
  #?(:clj clojure.lang.AMapEntry :cljs MapEntry)
  (-fmap [fa a->b] (first {(key fa) (a->b (val fa))}))

  #?(:clj clojure.lang.Delay, :cljs Delay)
  (-fmap [fa a->b] (delay (a->b (force fa))))

  #?(:clj clojure.lang.Fn, :cljs function)
  (-fmap [fa f] (comp f fa))
  )

(extend-protocol FunctorDiscard
  #?(:clj clojure.lang.IPersistentSet, :cljs ISet)
  (-<$ [fb a] (conj (empty fb) a))
  )


(comment
  (composition-law dec inc (Identity. 1))
  )


(comment
  (hyperfiddle.rcf/enable!)
  (hyperfiddle.rcf/enable! false)
  )
(tests
  (identity-law (Identity. 1))
  := true
  (composition-law dec inc (Identity. 1))
  := true

  (identity-law (list 1 2 3 4))
  := true

  (composition-law dec inc (list 1 2 3 4))
  := true


  (fmap inc '(1 2))
  := '(2 3)
  (fmap inc #{1 2})
  := #{3 2}
  (fmap inc [1 2])
  := [2 3]
  (fmap inc {:a 1, :b 2})
  := {:a 2, :b 3}
  (<$ 1 '(1 2))
  := '(1 1)
  (<$ 1 #{1 2})
  := #{1}
  (<$ 1 [1 2])
  := [1 1]
  (<$ 1 {:a 2, :b 3})
  := {:a 1, :b 1}
  (<$ 1 (first {:a 2, :b 3}))
  := [:a 1]

  (unzip (Identity. [1 2]))
  := [(Identity. 1) (Identity. 2)]
  (unzip '([1 2]))
  := '[(1) (2)]
  (unzip '[[1 2]])
  := [[1] [2]]

  (unzip #{[1 2]})
  := [#{1} #{2}]

  (unzip (first {:map-entry [1 2]}))
  := [[:map-entry 1] [:map-entry 2]]

  (unzip {:key [1 2], :value [3 4]})
  := [{:key 1, :value 3} {:key 2, :value 4}]

  (void '()) := '()
  (void '[]) := []
  (void '(1)) := '(())
  (void '[1]) := [()]
  (void #{1 2}) := #{()}
  (void {:a 1, :b 2}) := {:a (), :b ()}
  )

(ns hyperfiddle.ct.cofree
  "Cofree comonads are quite useful for annotating syntax trees, or talking about streams.

  https://hackage.haskell.org/package/free-5.2/docs/Control-Comonad-Cofree.html"
  (:refer-clojure :exclude [extend])
  (:require [hyperfiddle.rcf :refer [tests]]
            [hyperfiddle.ct.functor :refer [Functor fmap ->Identity runIdentity]]
            [hyperfiddle.ct.bifunctor :refer [Bifunctor bimap]]
            [hyperfiddle.ct.pair :refer [pair pair? fst snd]]
            [hyperfiddle.ct.monoid :refer [mappend mempty]]
            [hyperfiddle.ct.comonad :refer [Comonad extract duplicate extend liftW comonad-law-1 comonad-law-2 comonad-law-3]]
            ))

(defprotocol CofreeFunctor
  "This is the base functor of the cofree comonad transformer.
  https://hackage.haskell.org/package/free-5.2/docs/Control-Comonad-Trans-Cofree.html#t:CofreeF
  "
  (headF [fa] "headF :: CofreeF f a b -> a. Extract the head of the base functor.")
  (tailF [fa] "tailF :: CofreeF f a b -> f b. Extract the tails of the base functor"))

(defrecord CofreeF [a as]
  Functor
  (-fmap [_ f] (CofreeF. a (fmap f as)))
  Bifunctor
  (-bimap [_ f g] (CofreeF. (f a) (fmap g as)))
  CofreeFunctor
  (headF [fa] a)
  (tailF [fa] as))

(defprotocol ComonadCofree
  "Allows you to peel a layer off a cofree comonad.
  class (Functor f, Comonad w) => ComonadCofree f w | w -> f
  https://hackage.haskell.org/package/free-5.2/docs/Control-Comonad-Trans-Cofree.html#t:ComonadCofree"
  (-unwrap [Wa] "unwrap :: w a -> f (w a). Remove a layer."))

(defn unwrap "unwrap :: w a -> f (w a). Given a Cofree instance, return its children. i.e. remove a layer."
  [Wa] (-unwrap Wa))

(defprotocol ICofreeT
  (-runCofreeT [this]))

(defn runCofreeT "runCofreeT :: w (CofreeF f a (CofreeT f w a))" [cofreeT] (-runCofreeT cofreeT))

(defrecord CofreeT [fwa] ; CofreeT f w a
  ICofreeT
  (-runCofreeT [_] fwa)
  Functor
  (-fmap [fa f] (CofreeT. (fmap (bimap f (fmap f)) (runCofreeT fa))))
  ;; This is also an Applicative and Monad – implement if needed.
  Comonad
  (-extract [t] (headF (extract (runCofreeT t)))) ; CofreeT f w a -> a
  (-duplicate [t] ; CofreeT f w a -> CofreeT f w (CofreeT f w a)
    (extend identity t))
  (-extend [wa f] ; (CofreeT f w a -> b) -> CofreeT f w a -> CofreeT f w b
    (CofreeT.
      (extend (fn [w]
                (->CofreeF (f (CofreeT. w))
                  (fmap (extend f) (tailF (extract w)))))
        (runCofreeT wa))))
  ComonadCofree
  (-unwrap [t] (tailF (extract (runCofreeT t)))))

(defn cofree "cofree :: CofreeF f a (Cofree f a) -> Cofree f a. Cofree constructor" [fa]
  (assert (satisfies? CofreeFunctor fa))
  (->CofreeT (->Identity fa)))

(defn runCofree "runCofree :: Cofree f a -> CofreeF f a (Cofree f a). Unpeel the first layer off a cofree comonad value." [fa]
  (runIdentity (runCofreeT fa)))


(comment
  (cofree (->CofreeF 1 [(->CofreeF 2 ())]))
  (extract *1)
  (unwrap *1)
  (extract *1)

  (runCofree *1)
  (tailF *1)
  (runCofree *1)
  (headF *1)
  )

(defn coiterT "coiterT :: (Functor f, Comonad w) => (w a -> f (w a)) -> w a -> CofreeT f w a.
  Unfold a CofreeT comonad transformer from a coalgebra and an initial comonad."
  ([coalgebra] (partial coiterT coalgebra))
  ([coalgebra wa]
   (->CofreeT (extend (fn [w] (->CofreeF (extract w) (fmap (coiterT coalgebra) (coalgebra w)))) wa))))



(defn tree-seqT "tree-seqT :: Foldable f => Cofree f a -> [Cofree f a]
  Pre-order traversal of a Cofree structure."
  [t] (cons t (lazy-seq (mapcat tree-seqT (unwrap t)))))



(defn- _countdown-coiter [wa]
  (let [n (extract wa)]
    (if (pos? n) [(delay (dec n))] [])))

(tests
  (def t (coiterT _countdown-coiter (delay 3)))
  (extract t) := 3
  (map extract (unwrap t)) := '(2)

  (def t1 (first (unwrap t)))
  (extract t1) := 2
  (map extract (unwrap t1)) := '(1)

  (def t2 (first (unwrap t1)))
  (extract t2) := 1
  (map extract (unwrap t2)) := '(0)

  (def t3 (first (unwrap t2)))
  (extract t3) := 0
  (empty? (unwrap t3)) := true)

(defn- _branching-coiter [wa]
  (let [n (extract wa)]
    (if (pos? n) [(delay (dec n)) (delay (dec n))] [])))

(tests
  (def level1 (coiterT _branching-coiter (delay 2)))
  (extract level1) := 2
  (def level2 (unwrap level1))
  (map extract level2) := '(1 1)
  (def level3 (mapcat unwrap level2))
  (map extract level3) := '(0 0 0 0)
  (def level4 (map unwrap level3))
  (every? empty? level4) := true)


(tests
  "verify `extend extract = id` for unfold"
  (def t (coiterT _branching-coiter (delay 2)))
  (extract (extend extract t)) := (extract t)
  (map extract (unwrap (extend extract t))) := (map extract (unwrap t)))



(defn hoistCofreeT
  "φ :: ∀x. w x -> w' x  (ideally a comonad morphism)"
  [phi t]
  (->CofreeT
    (phi
      (fmap (fn [fa] (->CofreeF (headF fa) (fmap #(hoistCofreeT phi %) (tailF fa))))
        (runCofreeT t)))))



(defn fold-postorder
  "Post-order fold. φ is an algebra, taking (label, seq of folded child results) and returning b.
   Type: φ :: (a, Seq b) -> b  ; fold-cofree :: Cofree a -> b"
  ([φ] (partial fold-postorder φ))
  ([φ w]
   (φ (extract w) (map (fold-postorder φ) (unwrap w)))))

(defn synthesize
  "Annotate every node with the folded value given by algebra `φ` of its subtree. Lazily computed on extract.
   Equivalent to: fmap (fold-cofree φ) (duplicate w) – i.e. extend composed with a catamorphism.

  > A synthesized attribute is computed from the values of attributes of the
  > children. Since the values of the children must be computed first, this is an
  > example of bottom-up propagation. — https://en.wikipedia.org/wiki/Attribute_grammar#Synthesized_attributes
  "
  ([φ] (partial synthesize φ))
  ([φ w] (extend (fold-postorder φ) w)))
(ns hyperfiddle.hfql1.cofree-comonad ; coco-coco: generate cofree comonads from a coalgebra using coiteration.
  "Cofree comonads are quite useful for annotating syntax trees, or talking about streams.

  https://hackage.haskell.org/package/free-5.2/docs/Control-Comonad-Cofree.html"
  (:refer-clojure :exclude [extend])
  (:require [hyperfiddle.rcf :refer [tests]]))

(defn- ->Cofree
  "Cofree node: a MetaFn carrying a lazy ::value and returning a seq of child thunks at arity 0."
  ([v] (->Cofree v ()))
  ([v children]
   (assert (delay? v) "Cofree `v` must be lazy (wrapped in `delay`) to ensure Cofree-wrapped effects are only computed on request (i.e. exctract), and not on every Cofree structural navigation.")
   (with-meta
     (fn ([] children))
     {::value v}))
  )

(defn extract "Cofree -> a. Realize the effect of the given cofree instance, producing a value."
  [Cofree]
  (force (::value (meta Cofree))))

(defn extend "Given a function f :: (w a -> b), return a structure with the same shape where each position is annotated by `(f context-at-that-position)`.
  A.k.a. cobind – dual of monadic bind >>=.
  extend :: (w a -> b) -> w a -> w b -- W stands for Comonad, as an upside down M.
  bind   :: m a -> (a -> m b) -> m b
  Here specialised to the Cofree Comonad:
  extend :: (Cofree -> b) -> Cofree a -> Cofree b
"
  ([wa->b] (partial extend wa->b))
  ([wa->b Wa]
   (vary-meta
     (->Cofree (delay (wa->b Wa))
       (map (extend wa->b) (Wa)))
     #(merge (meta Wa) %))))

(defn duplicate "Turn a structure `w a` into the tree of all its sub-contexts: each node’s
  label becomes the subtree rooted at that node, preserving shape.

  Dual of monadic join.
  duplicate :: w a -> w (w a)
  join      :: m (m a) -> m a
  Here specialised to the Cofree Comonad:
  duplicate :: Cofree f a -> Cofree f (Cofree f a)
  "
  [Wa] (extend identity Wa))

(defn unwrap "Given a Cofree instance, return its children.
  unwrap :: w a -> f (w a)
  Here specialised to the Cofree Comonad:
  unwrap :: Cofree f a -> f (Cofree f a)
"
 [Wa] (Wa))

(defn- end? [Cofree] (empty? (unwrap Cofree)))

(defn liftW "Comonad w => (a -> b) -> w a -> w b " [a->b] (partial extend (comp a->b extract)))
(defn fmap "(a -> b) -> f a -> f b"
  ([f] (liftW f))
  ([f W] ((fmap f) W)))

(defn lift "Lift one or many projection functions `f` into the cofree-comonadic context.
  e.g. (lift inc), (lift identity), (lift :a), or (lift [(lift :a)]) "
  [proj]
  (cond
    (vector? proj)
    (fn edges
      ([] proj) ; edge descriptor
      ([x]
       (->Cofree
         (delay (map (fn [W] (extract (W x))) proj))
         (map (fn [W] #_(fn []) (W x)) proj))))
    :else
    (fn edge
      ([] '()) ; edge descriptor
      ([x]
       (->Cofree (delay (doto (proj x)
                          #_(->> (prn "Realizing" `(~proj ~x) "=>")))))))))

(tests
  (def _leaf ((lift :a) {:a 1}))
  (end? _leaf)
  := true

  (extract (extend (comp inc extract) _leaf))
  := 2

  (seq? ((extend (comp inc extract) _leaf)))
  := true

  (empty? ((extend (comp inc extract) _leaf)))
  := true
  )

(defn- comonad-law-1 "extend extract = id" [W]
  (= (extract (extend extract W))
    (extract W)))

(tests
  (comonad-law-1 ((lift :a) {:a 1}))
  := true
  )

(defn- comonad-law-2  "extract . extend f = f" [f W]
  (= (extract (extend f W))
    (f W)))

(tests
  (comonad-law-2 (comp inc extract) ((lift :a) {:a 1}))
  := true
  )

(defn- comonad-law-3 "extend f . extend g = extend (f . extend g)" [f g W]
  (let [left (extend f (extend g W)) ; extend f . extend g
        right (extend (comp f (extend g)) W)] ; extend (f . extend g)
    (= (extract left) (extract right))))

(tests
  (comonad-law-3 (comp str extract) (comp inc extract) ((lift :a) {:a 1}))
  := true
  )

(tests
  (extract (fmap inc ((lift :a) {:a 1})))
  := 2
)

(tests ; secondary comonad laws
  "extract . duplicate = id"
  (let [W ((lift :a) {:a 1})]
    (extract (duplicate W))
    := W)

  "fmap extract . duplicate = id"
  (let [W ((lift :a) {:a 1})]
    (extract (fmap extract (duplicate W)))
    := (extract W))

  "duplicate . duplicate = fmap duplicate . duplicate"
  (let [W ((lift :a) {:a 1})
        left (duplicate (duplicate W))
        right ((fmap duplicate) (duplicate W))]
    (extract (extract left))
    := (extract (extract right))
    )
)

(tests ; tertiary comonad laws
  "extend f = fmap f . duplicate"
  (def _f (comp inc extract))
  (def _W ((lift :a) {:a 1}))
  (extract (extend _f _W))
  := (extract ((fmap _f) (duplicate _W)))

  "duplicate = extend id"
  (extract (duplicate _W))
  := (extract (extend identity _W))

  "fmap f = extend (f . extract)"
  (extract (fmap inc _W))
  := (extract (extend (comp inc extract) _W))
  )

(tests
  (def q (lift [(lift :a) (lift :b)]))
  (def n (q {:a 1 :b 2 :c 3}))

  ;; Law-like check: extend extract ≈ identity (observationally)
  (extract (extend extract n))
  := (extract n)

  (map extract (n))
  := '(1 2)

  (map extract ((extend extract n)))
  := '(1 2)

  (every? end? ((extend extract n)))
  := true
  )

;;;;;;;;;;;;;;;;;;;;
;; EXTENDED TESTS ;;
;;;;;;;;;;;;;;;;;;;;

(defn- _leaf-count [W]
  (let [children (W)]
    (if (seq children)
      (reduce + (map _leaf-count children))
      1)))

(tests
  (def _q (lift [(lift :a)
                 (lift :b)
                 (lift [(lift :c)
                        (lift :d)])]))
  (def _n (_q nil))

  (extract (extend _leaf-count _n))
  := 4
  (map extract ((extend _leaf-count _n)))
  := '(1 1 2)
  )

(defn- lazy [object] (if (delay? object) object (delay object)))

;; -- | Unfold a cofree comonad from a seed.
;; unfold :: Functor f => (b -> (a, f b)) -> b -> Cofree f a
;; unfold f c = case f c of
;;   (x, d) -> x :< fmap (unfold f) d
(defn unfold "Build a Cofree from a `coalgebra` and a `seed`.
  `coalgebra` must return a pair of a value `a` and a seqable container of next seeds.
  Maps are supported (children come from (vals fb)).

  unfold :: Functor f => (b    -> [a, f b   ]) -> b    -> Cofree f a
  unfold :: Functor f => (seed -> [a, f seed]) -> seed -> Cofree f a
"
  ([coalgebra] (partial unfold coalgebra))
  ([coalgebra seed]
   (let [[a fb :as step] (coalgebra seed)]
     (vary-meta
       (->Cofree (lazy a)
         (map (unfold coalgebra)
           (cond
             (map? fb)   (vals fb) ; not sure about this one, we may need it for traversals?
             ;; (nil? fb)   nil
             :else       fb)))
       merge (meta step)))))

(defn- _countdown-coalgebra [n] [n (when (pos? n) [(dec n)])])

(tests
  (def t (unfold _countdown-coalgebra 3))
  (extract t) := 3
  (map extract (t)) := '(2)

  (def t1 (first (t)))
  (extract t1) := 2
  (map extract (t1)) := '(1)

  (def t2 (first (t1)))
  (extract t2) := 1
  (map extract (t2)) := '(0)

  (def t3 (first (t2)))
  (extract t3) := 0
  (empty? (t3)) := true)

(defn- _branching-colagebra [n] [n (when (pos? n) [(dec n) (dec n)])])

(tests
  (def level1 (unfold _branching-colagebra 2))
  (extract level1) := 2
  (def level2 (level1))
  (map extract level2) := '(1 1)
  (def level3 (mapcat unwrap level2))
  (map extract level3) := '(0 0 0 0)
  (def level4 (map unwrap level3))
  (every? empty? level4) := true)


(tests
  "verify `extend extract = id` for unfold"
  (def t (unfold _branching-colagebra 2))
  (extract (extend extract t)) := (extract t)
  (map extract ((extend extract t))) := (map extract (t)))

;; coiter :: Functor f => (a -> f a) -> a -> Cofree f a
;; coiter psi a = a :< (coiter psi <$> psi a)
;; coiter f = unfold (id Control.Arrow.&&& f)
(defn coiter "Use coiteration to generate a cofree comonad from a `seed` using given `coalgebra`.
  Accepts any seqable `f a`; if `coalgebra` returns a map, we use its vals.
  coiter :: Functor f => (a -> f a) -> a -> Cofree f a
                             ↓         ↓
                          coalgebra   seed
"
  ([coalgebra] (partial coiter coalgebra))
  ([coalgebra seed]
   (unfold (juxt identity coalgebra) seed)))

(tests "Linear chain"
  (def _linear-coalgebra (fn [n] (when (pos? n) [(dec n)])))
  (def t (coiter _linear-coalgebra 3))
  (extract t) := 3
  (map extract (t)) := '(2)
  (def t1 (first (t)))
  (map extract (t1)) := '(1)
  (def t2 (first (t1)))
  (map extract (t2)) := '(0)
  (end? (first (t2))) := true
  )

(tests "Branching"
  (def _branching-colagebra (fn [n] (when (pos? n) [(dec n) (dec n)])))
  (def b (coiter _branching-colagebra 2))
  (extract b) := 2
  (map extract (b)) := '(1 1)
  (def b2 (mapcat unwrap (b)))
  (map extract b2) := '(0 0 0 0)
  (every? end? b2) := true
  )

(tests "Law check vs unfold"
  ;; coiter psi a == unfold (juxt identity psi) a
  (def _psi _linear-coalgebra)
  (def t1 (coiter _psi 3))
  (def t2 (unfold (juxt identity _psi) 3))
  (extract t1) := (extract t2)
  (map extract (t1)) := (map extract (t2))
  )

(defn fold-preorder
  "Post-order fold. φ takes (label, seq of folded child results) and returns b.
   Type: φ :: (a, Seq b) -> b  ; fold-cofree :: Cofree a -> b"
  ([φ] (partial fold-preorder φ))
  ([φ w]
   (φ (extract w) (map (fold-preorder φ) (unwrap w)))))

(def ^{:arglists '([Cofree])}
  values "Utility. Lazy preorder traversal of cofree values."
  (fold-preorder
    (fn [a bss]
      (lazy-seq (cons a (mapcat identity bss))))))

(tests
  (values ((lift :a) {:a 1})) := '(1)
  (values ((lift [(lift :a)]) {:a 1})) := '((1) 1)
  (values ((lift [(lift :a) (lift :b)]) {:a 1, :b 2, :c 3})) := '((1 2) 1 2)
  )

(defn fold-postorder
  "Post-order fold. φ takes (label, seq of folded child results) and returns b.
   Type: φ :: (a, Seq b) -> b  ; fold-cofree :: Cofree a -> b"
  ([φ] (partial fold-postorder φ))
  ([φ w]
   (φ (extract w) (map (fold-postorder φ) (unwrap w)))))

(defn synthesize
  "Annotate every node with the folded value `φ` of its subtree. Lazily computed on extract.
   Equivalent to: fmap (fold-cofree φ) (duplicate w) – i.e. extend composed with a catamorphism.

  > A synthesized attribute is computed from the values of attributes of the
  > children. Since the values of the children must be computed first, this is an
  > example of bottom-up propagation. — https://en.wikipedia.org/wiki/Attribute_grammar#Synthesized_attributes
  "
  ([φ] (partial synthesize φ))
  ([φ w] (extend (fold-postorder φ) w)))


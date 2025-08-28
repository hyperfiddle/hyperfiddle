(ns hyperfiddle.hfql1.cofree-comonad
  (:refer-clojure :exclude [extend])
  (:require
   [hyperfiddle.rcf :refer [tests]]))

(defn extract "Cofree -> a" [W] (force (::value (meta W))))
(defn extend "(Cofree -> b) -> Cofree a -> Cofree b. Or (w a -> b) -> w a -> w b" [wa->b W]
  (vary-meta
    (fn [] (map (fn [thunk] (fn [] (extend wa->b (thunk)))) (W)))
    merge
    (meta W)
    {::value (delay (wa->b W))}))

(defn duplicate "Cofree f a -> Cofree f (Cofree f a)" [W] (extend identity W))

(defn liftW "Comonad w => (a -> b) -> w a -> w b " [a->b] (partial extend (comp a->b extract)))
(defn fmap "(a -> b) -> f a -> f b"
  ([f] (liftW f))
  ([f W] ((fmap f) W)))

(defn- node
  "Cofree node = MetaFn carrying ::value and returning a seq of child thunks at arity 0."
  ([v] (node v '()))
  ([v kids]
   (with-meta
     (fn ([] kids))
     {::value v})))

(defn lift
  "Lift one or many projection functions `f` into the cofree-comonadic context.
  e.g. (lift inc), (lift identity), (lift :a), or (lift [(lift :a)]) "
  [proj]
  (cond
    (vector? proj)
    (fn edges
      ([] proj) ; edge descriptor
      ([x]
       (node
         (delay (map (fn [W] (extract (W x))) proj))
         (map (fn [W] (fn [] (W x))) proj))))
    :else
    (fn edge
      ([] '()) ; edge descriptor
      ([x]
       (node (delay (proj x)))))))

(defn- end? [continuation] (empty? (continuation)))

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
        right (extend (comp f (partial extend g)) W)] ; extend (f . extend g)
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


(defn- call [f & args] (apply f args)) ; helper

(tests
  (def q (lift [(lift :a) (lift :b)]))
  (def n (q {:a 1 :b 2 :c 3}))

  ;; Law-like check: extend extract ≈ identity (observationally)
  (extract (extend extract n))
  := (extract n)

  (map (comp extract call) (n))
  := '(1 2)

  (map (comp extract call) ((extend extract n)))
  := '(1 2)

  (every? end? (map call ((extend extract n))))
  := true
  )

;;;;;;;;;;;;;;;;;;;;
;; EXTENDED TESTS ;;
;;;;;;;;;;;;;;;;;;;;

(defn- _leaf-count [W]
  (let [kids (map call (W))]
    (if (seq kids)
      (reduce + (map _leaf-count kids))
      1)))

(tests
  (def _q (lift [(lift :a)
              (lift :b)
              (lift [(lift :c)
                     (lift :d)])]))
  (def _n (_q nil))

  (extract (extend _leaf-count _n))
  := 4

  (map (comp extract call) ((extend _leaf-count _n)))
  := '(1 1 2)

  )

(defn unfold
  "unfold :: (b -> [a, f b]) -> b -> Cofree f a
   Build a Cofree (as MetaFn) from a `coalgebra`.
   `coalgebra` must return a pair of a value `a` and a seqable container of next seeds.
   Maps are supported (children come from (vals fb))."
  [coalgebra seed]
  (let [[a fb] (coalgebra seed)
        seeds  (cond
                 (map? fb)   (vals fb) ; not sure about this one, we may need it for traversals?
                 (nil? fb)   nil
                 :else       fb)
        kids   (map (fn [b] (fn [] (unfold coalgebra b))) seeds)]
    (node a kids)))


(defn- _countdown-coalgebra [n] [n (when (pos? n) [(dec n)])])

(tests
  (def t (unfold _countdown-coalgebra 3))
  (extract t) := 3
  (map (comp extract call) (t)) := '(2)

  (def t1 ((first (t))))
  (extract t1) := 2
  (map (comp extract call) (t1)) := '(1)

  (def t2 ((first (t1))))
  (extract t2) := 1
  (map (comp extract call) (t2)) := '(0)

  (def t3 ((first (t2))))
  (extract t3) := 0
  (empty? (t3)) := true)

(defn- _branching-colagebra [n] [n (when (pos? n) [(dec n) (dec n)])])

(tests
  (def level1 (unfold _branching-colagebra 2))
  (extract level1) := 2
  (def level2 (map call (level1)))
  (map extract level2) := '(1 1)
  (def level3 (mapcat call level2))
  (map extract (map call level3)) := '(0 0 0 0) ; odd gymnastic. Why is there an extra (map call) layer? Is this correct?
  (def level4 (map call level3)) ; same here
  (every? empty? (map call level4)) := true)


(tests
  "verify `extend extract = id` for unfold"
  (def t (unfold _branching-colagebra 2))
  (extract (extend extract t)) := (extract t)
  (map (comp extract #(%)) ((extend extract t))) := (map (comp extract #(%)) (t)))



;; TODO
"
-- @'coiter' f = 'unfold' ('id' 'Control.Arrow.&&&' f)@
coiter :: Functor f => (a -> f a) -> a -> Cofree f a
coiter psi a = a :< (coiter psi <$> psi a)
"
(defn coiter [])
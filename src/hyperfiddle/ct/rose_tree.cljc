(ns hyperfiddle.ct.rose-tree
  (:refer-clojure :exclude [extend])
  (:require [hyperfiddle.ct.functor :refer [Functor fmap]]
            [hyperfiddle.ct.bifunctor :refer [Bifunctor bimap]]
            ;; [hyperfiddle.ct.applicative :as ap :refer [Applicative fapply pure]]
            [hyperfiddle.ct.comonad :refer [Comonad duplicate extract]]
            [hyperfiddle.ct.cofree :refer [ComonadCofree unwrap tree-seqT]]
            [hyperfiddle.rcf :refer [tests]]
            ))

(defrecord RoseTree [a children]
  Functor
  (-fmap [Fa f] (RoseTree. (f a) (fmap (fmap f) children)))
  ;; Applicative
  ;; (-pure [_ a] (RoseTree. a []))
  ;; (-fapply [Ff Fa]
  ;;   (let [b (:a Fa)
  ;;         kids (:children Fa)]
  ;;     (ap/with-context Ff
  ;;       (RoseTree. (a b) (mappend (fmap (fmap a) kids)
  ;;                          (fmap #(fapply % Fa) children))))))
  ;; Note this can be made an applicative or monad but it's not necessarily useful.
  ;; fapply and bind is "grafting" onto the tree and produces uncommon shapes (think cartesian products).
  Bifunctor
  (-bimap [_ f g] (RoseTree. (f a) (g children)))
  Comonad
  (-extract [_] a)
  (-duplicate [Wa] (RoseTree. Wa (fmap duplicate children)))
  (-extend [Wa f] (fmap f (duplicate Wa)))
  ComonadCofree
  (-unwrap [fa] children)
  )

(defn rose-tree
  ([a] (partial rose-tree a))
  ([a children]
   (assert (seqable? children))
   (->RoseTree a children)))

(defn mapHead ":: (a -> a) -> RoseTree a [a] -> RoseTree a [a]. Apply a function to the current Rose node.
  Different from fmap. fmap applies a function onto all nodes of a tree."
  [f rose]
  (rose-tree (f (extract rose)) (unwrap rose)))

(defn mapTail ":: (a -> a) -> RoseTree a [a] -> RoseTree a [a]. Apply a function to direct children of Rose node.
  Different from fmap. fmap applies a function onto all nodes of a tree. "
  [f rose]
  (rose-tree (extract rose) (fmap #(mapHead f %) (unwrap rose))))

;; (fapply (rose-tree inc [(rose-tree str [])]) (rose-tree 1 [(rose-tree 2 []) (rose-tree 3 [])]))

;; -- | Unfold a cofree comonad from a seed.
;; unfold :: Functor f => (b -> (a, f b)) -> b -> Cofree f a
;; unfold f c = case f c of
;;   (x, d) -> x :< fmap (unfold f) d
(defn unfold "Build a RoseTree from a `coalgebra` and a `seed`.
  `coalgebra` must return a pair of a value `a` and a seqable container of next seeds.

  unfold :: Functor f => (seed -> [a, f seed]) -> seed -> RoseTree a (f seed)
"
  ([coalgebra] (partial unfold coalgebra))
  ([coalgebra seed]
   (when seed
     (let [[a fb] (coalgebra seed)]
       (assert (satisfies? Functor fb) (str "Unfolding " seed " produced " [a fb] " and the right part is not a Functor."))
       (rose-tree a (fmap (unfold coalgebra) fb))))))

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


(tests
  (def _binary_int_tree (coiter (fn [x] (if (<= x 2) [(inc x) (inc (inc x))] [])) 0))

  ((fn rec [rose]
     (when rose
       [(extract rose) (fmap rec (unwrap rose))]))
   _binary_int_tree)
  := '[0
       ([1
         ([2
           ([3 []]
            [4 []])]
          [3 []])]
        [2
         ([3 []]
          [4 []])])]

  (->> _binary_int_tree
    (tree-seq (comp not-empty unwrap) unwrap)
    (map extract))
  := [0 1 2 3 4 3 2 3 4]

  (->> (tree-seqT _binary_int_tree)
    (map extract))
  := [0 1 2 3 4 3 2 3 4]

  (extract _binary_int_tree)
  := 0
  (map extract (unwrap _binary_int_tree))
  := [1 2]

  (->> (unwrap _binary_int_tree)
    (mapcat unwrap)
    (map extract))

  := [2 3 3 4]

  (extract (mapHead inc _binary_int_tree ))
  := 1
  (map extract (unwrap (mapHead inc _binary_int_tree)))

  := [1 2]

  (extract (mapTail inc _binary_int_tree))
  := 0

  (map extract (unwrap (mapTail inc _binary_int_tree)))
  := [2 3]

  )
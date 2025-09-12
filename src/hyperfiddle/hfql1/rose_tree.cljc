(ns hyperfiddle.hfql1.rose-tree
  (:refer-clojure :exclude [extend])
  (:require [hyperfiddle.hfql1.functor :refer [Functor fmap]]
            ;; [hyperfiddle.hfql1.applicative :as ap :refer [Applicative fapply pure]]
            [hyperfiddle.hfql1.comonad :refer [Comonad duplicate]]
            [hyperfiddle.hfql1.cofree :refer [ComonadCofree]]
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
  Comonad
  (-extract [_] a)
  (-duplicate [Wa] (RoseTree. Wa (fmap duplicate children)))
  (-extend [Wa f] (fmap f (duplicate Wa)))
  ComonadCofree
  (-unwrap [_] children))

(defn rose-tree
  ([a] (partial rose-tree a))
  ([a children]
   (assert (seqable? children))
   (->RoseTree a children)))

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
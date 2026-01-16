(ns hyperfiddle.ct.either
  "The Either type represents values with two possibilities: a value of type Either a b is either Left a or Right b.
  The Either type is sometimes used to represent a value which is either correct
  or an error; by convention, the Left constructor is used to hold an error
  value and the Right constructor is used to hold a correct value (mnemonic: \"right\" also means \"correct\").
  https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Data-Either.html

  Either is the minimal definition of a coproduct. The categorical dual of a
  product (e.g. Pair). Pair is a×b, Either is a|b. Isomorphically:
   - a × b ⇔ a ∧ b
   - a | b ⇔ a ∨ b – note this is OR, not XOR¹.

  ¹ Either A B is inhabited whenever A is inhabited or B is inhabited, even if
  both are inhabited. This is a common misconception: values vs propositions. An
  *instance* of Either is always either Left a or Right b (constructors are
  disjoint), so a single value cannot be both at once. But the *proposition* A ∨ B
  means: you have evidence for A or evidence for B (and you can indicate
  which). XOR is (A ∨ B) ∧ ¬(A ∧ B), which requires additionally proving that A
  and B cannot both hold. 'Picking a side' at construction time does not provide
  that impossibility proof.
  "
  (:require
   [hyperfiddle.ct.function :refer [const]]
   [hyperfiddle.ct.semigroup :refer [Semigroup <>]]
   [hyperfiddle.ct.functor :refer [Functor fmap]]
   [hyperfiddle.ct.bifunctor :as bf :refer [Bifunctor bimap]]
   [hyperfiddle.ct.applicative :refer [Applicative fapply]]
   [hyperfiddle.ct.monad :refer [Monad bind return valid-bind-return-value?]]
   [hyperfiddle.ct.maybe :refer [map-maybe just nothing]]
   [hyperfiddle.ct.pair :refer [pair]]
   [hyperfiddle.rcf :refer [tests]]
   ))

(defprotocol Either) ; marker protocol

(defn either? [x] (satisfies? Either x)) ; satisfies here isn't slow because Either can only be Left or Right and both implement the protocol directly.

(declare either semigroup-mappend pure-either fapply-either monad-bind)

(defrecord Right [b]
  Either
  Semigroup ; lawful semigroup impl that doesn't require wrapped values to themeselves be semigroup.
  (-<> [either-a either-b] (semigroup-mappend either-a either-b))
  Functor
  (-fmap [_this f] (->Right (f b)))
  Bifunctor
  (-bimap [_this _f g] (->Right (g b)))
  Applicative
  (-pure [_ a] (pure-either a))
  (-fapply [either-f either-v] (fapply-either either-f either-v))
  Monad
  (-bind [ma a->mb] (monad-bind ma a->mb)))

(defn right ":: b -> Right b" [b] (->Right b))
(defn right? "right? :: Either a b -> Bool" [either] (instance? Right either)) ; isRight in haskell
(defn from-right ":: b -> Either a b -> b. Return the contents of a Right-value or a default value otherwise."
  [b either] (if (right? either) (.-b either) b))
(defn rights ":: [Either a b] -> [b]. Extracts from a list of Either all the Right elements. All the Right elements are extracted in order."
  [eithers]
  (map-maybe (either (const nothing) just eithers)))

(tests
  (either? (right 1))
  := true
  (right? (right 1))
  := true
  (rights [(right 1) (right 2)])
  := [1 2]
  )

(defrecord Left [a]
  Either
  Semigroup ; lawful semigroup impl that doesn't require wrapped values to themeselves be semigroup.
  (-<> [either-a either-b] (semigroup-mappend either-a either-b))
  Functor
  (-fmap [this _f] this)
  Bifunctor
  (-bimap [_this f _g] (->Left (f a)))
  Applicative
  (-pure [_ a] (pure-either a))
  (-fapply [either-f either-v] (fapply-either either-f either-v))
  Monad
  (-bind [ma a->mb] (monad-bind ma a->mb))
  )

(defn left ":: a -> Left a" [a] (->Left a))
(defn left? "left? :: Either a b -> Bool" [either] (instance? Left either)) ; isLeft in haskell
(defn from-left ":: a -> Either a b -> a. Return the contents of a Left-value or a default value otherwise." [a either]
  (if (left? either) (.-a either) a))
(defn lefts ":: [Either a b] -> [a]. Extracts from a list of Either all the Left elements. All the Left elements are extracted in order."
  [eithers]
  (map-maybe (either just (const nothing) eithers)))

(tests
  (either? (left 1))
  := true
  (left? (left 1))
  := true
  (lefts [(left 1) (left 2)])
  := [1 2]
  )

(tests
  (lefts [(left 1) (right 2) (left 3) (right 4)])
  := [1 3]
  (rights [(left 1) (right 2) (left 3) (right 4)])
  := [2 4]
  )

(defn either ":: (a -> c) -> (b -> c) -> Either a b -> c.
  Not a constructor. Use `left` or `right`. Case analysis for the Either type.
  If the value is Left a, apply the first function to a; if it is Right b, apply
  the second function to b. "
  ([a->c] (partial either a->c))
  ([a->c b->c] (partial either a->c b->c))
  ([a->c b->c either-a-b]
   {:pre [(either? either-a-b)]}
   (cond
     (left? either-a-b) (a->c (from-left nil either-a-b))
     (right? either-a-b) (b->c (from-right nil either-a-b))
     ;; in case :pre condition is disabled
     :else (throw (ex-info (str "Object is neither Left nor Right. Type: " (type either-a-b) " .") {:either-a-b either-a-b})))))


(defn partition-either ":: [Either a b] -> ([a], [b]). Partitions a list of Either into two lists. All
  the Left elements are extracted, in order, to the first component of the
  output. Similarly the Right elements are extracted to the second component of
  the output. "
 [eithers]
  (reduce
    (fn [pair either-a-b]
      (either
        (fn [a] (bf/first #(conj % a) pair))
        (fn [b] (bf/second #(conj % b) pair))
        either-a-b))
    (pair [] [])
    eithers))


(tests
  (partition-either [(left 1) (right 2) (left 3) (right 4)])
  )

(defn- semigroup-mappend [either-a either-b] (either (const either-b) (const either-a) either-a))

(tests
  "Semigroup"
  (<> (right 1) (right 2))
  := (right 1)

  (<> (right 1) (left 2))
  := (right 1)

  (<> (left 1) (right 2))
  := (right 2)

  (<> (left 1) (left 2))
  := (left 2)
  )

(tests
  "Functor"
  (fmap inc (left 1)) := (left 1)
  (fmap inc (right 1)) := (right 2)
  )

(tests
  "Bifunctor"
  (map (bimap inc inc) [(left 1) (right 5)])
  := [(left 2) (right 6)]
  )

;;; Applicative
(defn- pure-either [a] (right a))
(defn- fapply-either [either-f either-v]
  (either
    (fn [a] (left a))
    (fn [b] (fmap b either-v))
    either-f))


(tests
  "Applicative"
  (fapply (right inc) (right 1))
  := (right 2)

  (fapply (right inc) (left 1))
  := (left 1)

  (fapply (left inc) (right 1))
  := (left inc)

  (fapply (left inc) (left 1))
  := (left inc)
  )

(defn- monad-bind [mb b->mb]
  (either
    (const mb)
    (fn [b] (b->mb b))
    mb))

(defmethod valid-bind-return-value? Left [_a b] (either? b))
(defmethod valid-bind-return-value? Right [_a b] (either? b))

(tests
  (bind (left 1) (fn [_] (throw (ex-info "should short-circuit" {}))))
  := (left 1)

  (bind (right 1) (comp return inc))
  := (right 2)

  (bind (right 1) (fn [num] (if (odd? num) (left num) (right num))))
  := (left 1)

  (-> (right 1)
    (bind (fn [num] (if (odd? num) (left num) (right num))))
    (bind (comp return inc))
    )
  := (left 1)

  (-> (right 2)
    (bind (fn [num] (if (odd? num) (left num) (right num))))
    (bind (comp return inc))
    )
  := (right 3)

  )

(defn unwrap! ":: Throwable a => Either a b -> b. Given an Right b, return `b`. Given a Left a, throws a."
  [e]
  (either #(throw %) identity e))
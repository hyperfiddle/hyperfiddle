(ns hyperfiddle.hfql1.function
  "https://hackage.haskell.org/package/base-4.11.0.0/docs/Data-Function.html"
  (:require [hyperfiddle.rcf :refer [tests]]))

(def id "id :: a -> a" identity)

(defn flip
  "flip :: (a -> b -> c) -> b -> a -> c."
  ([f] (fn flip-f
         ([a] (flip f a))
         ([a b] (flip f b a))))
  ([f a] (fn flip-f-a [b] (flip f b a)))
  ([f b a] (f b a)))


(tests
  ;; discard left
  ({} 1 2) := 2
  ;; discard right
  ((flip {}) 1 2) := 1
  ;; flip ∘ flip = id
  ((flip (flip {})) 1 2)
  := ({} 1 2)
  )

(defn const "const :: a -> b -> a"
  ([a] (fn const-a [b] (const a b)))
  ([a b] ((flip {}) a b)))

(defn dot
  "(.) :: (b -> c) -> (a -> b) -> a -> c"
  ([] dot)
  ([f] (fn dot*
         ([g] (dot f g))
         ([g a] (dot f g a))))
  ([f g] (fn dot* [a] (dot f g a)))
  ([f g a] (f (g a))))

(tests
  ((((dot) inc) inc) 1) := 3)

(defn $ "($) :: (a -> b) -> a -> b. $ has low, right-associative binding precedence. aka `call`. "
  ([a] #($ % a))
  ([a->b a] (a->b a)))

(tests
  ;; call
  ($ inc 1) := (apply inc [1])
  ;; call later
  (($ 1) inc) := 2
  )

(def & "(&) :: a -> (a -> b) -> b " (flip $))

(tests
  ;; call later
  ((& 1) inc) := 2
  ;; reversed call
  (& 1 inc) := 2
  ;; chain
  (-> 1 (& inc) (& inc)) := 3
  )

(defn fix "fix :: (a -> a) -> a. Eager!" [f]
  (letfn [(g [x] ((f g) x))]
    g))

(tests
  (def factorial
    (fix (fn [rec]
           (fn [n] (if (zero? n) 1 (* n (rec (dec n))))))))
  (factorial 5)
  := 120
  )

;; (def on) ; not implemented
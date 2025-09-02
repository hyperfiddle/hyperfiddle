(ns hyperfiddle.hfql1.function
  "https://hackage.haskell.org/package/base-4.11.0.0/docs/Data-Function.html")

(def id "id :: a -> a" identity)

(defn flip
  ([f] (fn [a b] (f b a)))
  ([f a] (fn [b] (f b a))))

(defn const "const :: a -> b -> a"
  ([a] (partial const a))
  ([a b] ((flip {}) a b)))

(def dot "(.) :: (b -> c) -> (a -> b) -> a -> c" (partial partial comp))

(defn $ "($) :: (a -> b) -> a -> b. $ has low, right-associative binding precedence"
  ([a] #($ % a))
  ([a->b a] (a->b a)))

(def & "(&) :: a -> (a -> b) -> b " (flip $))

(comment
  (-> 1 (& inc) (& inc)) := 3
  )

(defn fix "fix :: (a -> a) -> a. Eager!" [f]
  (letfn [(g [& args] (apply (f g) args))]
    g))

(comment
  (def factorial
    (fix (fn [rec]
           (fn [n] (if (zero? n) 1 (* n (rec (dec n))))))))
  (factorial 5)
  := 120
  )


;; (def on) ; not implemented
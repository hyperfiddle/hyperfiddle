(ns hyperfiddle.ct.bifunctor
  "https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Bifunctor.html"
  (:refer-clojure :exclude [first second #?(:cljs -first) #?(:cljs -second)])
  (:require [hyperfiddle.ct.function :refer [id]]
            [hyperfiddle.ct.functor :refer [Functor fmap]]
            [hyperfiddle.rcf :refer [tests]])
  #?(:cljs (:require-macros [hyperfiddle.ct.bifunctor :refer [satisfies?-mem]])))

(defprotocol Bifunctor
  (-bimap [p f g] "bimap :: (a -> b) -> (c -> d) -> p a c -> p b d"))

(defprotocol BifunctorFirstSecond
  "Optional: implement only if you choose to define `first` and `second` instead of `bimap`."
  (-first [p f] "first :: (a -> b) -> p a c -> p b c")
  (-second [p f] "second :: (a -> b) -> p a b -> p a c"))

(defmacro ^:private satisfies?-mem [protocol] ; has to be a macro because cljs satisfies? is a macro.
  `(let [!state# (atom {})] ; we don't expect this piece of state to grow up to OOM because it caches types.
     (fn [obj#]
       (let [typ# (type obj#)]
         (if-let [entry# (find @!state# typ#)]
           (val entry#)
           (let [check# (satisfies? ~protocol obj#)]
             (swap! !state# assoc typ# check#)
             check#))))))

(def bifunctor? (satisfies?-mem Bifunctor))
(def bifunctor-first-second? (satisfies?-mem BifunctorFirstSecond))

(declare bimap)
(defn first "first :: (a -> b) -> p a c -> p b c .  Map covariantly over the first argument."
  ([f] (partial first f))
  ([f p]
   (if (bifunctor-first-second? p)
     (-first p f)
     (bimap f id p))))

(defn second "second :: (a -> b) -> p a b -> p a c . Map covariantly over the second argument."
  ([f] (partial second f))
  ([f p]
   (if (bifunctor-first-second? p)
     (-second p f)
     (bimap id f p))))

(defn bimap "bimap :: (a -> b) -> (c -> d) -> p a c -> p b d . Map over both arguments at the same time."
  ([f] (partial bimap f))
  ([f g] (partial bimap f g))
  ([f g p]
   (if (bifunctor? p)
     (-bimap p f g)
     (if (bifunctor-first-second? p)
       (first f (second g p))
       (throw (ex-info "Object is not an instance of Bifunctor" {:object p}))))))

(extend-protocol Bifunctor
  #?(:clj clojure.lang.Fn, :cljs function)
  (-bimap [h f g] (fn [a b] (h (f a) (g b))))
  #?(:clj clojure.lang.AMapEntry :cljs MapEntry)
  (-bimap [entry f g] (clojure.core/first {(f (key entry)) (g (val entry))})))

;; minimal impl for tests
(defn- pair [a b] (clojure.core/first {a b})
  #_(reify
    #?@(:clj [java.util.Map$Entry
              (getKey [_] a)
              (getValue [_] b)]
        :cljs [IMapEntry
               (-key [_] a)
               (-val [_] b)])
    Functor
    (-fmap [_ f] (pair a (f b)))
    Bifunctor
    (-bimap [h f g] (pair (f (key h)) (g (val h))))
    #?@(:clj [Object
              (equals [_ other] (and (= a (key other)) (= b (val other))))
              (hashCode [_] (hash [a b]))]
        :cljs [IEquiv
               (-equiv [_ other] (and (= a (key other)) (= b (val other))))])))

(tests
  "second ≡ fmap"
  (second inc (pair nil 1))
  := (fmap inc (pair nil 1)))

(tests
  "second id ≡ id"
  (second id (pair nil 1))
  := (pair nil 1))

(tests
  "bimap id id ≡ id"
  (bimap id id (pair 1 2))
  := (pair 1 2))

(tests
  "first id ≡ id, second id ≡ id"
  (first id (pair "a" 1))
  := (pair "a" 1)

  (second id (pair "a" 1))
  := (pair "a" 1))

(tests
  "bimap f g ≡ first f ∘ second g"
  (bimap inc str (pair 1 2))
  := (first inc (second str (pair 1 2))))

(tests
  "bimap (f ∘ g) (h ∘ i) ≡ bimap f h ∘ bimap g i"
  (bimap (comp str inc) (comp str inc) (pair 1 2))
  := (bimap str str (bimap inc inc (pair 1 2))))

(tests
  "first (f ∘ g) ≡ first f ∘ first g"
  (first (comp str inc) (pair 1 2))
  := (first str (first inc (pair 1 2))))

(tests
  "second (f ∘ g) ≡ second f ∘ second g"
  (second (comp str inc) (pair 1 2))
  := (second str (second inc (pair 1 2))))



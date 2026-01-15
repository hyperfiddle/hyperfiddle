(ns hyperfiddle.ct.maybe
  "https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Maybe.html"
  (:require [hyperfiddle.ct.functor :refer [Functor]]))

(defprotocol Maybe)

(defrecord Just [a]
  Maybe
  Functor
  (-fmap [_ f] (Just. (f a))))

(defrecord Nothing []
  Maybe
  Functor
  (-fmap [fa _] fa))

(defn just ":: a -> Maybe a" [a] (->Just a))
(def ^:const nothing ":: Nothing" (->Nothing))

(defn just? ":: Maybe a → Bool" [object] (instance? Just object))
(defn nothing? ":: Maybe a → Bool" [object] (instance? Nothing object))
(defn- maybe? [object] (satisfies? Maybe object))

(defn from-just ":: Maybe a → a" [maybe-a]
  (assert (just? maybe-a))
  (.-a ^Just maybe-a))

(defn maybe ":: b → (a → b) → Maybe a → b"
  ([default] (partial maybe default))
  ([default f] (partial maybe default f))
  ([default f maybe-a]
   (assert (maybe? maybe-a))
   (if (just? maybe-a)
     (f (from-just maybe-a))
     default)))

(defn from-maybe ":: a → Maybe a → a"
  ([default] (partial from-maybe default))
  ([default maybe-a]
   (maybe default identity maybe-a)))

(defn list-to-maybe ":: [a] → Maybe a" [list-a]
  (if (empty? list-a)
    nothing
    (just (first list-a))))

(def maybe-to-list ":: Maybe a → [a]" (maybe (list) list))

(defn map-maybe ":: (a → Maybe b) → [a] → [b]" ; TODO generalize to Filterable so to preserve the input collection type.
  ([f] (partial map-maybe f))
  ([f as]
   (keep
     (fn [a]
       (let [maybe-a (f a)]
         (if (just? maybe-a)
           (from-just maybe-a)
           nil)))
     as)))

(defn cat-maybes ":: [Maybe a] → [a]" [as] (map-maybe identity as))
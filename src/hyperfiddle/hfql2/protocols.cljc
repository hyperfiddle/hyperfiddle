(ns hyperfiddle.hfql2.protocols
  (:require [clojure.datafy]))

(defprotocol Identifiable
  :extend-via-metadata true
  (identify [o]))

(defmulti resolve-dispatch type)
(defmulti ^{:arglists '([object])} hfql-resolve resolve-dispatch)
(defmethod hfql-resolve :default [object] object) ; don't know how to resolve ≡ identity

(defprotocol Navigable
  :extend-via-metadata true
  "Exact same semantics as `clojure.core.protocols/Navigable`. Default implementation delegates to `ccp/Navigable`.
Alice for an alternative Navigable implementation without damaging an eventually existing `ccp/Navigable` implementation."
  (nav [coll k v] "return (possibly transformed) v in the context of coll and k (a key/index or nil), defaults to returning v."))

(extend-protocol Navigable
  #?(:clj Object, :cljs default)
  (nav [coll k v] (clojure.datafy/nav coll k v)))

(defprotocol Suggestable
  :extend-via-metadata true
  (suggest [o]))

(defprotocol ComparableRepresentation
  (comparable [this] "Return a representation of Object suitable for `compare`."))
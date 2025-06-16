(ns hyperfiddle.api
  (:refer-clojure :exclude [resolve])
  (:require [hyperfiddle.electric3 :as e]
            [clojure.core.protocols :as ccp]
            [hyperfiddle.nav0] ; protocols only
            ))

(def ^:depreacted ^:dynamic *$*) ; legacy
(e/declare ^:dynamic *exports*)
(e/declare ^:dynamic *bindings*)

(def ^:dynamic *hierarchy* #?(:clj @#'clojure.core/global-hierarchy :cljs (cljs.core/get-global-hierarchy)))

(defmulti -resolve (fn ([object] (type object)) ([object opts] (type object))) :hierarchy #'*hierarchy*)
(defmethod -resolve :default [_object & _opts] nil)

(defn resolve
  ([object] (-resolve object))
  ([object {::keys [hierarchy] :as opts}]
   (binding [*hierarchy* (or hierarchy *hierarchy*)]
     (-resolve object (dissoc opts ::hierarchy)))))

(defmulti -pretty-print (fn ([object] (type object)) ([object opts] (type object))) :hierarchy #'*hierarchy*)
(defmethod -pretty-print :default [object & _opts] (pr-str object))

(defn pretty-print
  ([object] (pretty-print object nil))
  ([object {::keys [hierarchy] :as opts}]
   (binding [*hierarchy* (or hierarchy *hierarchy*)]
     (-pretty-print object (dissoc opts ::hierarchy)))))

(defn navigable-indexed [nav-fn collection]
  (vary-meta collection assoc `ccp/nav (#?(:clj bound-fn :cljs fn) [_coll key value] (nav-fn key value))))

(defn navigable [nav-fn collection] (navigable-indexed (fn [_index value] (nav-fn value)) collection))

(defn identifiable [identify-fn object]
  (vary-meta object assoc `hyperfiddle.nav0/-identify (#?(:clj bound-fn* :cljs identity) identify-fn)))


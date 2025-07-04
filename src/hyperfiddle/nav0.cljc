(ns hyperfiddle.nav0
  (:refer-clojure :exclude [resolve])
  (:require [clojure.core.protocols :as ccp]
            [hyperfiddle.rcf :refer [tests]]))

(defprotocol Identifiable :extend-via-metadata true
  (-identify [o])) ; local-symbolize

(extend-protocol Identifiable
  #?(:clj Object :cljs default) (-identify [_])
  nil (-identify [_]))

(defn identifiable [identify-fn object]
  (vary-meta object assoc `-identify (#?(:clj bound-fn* :cljs identity) identify-fn)))

(defn identify [obj] "
Produces a symbolic identity for `obj`, or nil if `obj` does not explicitly
implement `Identifiable`. Serializable and uniquely resolvable symbolic
identities should be favored.
All values being self-identical, `(or (identify x) x)` always yields a valid
identifier, though it might not be serializable."
  (-identify obj))

(comment
  (identify (Object.)) := nil
  (let [obj (Object.)] (pr-str (or (identify obj) obj))) := "#object[java.lang.Object 0x75bf525d \"java.lang.Object@75bf525d\"]"
  (identify nil) := nil
  (identify (identifiable (constantly `hash-map) {})) := `hash-map
  )

(def ^:dynamic *hierarchy* #?(:clj @#'clojure.core/global-hierarchy :cljs (cljs.core/get-global-hierarchy)))

(defmulti -resolve (fn ([object] (type object)) ([object opts] (type object))) :hierarchy #'*hierarchy*)
(defmethod -resolve :default [_object & _opts] nil)

(defn resolve
  ([object] (-resolve object))
  ([object {::keys [hierarchy] :as opts}]
   (binding [*hierarchy* (or hierarchy *hierarchy*)]
     (-resolve object (dissoc opts ::hierarchy)))))

(defprotocol NavContext
  (-nav-context [o]))

(extend-protocol NavContext
  nil (-nav-context [_])
  #?(:clj Object, :cljs default) (-nav-context [_] {})) ; https://github.com/clojure/clojurescript/blob/26dea31110502ae718399868ad7385ffb8efe6f6/src/main/cljs/clojure/datafy.cljs#L25-L27

(defn nav-context "
Provide an opportunity to compute (meta (datafy x)) without touching all attributes. User
implementation should respect (= (nav-context x) (meta (datafy x))). Use case: hf-pull produces
navigable pulled maps, without touching all attributes."
  [x]
  (some-> (-nav-context x)
    (assoc :clojure.datafy/obj x)
    #?(:clj (assoc :clojure.datafy/class (-> x class .getName symbol)))))

(defn navigable-indexed [nav-fn collection]
  (vary-meta collection assoc `ccp/nav (#?(:clj bound-fn :cljs fn) [_coll key value] (nav-fn key value))))

(defn navigable [nav-fn collection] (navigable-indexed (fn [_index value] (nav-fn value)) collection))

(comment
  (nav-context nil) := nil
  (nav-context java.lang.String)
  := {:clojure.datafy/obj java.lang.String, :clojure.datafy/class java.lang.Class})


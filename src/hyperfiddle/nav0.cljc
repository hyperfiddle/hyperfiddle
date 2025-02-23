(ns hyperfiddle.nav0
  (:require [hyperfiddle.rcf :refer [tests]]))

(defprotocol Identifiable :extend-via-metadata true
  (-identify [o])) ; local-symbolize

(defprotocol NavContext
  (-nav-context [o]))

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
  )

(extend-protocol Identifiable
  #?(:clj Object :cljs default) (-identify [_])
  nil (-identify [_]))

(defn nav-context "
Provide an opportunity to compute (meta (datafy x)) without touching all attributes. User
implementation should respect (= (nav-context x) (meta (datafy x))). Use case: hf-pull produces
navigable pulled maps, without touching all attributes."
  [x]
  (some-> (-nav-context x)
    (assoc :clojure.datafy/obj x)
    #?(:clj (assoc :clojure.datafy/class (-> x class .getName symbol))))) ; https://github.com/clojure/clojurescript/blob/26dea31110502ae718399868ad7385ffb8efe6f6/src/main/cljs/clojure/datafy.cljs#L25-L27


(extend-protocol NavContext
  nil (-nav-context [_])
  #?(:clj Object, :cljs default) (-nav-context [_] {}))

(comment
  (nav-context nil) := nil
  (nav-context java.lang.String)
  := {:clojure.datafy/obj java.lang.String, :clojure.datafy/class java.lang.Class})

(tests "insight: nav on dehydrated collection with nil key can be used to hydrate an object in context"
  (require '[clojure.datafy :refer [datafy nav]] '[dustingetz.mbrainz :refer [test-db lennon]])
  #_(clojure.repl/doc nav) ; big idea: k is optional actually!
  ; nav can use k if it helps you enrich the object but you don't have to!
  ; G: I think it was a mistake for Rich to make nav look like get and get-in
  (def xs (with-meta [123 125 lennon] ; attach polymorphic context to the resultset not the element
            {`clojure.core.protocols/nav ; polymorphic not by type but by meta
             (fn [xs k v] (d/entity @test-db v))}))
  "nav can resolve a hydrated object from a dehydrated resultset"
  (nav xs nil lennon) := (d/entity @test-db lennon))

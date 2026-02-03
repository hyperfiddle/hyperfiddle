(ns hyperfiddle.hfql2.protocols
  (:require [clojure.datafy]))

(defprotocol Identifiable
  :extend-via-metadata true
  (-identify [o]))

(defmulti -resolve-dispatch type)
(defmulti ^{:arglists '([object])} -hfql-resolve -resolve-dispatch)
(defmethod -hfql-resolve :default [object] nil)

(defprotocol Navigable
  :extend-via-metadata true
  "Same semantics as `clojure.core.protocols/Navigable`. Defaults to delegating to `ccp/Navigable`, while allowing an alternative implementation without overriding any existing `ccp/Navigable` implementation."
  (-nav [coll k v] "return (possibly transformed) v in the context of coll and k (a key/index or nil), defaults to returning v."))

(extend-protocol Navigable
  #?(:clj Object, :cljs default)
  (-nav [coll k v] (clojure.datafy/nav coll k v)))

(defn nav [object k v] (-nav object k v)) ; useful to have it here for ns circular deps. Should also be exposed from hfql2 main ns.

(defprotocol Datafiable
  :extend-via-metadata true
  "Same semantics as `clojure.core.protocols/Datafiable`. Teaches HFQL how to handle foreign objects. E.g. how to operate over mutable objects like iterators. Does NOT fall back to `ccp/Datafiable`.
  Real-world use case: iterators and enumerations (java.util.Enumeration) are mutable and forward-only. HFQL cannot treat them as regular seqs, looking ahead at the first item in the collection and infer columns without advancing the iterator pointer.
  Seqing the iterator again doesn't rewind it. Therefore HFQL must operate on a seqed version from the start, while retaining the ability:
    - to display \"#Iterator\" in the corresponding cell
    - passing the raw iterator to downstream traversals
    - pulling on the RAW object, not on the seqed variant.
  Many Java objects are iterator-like without implementing Iterator or Iterable. E.g. java.sql.ResultSet ."
  (-datafy [o] "return a representation of o as data (default identity)."))

(defmulti --datafy "A multimethod extension point for `datafy`. Called before protocol dispatch.
  Use this to disambiguate objects matching multiple Datafiable implementations
  or override one. E.g. ZipFile$ZipEntryIterator is both Iterator and
  Enumeration. Protocol dispatch will arbitrarily pick an implementation.
  This multimethod provide the ability to `prefer-method`."
  type)

(defn datafy
  "Attempts to return x as data. If the value has been transformed and the result supports
  metadata, :hyperfiddle.hfql2.protocols/obj will be set on the metadata to the
  original value of x."
  [x]
  (let [v (--datafy x)]
    (if (identical? v x)
      v
      (if (instance? #?(:clj clojure.lang.IObj :cljs IMeta) v)
        (vary-meta v assoc ::obj x)
        v))))

(extend-protocol Datafiable
  #?(:clj Object, :cljs default)
  (-datafy [o] o)
  nil
  (-datafy [o] o))

(defprotocol Suggestable
  :extend-via-metadata true
  (-suggest [o]))

(defprotocol ComparableRepresentation
  (-comparable [this] "Return a representation of Object suitable for `compare`."))

;; Safe search protocol - prevents blowup on infinite lazy seqs and broken toString impls
(defprotocol Searchable
  :extend-via-metadata true
  (-search-matches [this needle]
    "Return true if this value matches needle.
     Collection implementations should call `search-matches` for recursion."))

(def ^:dynamic *search-depth* 0)
(def ^:dynamic *search-max-depth* 3) ; Default to 3 because in [{:name "Alice"} …], "Alice" is at depth 3. 0: […], 1: {…}, 2: :name and "Alice"
(def ^:dynamic *search-max-collection-size* 100) ; Collections will be scanned up to 100 items.
(def ^:dynamic *search-max-str-len* 200) ; Only match on the first X chars of a string.

(defn search-matches
  "Entry point for safe search. Tracks depth via dynamic var.
   Safe against infinite lazy seqs, broken toString, and deeply nested structures."
  [obj needle]
  (boolean
    (when (< *search-depth* *search-max-depth*)
      (binding [*search-depth* (inc *search-depth*)]
        (-search-matches obj needle)))))
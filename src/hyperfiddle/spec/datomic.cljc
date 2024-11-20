(ns hyperfiddle.spec.datomic ; Rosie1
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [contrib.data :refer [update-existing]]
            #_[taoensso.timbre :as timbre]))

(def dbid {:db/ident       :db/id
           :db/cardinality :db.cardinality/one
           :db/valueType   :db.type/long})

(declare attr)

(defn isComponent [this a]
  ;; follows Datomic naming case conventions
  (get (attr this a) :db/isComponent))

(defn attr-unreverse [a]
  {:pre [(-> (name a) (subs 0 1) (= "_"))]}
  (keyword (namespace a) (-> (name a) (subs 1))))

(defn make-reverse-attr [schema a]
  ;; unique scalars can't be pulled in reverse
  ;; unique ref doesn't imply that _no other attr_ points to the entityvalue
  ;; isComponent implies a 1-1 relationship, so the reverse of an isComponent attr will be cardinality-one
  {:db/ident       a
   :db/valueType   :db.type/ref
   :db/cardinality (if (isComponent schema (attr-unreverse a))
                     :db.cardinality/one
                     :db.cardinality/many)})

(defn smart-lookup-ref-no-tempids
  "see `hypercrud.browser.context/smart-entity-identifier`"
  [{:keys [:db/id :db/ident] :as v}]
  (let [identity-lookup nil]
    (or #_(if (underlying-tempid ctx id) id)
        ;; the lookups are no good yet, must use the dbid (not the tempid, actions/with will handle that reversal)
        ident
        identity-lookup
        id
        (when-not (map? v) v) ;; id-scalar
        nil ;; This is an entity but you didn't pull any identity - error?
        )))

(defn ref? [x]
  ;; used as a tag for `:db.type/ref` just below
  (any? x))

(def type-of
  {`boolean? :db.type/boolean
   `double?  :db.type/double
   `float?   :db.type/float
   `decimal? :db.type/bigdec
   `inst?    :db.type/instant
   `keyword? :db.type/keyword
   `string?  :db.type/string
   `uri?     :db.type/uri
   `uuid?    :db.type/uuid
   `map?     :db.type/ref
   `ref?     :db.type/ref
   `symbol?  :db.type/symbol
   `integer? :db.type/long
   `number?  :db.type/long
   `nat-int? :db.type/long
   `int?     :db.type/long
   `pos-int? :db.type/long})

(def pred-of (set/map-invert type-of))

(defn to-spec [{:keys [db/ident db/cardinality db/valueType]}]
  (let [predicate   (pred-of valueType)]
    {:name      ident
     :type      (if predicate
                  :hyperfiddle.spec/predicate
                  (if (= :db.cardinality/many cardinality)
                    (if (= :db.type/ref valueType)
                      :hyperfiddle.spec/coll
                      :hyperfiddle.spec/keys)
                    :hyperfiddle.spec/keys))
     :predicate predicate}))

;; (to-spec {:db/ident       :dustingetz/gender
;;           :db/cardinality {:db/ident :db.cardinality/one}
;;           :db/valueType   {:db/ident :db.type/ref}})

(defn for-kv [kvs init f] (reduce-kv f init kvs))
(defn map-values [f m] (for-kv m m (fn [acc k v] (assoc acc k (f v)))))

(defn schema->spec [schema]
  (map-values to-spec schema))

(defn- most-meaningful [specs]
  (->> specs
       (map (fn [{:keys [children predicate]}]
              (cond
                (seq children) (most-meaningful children)
                predicate      predicate
                :else          nil)))
       (filter type-of)
       (first)))

(defn from-spec [attr]
  (when attr
    (let [{:keys [name type predicate children]} attr
          valueType                              (case type
                                                   :hyperfiddle.spec/keys      :db.type/ref
                                                   :hyperfiddle.spec/predicate (type-of predicate)
                                                   (:hyperfiddle.spec/coll
                                                    :hyperfiddle.spec/?
                                                    :hyperfiddle.spec/nilable) (:db/valueType (from-spec (first children)))
                                                   :hyperfiddle.spec/and       (type-of (most-meaningful children)))]
      (if-not valueType
        nil #_(timbre/debug  "Unable to infer an attr valueType from " {:name name :predicate predicate})
        {:db/ident       name
         :db/valueType   valueType
         :db/cardinality (if (= :hyperfiddle.spec/coll type) :db.cardinality/many :db.cardinality/one)}))))

(comment

  (def attr {:name :swinged.rosie.sub-req/ttstart, :type :hyperfiddle.spec/predicate, :predicate `clojure.core/integer?})
  (from-spec attr)
  (type-of `integer?)

  )

(defn spec->schema [spec]
  (map-values from-spec spec))

;; -----------------------------------------------------------------

(defn attr [{:keys [attributes]} a]
  (when a
    (s/assert keyword? a)
    (let [is-reverse-nav (-> (name a) (subs 0 1) (= "_"))]
      (cond
        (= a :db/id)   dbid
        is-reverse-nav (make-reverse-attr attributes a)
        :else
        (-> (get attributes a) ;; can be nil if UI asks for attribute that is missing from schema
            (from-spec)
            (update-existing :db/valueType smart-lookup-ref-no-tempids)
            (update-existing :db/cardinality smart-lookup-ref-no-tempids)
            (update-existing :db/isComponent smart-lookup-ref-no-tempids)
            (update-existing :db/unique smart-lookup-ref-no-tempids))))))

(def hydrate (partial map-values (fn [x]
                                   (if (keyword? x)
                                     {:db/ident x}
                                     x))))

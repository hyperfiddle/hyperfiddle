(ns dustingetz.datomic-contrib2
  (:import (datomic.query EntityMap))
  (:require [clojure.core.protocols :as ccp :refer [Datafiable Navigable]]
            [contrib.data :refer [index-by unqualify]]
            [datomic.api :as d] ; onprem only
            #_[hyperfiddle.electric :as e] ; no electric allowed to maximize reuse
            [hyperfiddle.nav0 :refer [Identifiable NavContext nav-context]]
            [hyperfiddle.rcf :refer [tests % tap]]))

(tests (require '[clojure.datafy :refer [datafy nav]]
         '[dustingetz.mbrainz :refer [test-db lennon pour-lamour yanne cobblestone]])
  (some? @test-db) := true)

(defn easy-attr [db ?a] ; better, copied from datomic-browser.datomic-model
  (when ?a
    (let [!e (d/entity db ?a)]
      [(unqualify (:db/valueType !e))
       (unqualify (:db/cardinality !e))
       (unqualify (:db/unique !e))
       (if (:db/isComponent !e) :component)])))

(tests
  (easy-attr @test-db :db/ident) := [:keyword :one :identity nil]
  (easy-attr @test-db :artist/name) := [:string :one nil nil])

;;; Entity back references

(defn reverse-attr [?kw]
  (if ?kw
    (keyword (namespace ?kw)
      (let [s (name ?kw)]
        (case (.charAt s 0)
          \_ (subs s 1)
          (str "_" s))))))

(tests
  (reverse-attr :foo/bar) := :foo/_bar
  (reverse-attr nil) := nil
  (reverse-attr :foo/_bar) := :foo/bar)

(defn reverse-attribute? [attribute]
  {:pre [(qualified-keyword? attribute)]}
  (clojure.string/starts-with? (name attribute) "_"))

(defn invert-attribute [attribute] ; fixme duplicates reverse-attr
  {:pre [(qualified-keyword? attribute)]}
  (let [nom (name attribute)]
    (keyword (namespace attribute) (if (reverse-attribute? attribute) (subs nom 1) (str "_" nom)))))

(tests
  (reverse-attribute? :foo/bar) := false
  (reverse-attribute? :foo/_bar) := true
  (invert-attribute :abstractRelease/artists) := :abstractRelease/_artists
  (invert-attribute (invert-attribute :abstractRelease/artists)) := :abstractRelease/artists)

(defn find-attr-ident [db attribute-id] ; faster than (:db/ident (d/entity …))
  (:v (first (d/datoms db :eavt attribute-id))))

(defn reverse-refs
  ([db target] (reverse-refs db target false))
  ([db target include-system-refs?]
   (->> (d/datoms db :vaet target)
     (eduction (if include-system-refs? ; single conditional check
                 (map identity) ; noop
                 (remove #(zero? ^long (:e %)))) ; should byte-compile to `==`
       (map (fn [datom] [(find-attr-ident db (:a datom)) (:e datom)]))))))

(comment (reverse-refs @test-db 527765581346058))

(defn back-references [db eid] ; optimized for speed – returns a map {:reverse/_ref #{entity entity ...}
  (as-> (reverse-refs db eid) % ; return eduction of vector pairs
    (contrib.data/group-by
      #(get % 0) ; fast `first`
      (fn [coll x] (conj! (or coll (transient #{})) (d/entity db (get x 1)))) ; fast `second`
      %)
    ;; single-pass invert-key + freeze entity sets
    (reduce-kv (fn [r k v] (-> (dissoc! r k) (assoc! (invert-attribute k) (persistent! v))))
      (transient %) %)
    (persistent! %)))

(tests
  (d/touch (d/entity @test-db yanne))
  (def !e (back-references @test-db yanne))
  ; WARNING: these are EntityMaps, NOT maps. d/touch returns native objects!
  #_{:abstractRelease/_artists #{#:db{:id 17592186058336} #:db{:id 17592186067319}},
     :release/_artists #{#:db{:id 17592186069801} #:db{:id 17592186080017}},
     :track/_artists #{#:db{:id 1059929209283807}
                       #:db{:id 862017116284124}
                       #:db{:id 862017116284125}
                       #:db{:id 1059929209283808}}}
  (map type (:abstractRelease/_artists !e)) := [EntityMap datomic.query.EntityMap]
  (map type (:release/_artists !e)) := [datomic.query.EntityMap datomic.query.EntityMap]
  (map type (:track/_artists !e)) := [datomic.query.EntityMap datomic.query.EntityMap datomic.query.EntityMap datomic.query.EntityMap])

(comment
  (back-references _db 778454232478138)
  (back-references _db (:db/id _artist_e))
  )

;;; Datafy/Nav

(defn query-schema [db]
  (d/q '[:find (pull ?attr [*
                            {:db/valueType [:db/ident]
                             :db/cardinality [:db/ident]
                             :db/unique [:db/ident]}])
         :where [:db.part/db :db.install/attribute ?attr]]
    db
    {:limit -1}))

(defn index-schema [schema] (into {} (comp cat (index-by :db/ident)) schema))
(defn ref? [indexed-schema a] (= :db.type/ref (get-in indexed-schema [a :db/valueType :db/ident])))

(extend-type datomic.query.EntityMap
  Identifiable (-identify [^datomic.query.EntityMap !e] (:db/id !e))
  NavContext (-nav-context [entity] {`ccp/nav (fn [e k v] (ccp/nav entity k v))})
  Navigable
  (nav [^datomic.query.EntityMap entity k v]
    (let [[typ card unique? comp?] (easy-attr (.-db entity) k)]
      (cond
        (#{:db/id :db/ident} k) entity
        ; TODO cache schema?
        (and (keyword? v) (ref? (index-schema (query-schema (.-db entity))) k)) (d/entity (.-db entity) v) ; traverse ident refs
        (= :identity unique?) (if (instance? datomic.query.EntityMap v) v (d/entity (.-db entity) [k v])) ; resolve lookup ref, todo cleanup
        () (k entity v) ; traverse refs or return value
        )))
  Datafiable
  (datafy [^datomic.query.EntityMap entity]
    (let [db (.-db entity)]
      (-> {:db/id (:db/id entity)}
        (into (d/touch entity))
        (into (back-references db (:db/id entity))) ; G: not more expansive than d/touch - heavily optimized.
        (with-meta (nav-context entity))
        ))))

(comment (d/entity @test-db [:abstractRelease/gid #uuid "320eeca0-a5ff-383f-a66e-b2f559ed0ab6"]))

;; Patch EntityMap printing to differentiate it from regular maps
;; (defonce original-entity-map-print-method (get-method print-method datomic.query.EntityMap))
;; (defmethod print-method datomic.query.EntityMap [e writer]
;;   (.write writer "#datomic.query.EntityMap ")
;;   (binding [*print-namespace-maps* false]
;;     (original-entity-map-print-method e writer)))

#_ ; BAD FUNCTION, leaving this tombstone to warn the next confused person
(defn untouch-refs [indexed-schema touched-entity] ; only touched attrs are present
  (letfn [(untouch-ref [{id :db/id}] (d/entity (.-db touched-entity) id))] ; resolve back to underlying for nav
    (reduce-kv
      (fn [acc k v]
        (if (ref? indexed-schema k)
          (cond
            (set? v) (assoc acc k (set (map untouch-ref v)))
            (map? v) (assoc acc k (untouch-ref v))
            :else (assoc acc k v))
          (assoc acc k v)))
      {} touched-entity)))
#_(tests
  #_(def test-schema (delay (index-schema (query-schema @test-db))))
  (def !e (d/entity @test-db pour-lamour))
  (:abstractRelease/artists (d/touch !e)) ; := #{#:db{:id 778454232478138} #:db{:id 580542139477874}} -- fail bc entity not= map
  (:abstractRelease/artists (untouch-refs @test-schema (d/touch (d/entity @test-db pour-lamour))))
  (map type *1) := [datomic.query.EntityMap datomic.query.EntityMap])


(tests
  (def !pour-lamour (d/entity @test-db pour-lamour))
  (do (d/touch !pour-lamour) (datafy !pour-lamour))
  ; RCF crash on '_: java.lang.AbstractMethodError: Receiver class datomic.query.EntityMap does not define or inherit an implementation ...
  #_ {:db/id 17592186058336,
      :abstractRelease/gid #uuid "f05a1be3-e383-4cd4-ad2a-150ae118f622",
      :abstractRelease/name "Pour l’amour des sous / Parle au patron, ma tête est malade",
      :abstractRelease/type :release.type/single,
      :abstractRelease/artists _ ; #{datomic.query.EntityMap datomic.query.EntityMap}
      :abstractRelease/artistCredit "Jean Yanne & Michel Magne"}

  "datomic presents deep refs as native entity, NOT maps" ; WARNING: EntityMap prints as {:db/id 1}, which is incredibly confusing.
  (map type (:abstractRelease/artists (d/touch !pour-lamour))) := [datomic.query.EntityMap datomic.query.EntityMap]

  "datafy presents deep refs as native entity NOT maps NOT scalars"
  (map type (:abstractRelease/artists (datafy !pour-lamour))) := [datomic.query.EntityMap datomic.query.EntityMap]

  (tests "self-nav resolves the original underlying reference"
    (let [x (as-> (datafy !pour-lamour) x (nav x :db/id (:db/id x)))]
      (type x) := datomic.query.EntityMap
      (= !pour-lamour x) := true))

  (as-> (datafy !pour-lamour) x
    (nav x :abstractRelease/artists (:abstractRelease/artists x))
    (map type x)) := [datomic.query.EntityMap datomic.query.EntityMap] ; prints as #{#:db{:id 778454232478138} #:db{:id 580542139477874}}

  (comment
    (query-schema _db)
    (def _schema (index-schema (query-schema _db)))
    (ref? _schema :db/ident)
    (get-in _schema [:abstractRelease/artists])
    (get-in _schema [:artist/country])))

(tests "sanity tests / docs"
  (tests
    (def !yanne
      (as-> (datafy !pour-lamour) x
        (nav x :abstractRelease/artists (:abstractRelease/artists x)) ; entities yanne and magne
        (index-by :db/id x)
        (nav x 778454232478138 (get x 778454232478138))))

    (type !yanne) := datomic.query.EntityMap
    (:db/id !yanne) := yanne)

  (tests
    (datafy !yanne) ; RCF crashes with java.lang.AbstractMethodError, RCF bug?
    #_{:artist/sortName "Yanne, Jean",
       :artist/name "Jean Yanne",
       :artist/type :artist.type/person,
       :artist/country :country/FR,
       :artist/gid #uuid"da0c147b-2da4-4d81-818e-f2aa9be37f9e",
       :artist/startDay 18,
       :artist/endDay 23,
       :artist/startYear 1933,
       :track/_artists _ ; EntityMap -- #{#:db{:id 1059929209283807} #:db{:id 862017116284124} #:db{:id 862017116284125} #:db{:id 1059929209283808}}
       :artist/endMonth 5,
       :release/_artists _ ; EntityMap -- #{#:db{:id 17592186069801} #:db{:id 17592186080017}}
       :abstractRelease/_artists _ ; EntityMap -- #{#:db{:id 17592186058336} #:db{:id 17592186067319}}
       :artist/endYear 2003,
       :db/id 778454232478138,
       :artist/startMonth 7,
       :artist/gender :artist.gender/male}
    (-> (datafy !yanne) :track/_artists first type) := datomic.query.EntityMap
    (-> (datafy !yanne) :track/_artists count) := 4
    (-> (datafy !yanne) :release/_artists count) := 2
    (-> (datafy !yanne) :abstractRelease/_artists count) := 2)

  (tests
    (def !france (as-> (datafy !yanne) x (nav x :artist/country (:artist/country x))))
    (datafy !france)
    #_{:db/id 17592186045645,
       :db/ident :country/FR,
       :country/name "France",
       :release/_country #{#:db{:id 17592186076553} ...},
       :artist/_country #{#:db{:id 765260092944782} ...},
       :label/_country #{#:db{:id 17592186068643} ...}}
    (-> (datafy !france) :release/_country first type) := datomic.query.EntityMap
    (-> (datafy !france) :release/_country count) := 574
    (-> (datafy !france) :artist/_country count) := 140
    (-> (datafy !france) :label/_country count) := 59))

(defn query-stats-as-meta [d-query-result]
  (if-let [ret (:ret d-query-result)]
    (with-meta ret (dissoc d-query-result :ret))
    d-query-result))
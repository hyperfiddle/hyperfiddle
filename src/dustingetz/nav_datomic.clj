(ns dustingetz.nav-datomic
  "Pure Clojure layer for Datomic navigation — query functions, protocol
   extensions, resolvers, and a stripped sitemap. No Electric dependencies.

   Three-layer architecture:
     datomic_contrib2.clj — stateless fns, EntityMap protocol extensions
     nav_datomic.clj      — dynamic vars, query fns, Datum/Db extensions, resolvers, sitemap
     datomic_browser2.cljc — Electric UI: tooltips, renderers, entrypoints"
  (:require
   [clojure.string :as str]
   [contrib.assert :refer [check]]
   [contrib.data :refer [get-with-residual-meta]]
   [datomic.api :as d]
   [datomic.lucene]
   [dustingetz.datomic-contrib2 :as dx] ; side-effect: extends EntityMap
   [dustingetz.str :refer [blank->nil pprint-str]]
   [hyperfiddle.hfql2 :as hfql :refer [hfql hfql-resolve]]
   [hyperfiddle.hfql2.protocols :refer [Identifiable -hfql-resolve Navigable Suggestable ComparableRepresentation]]
   [hyperfiddle.navigator6.rendering :as rendering]
   [hyperfiddle.navigator6.search :refer [*local-search]])
  (:import [datomic.db Datum Db]))

;; ── Dynamic vars ──────────────────────────────────────────────────
;; Bound via e/*bindings* → HFQL applies with with-bindings* when running resolvers.

(defonce ^:dynamic *uri* nil)                            ; Datomic URI (when browsing by URI)
(defonce ^:dynamic *db-name* nil)                        ; current database name
(defonce ^:dynamic *conn* nil)                           ; Datomic connection
(defonce ^:dynamic *db* nil)                             ; current database value
(defonce ^:dynamic *db-stats* nil)                       ; cached db-stats
(defonce ^:dynamic *filter-predicate* nil)               ; for d/filter
(defonce ^:dynamic *allow-listing-and-browsing-all-dbs* nil) ; security flag for multi-db browsing

;; ── Query functions ───────────────────────────────────────────────

(defn databases
  "List databases. Only meaningful when browsing by Datomic URI."
  []
  (when (some? *uri*)
    (let [database-names-list (cond
                                (= "*" (dx/datomic-uri-db-name *uri*))
                                (d/get-database-names *uri*)

                                *allow-listing-and-browsing-all-dbs*
                                (d/get-database-names (dx/set-db-name-in-datomic-uri *uri* "*"))

                                :else (list *db-name*))]
      (->> database-names-list
        (hfql/navigable (fn [_index db-name] (hfql-resolve `(d/db ~db-name))))))))

(defn attributes
  "Datomic schema, with Datomic query diagnostics"
  ([] (attributes (check *db*)))
  ([db]
   (let [x (d/query {:query '[:find [?e ...] :in $ :where [?e :db/valueType]] :args [db]
                      :io-context ::attributes, :query-stats ::attributes})
         x (get-with-residual-meta x :ret)]
     (hfql/navigable (fn [_index ?e] (d/entity db ?e)) x))))

(defn attribute-count [!e]
  (-> *db-stats* :attrs (get (:db/ident !e)) :count))

(defn indexed-attribute? [db ident]
  (true? (:db/index (dx/query-schema db ident))))

(defn fulltext-attribute? [db ident]
  (true? (:db/fulltext (dx/query-schema db ident))))

(defn fulltext-prefix-query [input]
  (some-> input (str) (datomic.lucene/escape-query) (str/replace #"^(\\\*)+" "") (blank->nil) (str "*")))

(defn attribute-detail [!e]
  (let [ident (:db/ident !e)
        search (not-empty (str/trim (str *local-search)))
        fulltext-query (fulltext-prefix-query search)
        entids (cond
                 ;; prefer fulltext search, when available
                 (and (fulltext-attribute? *db* ident) fulltext-query)
                 (d/q '[:find [?e ...] :in $ ?a ?search :where [(fulltext $ ?a ?search) [[?e]]]] *db* ident fulltext-query)
                 ;; indexed prefix search, when available
                 (indexed-attribute? *db* ident)
                 (->> (d/index-range *db* ident search nil)
                   (take-while #(if search (str/starts-with? (str (:v %)) search) true))
                   (map :e))
                 :else ; no available index
                 (->> (d/datoms *db* :aevt ident)
                   (filter #(if search (str/starts-with? (str (:v %)) search) true))
                   (map :e)))]
    (->> entids
      (hfql/filtered)
      (hfql/navigable (fn [_index ?e] (d/entity *db* ?e))))))

(defn summarize-attr [db k]
  (->> (dx/easy-attr db k) (remove nil?) (map name) (str/join " ")))

(defn summarize-attr* [?!a]
  (when ?!a (summarize-attr *db* (:db/ident ?!a))))

(defn tx-detail [!e]
  (mapcat :data (d/tx-range (d/log *conn*) (:db/id !e) (inc (:db/id !e)))))

(def entity-detail identity)
(def attribute-entity-detail identity)

(defn entity-history
  "History datoms for a Datomic entity, both inbound and outbound statements."
  [!e]
  (let [history (d/history *db*)]
    (concat
      (d/datoms history :eavt (:db/id !e !e))
      (d/datoms history :vaet (:db/id !e !e)))))

;; ── Protocol extensions ───────────────────────────────────────────

(defn- entity-exists? [db eid]
  (and (some? eid) (seq (d/datoms db :eavt eid))))

(extend-type Datum
  Identifiable
  (-identify [datum] `(datomic.db/datum ~@(dx/datom-identity datum)))
  Navigable
  (-nav [[e a v tx added] k _]
    (case k
      :e (d/entity *db* e)
      :a (d/entity *db* a)
      :v (if (= :db.type/ref (:value-type (d/attribute *db* a))) (d/entity *db* v) v)
      :tx (d/entity *db* tx)
      :added added))
  Suggestable
  (-suggest [_] (hfql [:e :a :v :tx :added]))
  ComparableRepresentation
  (-comparable [datum] (into [] datum)))

(defn db-name [db] (::db-name (meta db)))

(extend-type Db
  Identifiable
  (-identify [db] (when-let [nm (db-name db)]
                    (let [id `(d/db ~nm)
                          id (if (d/is-history db) `(d/history ~id) id)
                          id (if (d/is-filtered db) `(d/filter ~id) id)
                          id (if (d/since-t db) `(d/since ~id ~(d/since-t db)) id)
                          id (if (d/as-of-t db) `(d/as-of ~id ~(d/as-of-t db)) id)]
                      id)))
  ComparableRepresentation
  (-comparable [db] (db-name db)))

;; ── Resolvers ─────────────────────────────────────────────────────

(defmethod -hfql-resolve `d/entity [[_ eid]]
  (when (entity-exists? *db* eid)
    (d/entity *db* eid)))

(defmethod -hfql-resolve `datomic.db/datum [[_ e a serialized-v tx added]]
  (dx/resolve-datom *db* e a serialized-v tx added))

(defmethod -hfql-resolve `d/db [[_ db-name]]
  (when *uri*
    (let [datomic-uri-db-name (dx/datomic-uri-db-name *uri*)]
      (when (or (= "*" datomic-uri-db-name)
              (= db-name datomic-uri-db-name)
              *allow-listing-and-browsing-all-dbs*)
        (with-meta (d/db (d/connect (dx/set-db-name-in-datomic-uri *uri* db-name)))
          {::db-name db-name})))))

(defmethod -hfql-resolve `d/history [[_ db]] (d/history (hfql-resolve db)))
(defmethod -hfql-resolve `d/filter  [[_ db]] (d/filter (hfql-resolve db) *filter-predicate*))
(defmethod -hfql-resolve `d/since [[_ db t]] (d/since (hfql-resolve db) t))
(defmethod -hfql-resolve `d/as-of [[_ db t]] (d/as-of (hfql-resolve db) t))

;; ── Sitemap (stripped — no Electric tooltips/renderers) ───────────

(def sitemap
  {'databases
   (hfql {(databases) {* [db-name d/db-stats]}})

   'attributes
   (hfql {(attributes)
          {* [^{::hfql/link '(attribute-detail %)}
              #(:db/ident %)
              attribute-count
              summarize-attr*]}})

   'attribute-entity-detail
   (hfql {attribute-entity-detail
          [#(:db/id %)
           attribute-count
           summarize-attr*
           *]})

   'attribute-detail
   (hfql {attribute-detail
          {* [^{::hfql/link '(entity-detail %)}
              #(:db/id %)]}})

   'tx-detail
   (hfql {tx-detail
          {* [^{::hfql/link '(entity-detail :e)}
              #(:e %)
              ^{::hfql/link '(attribute-detail :a)
                ::hfql/label :db/ident}
              {:a :db/ident}
              :v]}})

   'entity-detail
   (hfql {entity-detail
          [#(:db/id %)
           *]})

   'entity-history
   (hfql {entity-history
          {* [^{::hfql/link '(entity-detail :e)}
              #(:e %)
              ^{::hfql/link '(attribute-detail :a)}
              {:a :db/ident}
              :v
              ^{::hfql/link '(tx-detail %v)}
              #(:tx %)
              :added]}})})

;; ── Rich sitemap (with generic tooltip callbacks) ───────────────
;; Same routes as `sitemap` but with ::hfql/Tooltip metadata pointing to
;; FnTooltip in hyperfiddle.navigator6.rendering. Agents bind *tooltip-fn*
;; in e/*bindings* to provide the tooltip implementation.
;; TODO: ColumnHeaderTooltip and Render need factory syntax in HFQL analyzer
;; to avoid brittle global bindings — backlogged.

(def rich-sitemap
  {'databases
   (hfql {(databases) {* [^{::hfql/link ['. [`(~'Inject ~'%v) 'attributes]]}
                           db-name d/db-stats]}})

   'attributes
   (hfql {(attributes)
          {* ^{::hfql/select '(attribute-entity-detail %)}
           [^{::hfql/link '(attribute-detail %)
              ::hfql/Tooltip `hyperfiddle.navigator6.rendering/FnTooltip}
            #(:db/ident %)
            attribute-count
            summarize-attr*]}})

   'attribute-entity-detail
   (hfql {attribute-entity-detail ^{::hfql/Tooltip `hyperfiddle.navigator6.rendering/FnTooltip}
          [#(:db/id %)
           attribute-count
           summarize-attr*
           *]})

   'attribute-detail
   (hfql {attribute-detail
          {* ^{::hfql/Tooltip `hyperfiddle.navigator6.rendering/FnTooltip}
           [^{::hfql/link '(entity-detail %)}
            #(:db/id %)]}})

   'tx-detail
   (hfql {tx-detail
          {* [^{::hfql/link '(entity-detail :e)
                ::hfql/Tooltip `hyperfiddle.navigator6.rendering/FnTooltip}
              #(:e %)
              ^{::hfql/link '(attribute-detail :a)
                ::hfql/Tooltip `hyperfiddle.navigator6.rendering/FnTooltip}
              ^{::hfql/label :db/ident}
              {:a :db/ident}
              :v]}})

   'entity-detail
   (hfql {entity-detail ^{::hfql/Tooltip `hyperfiddle.navigator6.rendering/FnTooltip}
          [#(:db/id %)
           *]})

   'entity-history
   (hfql {entity-history
          {* [^{::hfql/link '(entity-detail :e)
                ::hfql/Tooltip `hyperfiddle.navigator6.rendering/FnTooltip}
              #(:e %)
              ^{::hfql/link '(attribute-detail :a)
                ::hfql/Tooltip `hyperfiddle.navigator6.rendering/FnTooltip}
              {:a :db/ident}
              :v
              ^{::hfql/link '(tx-detail %v)
                ::hfql/Tooltip `hyperfiddle.navigator6.rendering/FnTooltip}
              #(:tx %)
              :added]}})})

;; ── Setup function (DI for agent and fiddle hosting) ────────────

(defn make-setup-fn
  "Create the multi-arity setup-fn for dependency injection.
   Zero-arity:  static bindings for root (URI, security flag, rendering config).
   One-arity:   called per Inject route segment with db-name from URL."
  [datomic-uri]
  (fn
    ([] ;; Static bindings — always active on every page
     {#'*uri* datomic-uri
      #'*allow-listing-and-browsing-all-dbs* true
      #'rendering/*server-pretty
      {datomic.query.EntityMap
       (fn [entity] (str "EntityMap[" (dx/best-human-friendly-identity entity) "]"))}
      #'rendering/*tooltip-fn*
      (fn [_entity _edge value]
        (pprint-str (into {} (d/touch value)) :print-length 10 :print-level 2))})
    ([db-name] ;; Called when route has (Inject "mbrainz-full")
     (let [uri (dx/set-db-name-in-datomic-uri datomic-uri db-name)
           conn (d/connect uri)
           db (d/db conn)]
       {#'*uri*      uri
        #'*db-name*  db-name
        #'*conn*     conn
        #'*db*       db
        #'*db-stats* (d/db-stats db)}))))

(comment
  ;; ── Demo: connect an agent with datomic browsing ──────────────
  (require '[clojure.repl.deps :refer [add-libs]])
  (add-libs '{com.datomic/peer {:mvn/version "1.0.7491"}})
  (require
    '[hyperfiddle.navigator-agent :as agent]
    '[hyperfiddle.navigator6.rendering :as rendering]
    '[dustingetz.str :refer [pprint-str]])
  (d/get-database-names "datomic:dev://localhost:4334/*")

  (def conn (d/connect "datomic:dev://localhost:4334/mbrainz-full"))
  (def my-agent
    (agent/connect! "ws://a.localhost:9090/agent"
      dustingetz.nav-datomic/rich-sitemap
      (fn []
        {#'dustingetz.nav-datomic/*db*       (d/db conn)
         #'dustingetz.nav-datomic/*conn*     conn
         #'dustingetz.nav-datomic/*db-stats* (d/db-stats (d/db conn))
         #'rendering/*server-pretty {datomic.query.EntityMap (fn [entity] (str "EntityMap[" (dx/best-human-friendly-identity entity) "]"))}
         #'rendering/*tooltip-fn* (fn [entity edge value] (pprint-str (into {} (d/touch value)) :print-length 10 :print-level 2))})))

  ;; Dynamic — multi-database with Inject route segment
  (def base-uri "datomic:dev://localhost:4334/*")
  (def my-agent
    (agent/connect! "ws://a.localhost:9090/agent"
      ;; Sitemap — databases page links through Inject to attributes
      (merge (dissoc dustingetz.nav-datomic/rich-sitemap 'databases)
        {'databases (hfql {(databases)
                           {* [^{::hfql/link ['.. [`(~'Inject ~'%v) 'attributes]]}
                               db-name
                               d/db-stats]}})})
      (fn
        ([] ;; Static bindings — always active on every page
         {#'dustingetz.nav-datomic/*uri* base-uri
          #'dustingetz.nav-datomic/*allow-listing-and-browsing-all-dbs* true
          #'rendering/*server-pretty {datomic.query.EntityMap (fn [entity] (str "EntityMap[" (dx/best-human-friendly-identity entity) "]"))}
          #'rendering/*tooltip-fn* (fn [entity edge value] (pprint-str (into {} (d/touch value)) :print-length 10 :print-level 2))})
        ([db-name] ;; Called when route has (Inject "mbrainz-full")
         (let [uri (dx/set-db-name-in-datomic-uri base-uri db-name)
               conn (d/connect uri)
               db (d/db conn)]
           {#'dustingetz.nav-datomic/*uri*      uri
            #'dustingetz.nav-datomic/*db-name*  db-name
            #'dustingetz.nav-datomic/*conn*     conn
            #'dustingetz.nav-datomic/*db*       db
            #'dustingetz.nav-datomic/*db-stats* (d/db-stats db)})))))

  ;; Browse at http://a.localhost:9090
  ;; Navigate to attributes → click attribute → entities → click entity → traverse refs

  (agent/disconnect! my-agent))

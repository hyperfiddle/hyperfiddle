(ns dustingetz.datomic-browser
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.nav0 :as hf-nav]
            [hyperfiddle.hfql0 #?(:clj :as :cljs :as-alias) hfql]
            [hyperfiddle.entity-browser4 :as entity-browser :refer [HfqlRoot]]
            [hyperfiddle.sitemap :refer [#?(:clj parse-sitemap)]]
            [hyperfiddle.router4 :as r]
            [hyperfiddle.electric-dom3 :as dom]
            [dustingetz.loader :refer [Loader]]
            [dustingetz.str :refer [pprint-str]]
            [clojure.string :as str]
            #?(:clj [datomic.api :as d])
            #?(:clj [dustingetz.datomic-contrib2 :as dx])))

(e/declare ^:dynamic *conn*)
(e/declare ^:dynamic *db*)
(e/declare ^:dynamic *db-stats*) ; shared for perfs – safe to compute only once

#?(:clj (defn attributes []
          (->> (d/query {:query '[:find [?e ...] :in $ :where [?e :db/valueType]] :args [*db*]
                         :io-context ::attributes, :query-stats ::attributes})
               (dx/query-stats-as-meta)
               (hf-nav/navigable (fn [?e] (d/entity *db* ?e))))))

#?(:clj (defn attribute-count [!e] (-> *db-stats* :attrs (get (:db/ident !e)) :count)))

#?(:clj (defn attribute-detail [a]
          (->> (d/datoms *db* :aevt a)
            (map :e)
            (hf-nav/navigable (fn [?e] (d/entity *db* ?e))))))

#?(:clj (defn summarize-attr [db k] (->> (dx/easy-attr db k) (remove nil?) (map name) (str/join " "))))
#?(:clj (defn summarize-attr* [?!a] (when ?!a (summarize-attr *db* (:db/ident ?!a)))))

#?(:clj (defn datom->map [[e a v tx added]]
          (->> {:e e, :a a, :v v, :tx tx, :added added}
            (hf-nav/identifiable hash)
            (hf-nav/navigable-indexed (fn [key value] (if (= :a key) (d/entity *db* a) value))))))

#?(:clj (defn tx-detail [e] (->> (d/tx-range (d/log *conn*) e (inc e)) (into [] (comp (mapcat :data) (map datom->map))))))

#?(:clj (defn entity-detail [e] (d/entity *db* e)))
#?(:clj (defn attribute-entity-detail [e] (d/entity *db* e)))

#?(:clj (defn entity-history [e]
          (let [history (d/history *db*)]
            (into [] (comp cat (map datom->map))
              [(d/datoms history :eavt (:db/id e e)) ; resolve both data and object repr, todo revisit
               (d/datoms history :vaet (:db/id e e))]))))

(e/defn ^::e/export EntityTooltip [?value entity props] ; FIXME props is a custom hyperfiddle deftype
  (e/server (pprint-str (d/pull *db* ['*] ?value))))

(e/defn ^::e/export SemanticTooltip [?value entity props] ; FIXME props is a custom hyperfiddle deftype
  (e/server
    (let [attribute (and props (hfql/unwrap props))] ; `and` is glitch guard, TODO remove
      (cond (= :db/id attribute) (EntityTooltip ?value entity props)
            (qualified-keyword? ?value)
            (let [[typ _ unique?] (dx/easy-attr *db* attribute)]
              (cond
                (= :db/id attribute) (EntityTooltip ?value entity props)
                (= :ref typ) (pprint-str (d/pull *db* ['*] ?value))
                (= :identity unique?) (pprint-str (d/pull *db* ['*] [attribute #_(:db/ident (d/entity db a)) ?value])) ; resolve lookup ref
                () nil))))))

(e/defn ^::e/export SummarizeDatomicAttribute [?v row props] ; FIXME props is a custom hyperfiddle deftype
  (e/server
    ((fn [attribute] (try (summarize-attr *db* attribute) (catch Throwable _))) (hfql/unwrap props))))

#?(:clj (defn safe-long [v] (if (number? v) v 1))) ; glitch guard, TODO remove
(e/defn ^::e/export EntityDbidCell [?value entity props] ; FIXME props is a custom hyperfiddle deftype
  (let [v2 (e/server (safe-long ?value))]
    (dom/span (dom/text v2 " ") (r/link ['. [`(entity-history ~v2)]] (dom/text "entity history")))))

#?(:clj (defmethod hf-nav/-resolve datomic.query.EntityMap [entity-map & _opts] (list `entity-detail (:db/id entity-map))))

#?(:clj ; list all attributes of an entity – including reverse refs.
   (extend-protocol hfql/Suggestable
     datomic.query.EntityMap
     (-suggest [entity]
       (let [attributes (cons :db/id (keys (d/touch entity)))
             reverse-refs (dx/reverse-refs (d/entity-db entity) (:db/id entity))
             reverse-attributes (->> reverse-refs (map first) (distinct) (map dx/invert-attribute))]
         (->> (concat attributes reverse-attributes)
              (mapv (fn [k] {:label k, :entry k})))))))

(e/defn ConnectDatomic [datomic-uri]
  (e/server
    (Loader #(d/connect datomic-uri)
      {:Busy (e/fn [] (dom/h1 (dom/text "Waiting for Datomic connection ...")))
       :Failed (e/fn [error]
                 (dom/h1 (dom/text "Datomic transactor not found, see Readme.md"))
                 (dom/pre (dom/text (pr-str error))))})))

#?(:clj
   (def datomic-browser-sitemap
     (parse-sitemap
       '{attributes (hfql/props [(hfql/props :db/ident {::hfql/link    (attribute-detail :db/ident)
                                                        ::hfql/Tooltip EntityTooltip})
                                 (hfql/props (attribute-count %) {::hfql/label attribute-count})
                                 (summarize-attr* %)
                                 :db/doc]
                      {::hfql/ColumnHeaderTooltip SummarizeDatomicAttribute
                       ::hfql/select              (attribute-entity-detail %)})

         (attribute-entity-detail :e) (hfql/props [(hfql/props :db/id {::hfql/Render EntityDbidCell})
                                                   (hfql/props (attribute-count %) {::hfql/label attribute-count})
                                                   (summarize-attr* %)]
                                        {::hfql/Tooltip SemanticTooltip})

         (attribute-detail :a) (hfql/props [(hfql/props :db/id {::hfql/link (entity-detail %v)})]
                                 {::hfql/ColumnHeaderTooltip SummarizeDatomicAttribute
                                  ::hfql/Tooltip             SemanticTooltip})

         (tx-detail :tx) [(hfql/props :e {::hfql/link    (entity-detail :e)
                                          ::hfql/Tooltip EntityTooltip})
                          (hfql/props {:a :db/ident} {::hfql/link    (attribute-detail %v)
                                                      ::hfql/Tooltip EntityTooltip})
                          :v]

         (entity-detail :e) (hfql/props [(hfql/props :db/id {::hfql/Render EntityDbidCell})] ; TODO want link and Tooltip instead
                              {::hfql/Tooltip SemanticTooltip})

         (entity-history :e) [:e
                              (hfql/props {:a :db/ident} {::hfql/link    (attribute-detail %v)
                                                          ::hfql/Tooltip EntityTooltip})
                              :v
                              (hfql/props :tx {::hfql/link    (tx-detail :tx)
                                               ::hfql/Tooltip EntityTooltip})
                              :added]})))

(e/defn DatomicBrowser [sitemap entrypoint conn]
  (let [db (e/server (e/Offload #(d/db conn)))
        db-stats (e/server (e/Offload #(d/db-stats db)))]
    (binding [*conn* conn
              *db* db
              *db-stats* db-stats
              e/*bindings* (e/server (merge e/*bindings* {#'*conn* conn, #'*db* db, #'*db-stats* db-stats}))
              e/*exports*  (e/exports)
              entity-browser/*server-pretty (e/server {datomic.query.EntityMap (fn [entity] (str "EntityMap" (pr-str entity)))})]
      (dom/link (dom/props {:rel :stylesheet :href "/hyperfiddle/electric-forms.css"}))
      (dom/link (dom/props {:rel :stylesheet :href "/hyperfiddle/datomic-browser.css"}))
      (HfqlRoot sitemap entrypoint))))

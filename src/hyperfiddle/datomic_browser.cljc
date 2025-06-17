(ns hyperfiddle.datomic-browser ; ? in datomic-browser.jar
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.nav0 :as hf-nav]
            [hyperfiddle.hfql0 #?(:clj :as :cljs :as-alias) hfql]
            [hyperfiddle.entity-browser4 :as entity-browser :refer [HfqlRoot]]
            [hyperfiddle.sitemap :refer [Index]]
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
          (prn 'attributes-search-string entity-browser/*search)
          (->> (d/query {:query '[:find [?e ...] :in $ :where [?e :db/valueType]] :args [*db*] :io-context ::attributes, :query-stats ::attributes})
               (dx/query-stats-as-meta)
               (hf-nav/navigable (fn [?e] (d/entity *db* ?e))))))

#?(:clj (defn attribute-count [!e] (-> *db-stats* :attrs (get (:db/ident !e)) :count)))

#?(:clj (defn attribute-detail [a]
          (->> (d/query {:query '[:find [?e ...] :in $ ?a :where [?e ?a]] :args [*db* a], :io-context ::attribute-detail, :query-stats ::attribute-detail})
               (dx/query-stats-as-meta)
               (hf-nav/navigable (fn [?e] (d/entity *db* ?e))))))

#?(:clj (defn summarize-attr [db k] (->> (dx/easy-attr db k) (remove nil?) (map name) (str/join " "))))
#?(:clj (defn summarize-attr* [?!a] (when ?!a (summarize-attr *db* (:db/ident ?!a)))))

#?(:clj (defn datom->map [[e a v tx added]]
          (->> {:e e, :a a, :v v, :tx tx, :added added}
            (hf-nav/identifiable hash)
            (hf-nav/navigable-indexed (fn [key value] (if (= :a key) (d/entity *db* a) value))))))

#?(:clj (defn tx-detail [e] (->> (d/tx-range (d/log *conn*) e (inc e)) (into [] (comp (mapcat :data) (map datom->map))))))

#?(:clj (defn entity-detail [e] (d/entity *db* e)))

#?(:clj (defn entity-history [e]
          (let [history (d/history *db*)]
            (into [] (comp cat (map datom->map))
              [(d/datoms history :eavt (:db/id e e)) ; resolve both data and object repr, todo revisit
               (d/datoms history :vaet (:db/id e e))]))))

(e/defn ^::e/export EntityTooltip [value query-result attribute] (e/server (pprint-str (d/pull *db* ['*] value))))

(e/defn ^::e/export SemanticTooltip [value entity attribute]
  (e/server
    (when-not (coll? value)
      (let [attr (and attribute (hfql/unwrap attribute)) ; `and` is glitch guard, TODO remove
            [typ _ unique?] (dx/easy-attr *db* attr)]
        (cond
          (= :db/id attr) (EntityTooltip value entity attribute)
          (= :ref typ) (pprint-str (d/pull *db* ['*] value))
          (= :identity unique?) (pprint-str (d/pull *db* ['*] [(hfql/unwrap attribute) #_(:db/ident (d/entity db a)) value])) ; resolve lookup ref
          () nil)))))

(e/defn ^::e/export SummarizeDatomicAttribute [v o attribute]
  (e/server
    ((fn [attribute] (try (summarize-attr *db* (hfql/unwrap attribute)) (catch Throwable _))) attribute)))

#?(:clj (defn safe-long [v] (if (number? v) v 1))) ; glitch guard, TODO remove
(e/defn ^::e/export EntityDbidCell [v o spec]
  (let [v2 (e/server (safe-long v))]
    (dom/span (dom/text v2 " ") (r/link ['. [`(entity-history ~v2)]] (dom/text "entity history")))))

#?(:clj (defmethod hf-nav/-resolve datomic.query.EntityMap [entity-map & _opts] (list `entity-detail (:db/id entity-map))))
#?(:clj (defmethod entity-browser/pretty-print datomic.query.EntityMap [entity-map & _opts] (str "EntityMap" (pr-str entity-map))))

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

(e/defn DatomicBrowser [sitemap entrypoint conn]
  (let [db (e/server (e/Offload #(d/db conn)))
        db-stats (e/server (e/Offload #(d/db-stats db)))]
    (binding [*conn* conn
              *db* db
              *db-stats* db-stats
              e/*bindings* (e/server (merge e/*bindings* {#'*conn* conn, #'*db* db, #'*db-stats* db-stats}))
              e/*exports*  (e/exports)]
      (dom/link (dom/props {:rel :stylesheet :href "/hyperfiddle/electric-forms.css"}))
      (dom/link (dom/props {:rel :stylesheet :href "/hyperfiddle/datomic-browser.css"}))
      (HfqlRoot sitemap entrypoint))))
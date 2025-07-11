(ns dustingetz.pg-sakila
  #_{org.postgresql/postgresql {:mvn/version "42.7.3"}
     com.github.seancorfield/next.jdbc {:mvn/version "1.3.955"}}
  (:require [clojure.core.protocols :refer [nav datafy] :rename {nav -nav, datafy -datafy}]
            [hyperfiddle.hfql0 :refer [-identify]]
            [hyperfiddle.rcf :refer [tests]]
            [next.jdbc :as jdbc]))

;; docker run -p 5432:5432 -d sakiladb/postgres:latest

(def test-config {:dbtype "postgresql"
                  :dbname "sakila"
                  :host (or (System/getenv "SAKILADB_HOST") "localhost")
                  :port 5432
                  :user "sakila"
                  :password "p_ssW0rd"})

(def test-conn (delay (jdbc/get-connection test-config)))

(tests (require '[clojure.datafy :refer [datafy nav]]
         '[hyperfiddle.hfql0 :refer [identify]]))

;; (hyperfiddle.rcf/enable!)

(tests "kick tires"
  (jdbc/execute! @test-conn ["SELECT first_name, last_name FROM actor LIMIT 3;"])
   := [#:actor{:first_name "PENELOPE", :last_name "GUINESS"}
       #:actor{:first_name "NICK", :last_name "WAHLBERG"}
       #:actor{:first_name "ED", :last_name "CHASE"}]

  (jdbc/execute! @test-conn
    ["SELECT first_name, last_name, count(*) films FROM actor AS a
     JOIN film_actor AS fa USING (actor_id)
     GROUP BY actor_id, first_name, last_name ORDER BY films DESC LIMIT 1;"])
  := [{:actor/first_name "GINA", :actor/last_name "DEGENERES", :films 42}])

(tests "next.jdbc can auto-nav simple FK joins"
  ; https://github.com/seancorfield/next-jdbc/blob/develop/doc/datafy-nav-and-schema.md
  (def x (jdbc/execute-one! @test-conn ["select * from film limit 1;"]
           {:schema {:film/language_id :language/language_id}}))
  (type x) := clojure.lang.PersistentHashMap
  (:film/language_id x) := 1 ; language is dehydrated
  (nav x :film/language_id (:film/language_id x)) ; now it's hydrated
   := #:language{:language_id 1,
                 :name "English             ",
                 :last_update #inst "2006-02-15T04:02:19.000000000-00:00"}
   )

; Next.jdbc doesn't support many-many nav through join tables: https://clojurians.slack.com/archives/C1Q164V29/p1739991789702079
; Let's do it ourselves.

(defn hydrate-language [conn id]
  (with-meta
    (jdbc/execute-one! conn ["SELECT * FROM language where language_id = ?" id])
    {`-identify :language/language_id
     `-nav (fn [o k v] v)}))

(tests
  (hydrate-language @test-conn 1)
   := #:language{:language_id 1,
                :name "English             ",
                :last_update #inst "2006-02-15T04:02:19.000000000-00:00"}
  (identify (hydrate-language @test-conn 1)) := 1)

(defn hydrate-actor [conn actor-id]
  (with-meta
    (jdbc/execute-one! conn ["select * from actor where actor_id = ?;" actor-id])
    {`-identify :actor/actor_id
     `-nav (fn [m k v] (case k v))}))

(defn actors-for-film [conn film-id]
  (with-meta
    (->> (jdbc/execute! conn ["SELECT actor.actor_id FROM actor
    LEFT OUTER JOIN film_actor ON film_actor.actor_id = actor.actor_id
    LEFT OUTER JOIN film ON film_actor.film_id = film.film_id
    WHERE film.film_id = ?" film-id])
      (mapv :actor/actor_id))
    {`-identify (fn [xs] `(actors-for-film ~film-id))
     `-nav (fn [xs k v] (hydrate-actor conn v))}))

(tests
  (count (actors-for-film @test-conn 854)) := 5
  (identify (actors-for-film @test-conn 854)) := [`actors-for-film 854])

(defn hydrate-film [conn film-id]
  (with-meta
    (-> (jdbc/execute-one! conn ["select * from film where film_id = ?;" film-id])
      (assoc :film/actors '...)) ; advertise virtual attr
    {`-identify :film/film_id
     `-nav (fn [m k v]
             (case k
               :film/language_id (hydrate-language conn v)
               :film/actors (actors-for-film conn (:film/film_id m))
               v))}))

(defn query-films [conn]
  (with-meta
    (->> (jdbc/execute! conn ["select film_id from film;"] #_{:schema {:film/language_id :language/language_id}})
      (mapv :film/film_id))
    {`-identify (fn [xs] `query-films)
     `-nav (fn [xs k v] (hydrate-film conn v))}))

(tests
  (def films (query-films @test-conn))
  (count films) := 1000
  (identify films) := `query-films
  (def film (nav films 0 (nth films 0)))
  (def film2 (nav films 1 (nth films 1)))
  (identify film) := 1
  (select-keys film [:film/film_id :film/language_id :film/actors])
  := {:film/film_id 1 :film/language_id 1 :film/actors '...}
  "nav one"
  (def language (nav film :film/language_id (get film :film/language_id)))
  (:language/name language) := "English             "
  "nav many"
  (def actors (nav film :film/actors (get film :film/actors)))
  (count actors) := 10
  (identify actors) := [`actors-for-film 1]
  (def actor (nav actors 0 (nth actors 0)))
  (select-keys actor [:actor/actor_id :actor/first_name])
  := #:actor{:actor_id 1, :first_name "PENELOPE"}
  (identify actor) := 1)

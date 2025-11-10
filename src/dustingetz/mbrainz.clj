(ns dustingetz.mbrainz
  (:require [datomic.api :as d] ; com.datomic/peer {:mvn/version "1.0.7075"}
            #_[hyperfiddle.electric3 :as e] ; not allowed in .clj
            [hyperfiddle.rcf :refer [tests]]))

(def datomic-uri "datomic:dev://localhost:4334/")

(declare test-conn)

(defn mbrainz-lite-uri []
  #_(def test-conn (delay (d/connect )))
  (def lennon 527765581346058) ; datomic pro
  (def pour-lamour 17592186058336)
  (def cobblestone 17592186068764)
  (def yanne 778454232478138)
  (str datomic-uri "mbrainz-1968-1973"))

(defn mbrainz-full-uri []
  (def lennon 17592186066840) ; datomic pro
  (def pour-lamour 17592186726207)
  (def cobblestone 17592198915567) ; or maybe 17592193008205, which?
  (def yanne 17592186555221)
  (str datomic-uri "mbrainz-full"))

(defn detect-mbrainz! []
  (let [available-dbs (set (d/get-database-names (str datomic-uri "*")))]
    (some available-dbs #{"mbrainz-1968-1973" "mbrainz-full"})))

(comment
  (d/get-database-names (str datomic-uri "*"))
  (detect-mbrainz!))

(def which-mbrainz (delay (detect-mbrainz!)))

(defn mbrainz-uri! []
  (case @which-mbrainz
    "mbrainz-full" (mbrainz-full-uri)
    "mbrainz-1968-1973" (mbrainz-lite-uri)))

(def test-conn (delay (d/connect (mbrainz-uri!))))
(def test-db (delay (d/db @test-conn)))

(tests
  (some? @test-db) := true
  (some? (d/touch (d/entity @test-db lennon))) := true
  (d/pull @test-db [:db/id :abstractRelease/name :abstractRelease/artistCredit] pour-lamour)
  := {:db/id pour-lamour,
      :abstractRelease/name "Pour l’amour des sous / Parle au patron, ma tête est malade",
      :abstractRelease/artistCredit "Jean Yanne & Michel Magne"})
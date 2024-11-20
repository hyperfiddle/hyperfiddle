(ns hyperfiddle.datomic ; Rosie1
  (:require [datomic.api :as d]
            [contrib.datomic :as cd]))

(def tempid? string?)

(defn entity [db e]
  (if (cd/tempid? e)
    {:db/id e}
    (d/entity db e)))

(defn pull [db pullexpr e]
  (if (cd/tempid? e)
    (if (some #{:db/id} pullexpr)
      {:db/id e}
      {})
    (d/pull db pullexpr e)))

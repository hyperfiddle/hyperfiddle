(ns hyperfiddle.datomic ; Rosie1
  (:require [datomic.api :as d]))

(def tempid? string?)

(defn entity [db e]
  (if (tempid? e)
    {:db/id e}
    (d/entity db e)))

(defn pull [db pullexpr e]
  (if (tempid? e)
    (if (some #{:db/id} pullexpr)
      {:db/id e}
      {})
    (d/pull db pullexpr e)))

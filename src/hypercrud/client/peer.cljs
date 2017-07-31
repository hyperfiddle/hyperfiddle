(ns hypercrud.client.peer
  (:require [cats.monad.either :as either]
            [hypercrud.client.core :as hc]
            [hypercrud.client.http :as http]
            [hypercrud.types.DbVal :refer [->DbVal]]
            [hypercrud.types.DbError :refer [DbError]]
            [promesa.core :as p]
            [reagent.core :as reagent]))


(defn human-error [e req]
  (let [unfilled-holes (->> (filter (comp nil? val) (.-params req)) (map key))]
    (if-not (empty? unfilled-holes)
      (either/left {:message "Invalid query" :datomic-error (.-msg e) :query (.-query req) :missing unfilled-holes})
      (either/left {:message "Datomic error" :datomic-error (.-msg e)}))))

(defn- process-result [resultset-or-error request]
  (if (instance? DbError resultset-or-error)
    (human-error resultset-or-error request)
    (either/right resultset-or-error)))

(defn hydrate-one! [entry-uri request stage-val]
  (-> (http/hydrate! entry-uri #{request} stage-val)
      (p/then (fn [{:keys [t pulled-trees-map]}]
                (if-let [result (get pulled-trees-map request)]
                  (process-result result request)
                  (either/left {:message "Server failure"}))))))

(deftype Peer [state-atom]
  hc/Peer
  (hydrate [this request]
    (if-let [result (or @(reagent/cursor state-atom [:static-ptm request])
                        @(reagent/cursor state-atom [:ptm request]))]
      (process-result result request)
      (either/left {:message "Loading" :request request})))

  (db [this conn-id branch]
    (->DbVal conn-id branch))

  (hydrate-one! [this request]
    (or (some-> @(reagent/cursor state-atom [:static-ptm request])
                (process-result request)
                (either/branch p/rejected p/resolved))
        (let [{:keys [entry-uri stage]} @state-atom]
          (hydrate-one! entry-uri request stage))))

  IHash
  (-hash [this] (goog/getUid this)))

(ns hypercrud.browser.links
  (:require [hypercrud.browser.base-64-url-safe :as base64]
            [hypercrud.client.core :as hc]
            [hypercrud.client.internal :as internal]
            [hypercrud.compile.eval :refer [eval]]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.util :as util]))


(defn build-params-map [link-ctx param-ctx]
  {:link-dbid (-> link-ctx :link-ctx/link :db/id)
   :query-params (->> (q-util/read-eval-formulas (:link-ctx/formula link-ctx))
                      (util/map-values #(q-util/run-formula % param-ctx)))
   ;; Create a result of shape [?e ?f] with new entities colored
   :create-new-find-elements (->> (:link/find-element (:link-ctx/link link-ctx))
                                  (mapv (juxt :find-element/name #(hc/*temp-id!* (-> % :find-element/connection :db/id :id))))
                                  (into {}))})


(defn query-link [link-ctx param-ctx]
  (let [tx-fn (if-let [tx-fn (-> link-ctx :link-ctx/link :link/tx-fn)]
                (let [{value :value error :error} (eval tx-fn)]
                  ;; non-fatal error, report it here so user can fix it
                  (if error (js/alert (str "cljs eval error: " error)))
                  value))]
    ;; link-dbid is assumed to be the editor-graph connection

    ;; add-result #(tx/edit-entity (:db/id entity) ident [] [(first %)])
    (if tx-fn
      {:on-click #((:user-swap! param-ctx) (tx-fn param-ctx))}
      {:href (base64/encode (pr-str (build-params-map link-ctx param-ctx)))})))

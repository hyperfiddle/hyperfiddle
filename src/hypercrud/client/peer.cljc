(ns hypercrud.client.peer
  (:require [cats.monad.either :as either]
            [hypercrud.types.DbVal :refer [->DbVal]]
            [hypercrud.util.branch :as branch]
            [hypercrud.util.reactive :as reactive]
            [hyperfiddle.io.util :refer [process-result]]))


(defn hydrate-val [ptm stage-val request]
  (let [request' [(branch/branch-vals-for-request request stage-val) request]]
    (if (contains? ptm request')
      (process-result (get ptm request') request)
      (either/left {:message "Loading" :data {:request request}}))))

; react on the answer, not the question
(defn trackable-hydrate [state-atom request]
  (let [ptm @(reactive/cursor state-atom [:ptm])
        stage-val @(reactive/cursor state-atom [:stage])]
    (hydrate-val ptm stage-val request)))

(defn hydrate [state-atom request]
  (reactive/track trackable-hydrate state-atom request))

(defn db-pointer [uri ?branch-name]
  {:pre [uri]}
  (->DbVal uri ?branch-name))

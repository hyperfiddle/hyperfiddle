(ns hyperfiddle.navigator6.selection
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.hfql2 :as hfql]
            [hyperfiddle.navigator6.state :as state]
            [geoffreygaillard.debug-tools :refer [debug-exceptions]]
            [hyperfiddle.navigator6.utils :as utils :refer [glitch-guard]]
            [hyperfiddle.ct.cofree :refer [unwrap]]
            [hyperfiddle.ct.pair :refer [to-pair]]
            [dustingetz.loader :refer [Offload]]
            [hyperfiddle.navigator6.selection2 :as selection2]
            )
  #?(:cljs (:require-macros [hyperfiddle.navigator6.selection :refer [with-local-selection]])))

(e/declare ^:dynamic *local-selection ^:dynamic *local-selection-symbolic ^:dynamic *parent-selection-symbolic)

;; (symbolic-cell-coordinates hyperfiddle.navigator6.matrix/_cellT)
;; (symbolic-row-coordinates hyperfiddle.navigator6.matrix/_cellT)

(e/defn WithLocalSelection [state-key resolve-fn Body]
  (e/client
    (binding [*local-selection-symbolic (e/server (to-pair (get state/*local-state state-key)))]
      (binding [*local-selection (e/server (let [resolve-fn ((e/capture-fn) resolve-fn)]
                                             (Offload #(resolve-fn (to-pair *local-selection-symbolic)) {:label 'resolve-selected-object})))]
        (->> (Body)
          (utils/InterceptCommand `SetSelection!
            (e/fn [[_ [row-index Getter]]]
              [`state/ReplaceState! assoc state-key (e/server ((glitch-guard selection2/symbolic-selection-coordinates) (Getter) row-index))])))))))

(defmacro with-local-selection [state-key resolve-fn & body]
  `(WithLocalSelection ~state-key ~resolve-fn (e/fn [] ~@body)))



(defn select-pull-step [stepT]
  (cond
    (hfql/pull? stepT) stepT
    (hfql/traversal? stepT) (some-> (unwrap stepT) first select-pull-step)
    ;; (some-> (unwrap stepT) first select-pull-step)
    :else (hfql/wrap-in-pull stepT) ; FIXME highly questionable
    #_(throw (ex-info "No pull child" {:type (hfql/node-type stepT)}))))


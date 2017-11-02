(ns hypercrud.browser.browser-ui
  (:require [cats.core :as cats :refer-macros [mlet]]
            [cats.monad.either :as either :refer-macros [try-either]]
            [hypercrud.browser.anchor :as anchor]
            [hypercrud.browser.auto-link :as auto-link]
            [hypercrud.browser.base :as base]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.routing :as routing]
            [hypercrud.client.core :as hc]
            [hypercrud.client.schema :as schema-util]
            [hypercrud.compile.eval :refer [eval-str']]
            [hypercrud.platform.native-event-listener :refer [native-listener]]
            [hypercrud.platform.safe-render :refer [safe-user-renderer]]
            [hypercrud.state.actions.core :as actions]
            [hypercrud.state.actions.util :as actions-util]
            [hypercrud.ui.stale :as stale]
            [hypercrud.util.core :as util]
            [reagent.core :as r]))


(declare ui')
(declare ui)

(let [browse (fn [anchor-index ident ctx & [user-renderer & args]]
               (let [ctx (if user-renderer
                           (assoc ctx :user-renderer user-renderer #_(if f #(apply f %1 %2 %3 %4 args)))
                           ctx)]
                 [ui (get anchor-index ident) ctx]))
      anchor (fn [anchor-index ident ctx label]
               (let [props (-> (anchor/build-anchor-props (get anchor-index ident) ctx)
                               #_(dissoc :style) #_"custom renderers don't want colored links")]
                 [(:navigate-cmp ctx) props label]))
      browse' (fn [anchor-index ident ctx]
                (ui' (get anchor-index ident) (assoc ctx :user-renderer identity)))
      anchor* (fn [anchor-index ident ctx] (anchor/build-anchor-props (get anchor-index ident) ctx))
      link-fn (fn [anchor-index ident label ctx] (anchor anchor-index ident ctx label))]
  (defn with-reprocessed-result [ui-fn result ordered-fes anchors ctx]
    (let [anchors (if (:keep-disabled-anchors? ctx)
                    anchors
                    (remove :anchor/disabled? anchors))
          anchor-index (->> anchors
                            (filter :anchor/ident)          ; cannot lookup nil idents
                            (mapv (juxt #(-> % :anchor/ident) identity)) ; [ repeating entity attr ident ]
                            (into {}))
          ctx (assoc ctx
                :anchor (r/partial anchor anchor-index)
                :browse (r/partial browse anchor-index)
                :anchor* (r/partial anchor* anchor-index)
                :browse' (r/partial browse' anchor-index)

                ; backwards compat
                :with-inline-result (r/partial browse anchor-index)
                :link-fn (r/partial link-fn anchor-index))]
      ; result is relation or set of relations
      (ui-fn result ordered-fes anchors ctx))))

(defn link-user-fn [link]
  (if-not (empty? (:link/renderer link))
    (-> (eval-str' (:link/renderer link))
        (either/branch
          (fn [e] (constantly [:pre (util/pprint-str e)]))
          (fn [user-fn]
            (fn [result ordered-fes anchors ctx]
              [safe-user-renderer user-fn result ordered-fes anchors ctx]))))))

(defn result-cmp [link pre-binding-ctx result ordered-fes anchors ctx]
  (let [ui-fn (case @(:display-mode pre-binding-ctx)
                ; todo executing this user-renderer is potentially unsafe
                :user (or (:user-renderer pre-binding-ctx) (link-user-fn link) hypercrud.ui.result/view)
                :xray hypercrud.ui.result/view
                :root hypercrud.ui.result/view)]
    (with-reprocessed-result ui-fn result ordered-fes anchors ctx)))

(defn hydrate-link [ctx]
  (if (auto-link/system-link? (get-in ctx [:route :link-id]))
    (either/right (auto-link/hydrate-system-link (get-in ctx [:route :link-id]) ctx))
    (hc/hydrate (:peer ctx) (base/meta-request-for-link ctx))))

(defn ui-from-route' [route ctx]
  (try
    (let [ctx (context/route ctx route)]
      (mlet [link (hydrate-link ctx)
             ordered-fes (base/get-ordered-find-elements link ctx)
             :let [ctx (context/override-domain-dbs ctx)]
             request (base/request-for-link link ordered-fes ctx)
             result (if request (hc/hydrate (:peer ctx) request) (either/right nil))
             ; schema is allowed to be nil if the link only has anchors and no data dependencies
             schemas (schema-util/hydrate-schema ordered-fes ctx)
             :let [f (r/partial result-cmp link ctx)]]
        (base/process-results f link request result schemas ordered-fes ctx)))
    ; js errors? Why do we need this?
    ; user-renderers can throw, should be caught lower though
    (catch :default e (either/left e))))

(defn ui-from-props' [anchor anchor-props ctx]
  (try
    ; if a user is invoking this fn explicitly they probably dont care if the anchor is hidden
    ; todo should filter hidden anchors out before recursing (in widget/render-inline-anchors)
    (if (:hidden anchor-props)
      (either/right [:noscript])                            ; todo cannot return hiccup here, this is a value function
      (mlet [route (anchor/build-anchor-route' anchor ctx)]
        ; entire context must be encoded in the route
        (ui-from-route' route (context/clean ctx))))
    ; js errors? Why do we need this.
    (catch :default e (either/left e))))

(defn ui' [anchor ctx]
  ; js errors? Why do we need this exception monad.
  (-> (try-either (anchor/build-anchor-props anchor ctx))   ; LOOOOOLLLLLL we are dumb
      (cats/bind #(ui-from-props' anchor % ctx))))

(defn ui-error-inline [e ctx]
  (let [dev-open? (some-> (:dev-open? ctx) deref)
        detail (if dev-open? (str " -- " (pr-str (:data e))))]
    [:code (:message e) " " detail]))

(defn ui-error-block [e ctx]
  #_(ex-message e) #_(pr-str (ex-data e))
  (let [dev-open? (some-> (:dev-open? ctx) deref)
        detail (if dev-open? (util/pprint-str (:data e)))]
    ; todo we don't always return an error with a message
    [:pre (:message e) "\n" detail]))

(defn ui-error [e ctx]
  ; :find-element :entity :attribute :value
  (let [C (cond
            (:ui-error ctx) (:ui-error ctx)                 ; botnav
            (:attribute ctx) ui-error-inline                ; table: header or cell, form: header or cell
            (:find-element ctx) ui-error-inline             ;
            :else ui-error-block)]                          ; browser including inline true links
    [C e ctx]))

(defn page-on-click [ctx route event]
  (when (and route (contains? @(r/cursor (-> ctx :peer .-state-atom) [:pressed-keys]) "alt"))
    ((:dispatch! ctx) (fn [dispatch! get-state]
                        (let [encoded-route (routing/encode route)]
                          (when (actions-util/navigable? encoded-route (get-state))
                            (actions/set-route encoded-route dispatch! get-state)))))
    (.stopPropagation event)))

(defn wrap-ui [v' route ctx]
  (let [on-click (r/partial (or (:page-on-click ctx)
                                (r/partial page-on-click ctx))
                            route)]
    ^{:key route}
    [native-listener {:on-click on-click}
     [stale/loading v'
      (fn [e] [:div.ui (ui-error e ctx)])
      (fn [v] [:div.ui v])
      (fn [v] [:div.ui.loading v])]]))

(defn ui-from-route [route ctx]
  [wrap-ui (ui-from-route' route ctx) route ctx])

(defn ui [anchor ctx]
  ; js errors? Why do we need this exception monad.
  (let [anchor-props (try-either (anchor/build-anchor-props anchor ctx)) ; LOOOOOLLLLLL we are dumb
        v' (mlet [anchor-props anchor-props]
             (ui-from-props' anchor anchor-props ctx))
        route (-> (cats/fmap :route anchor-props)
                  (cats/mplus (either/right nil))
                  (cats/extract))]
    [wrap-ui v' route ctx]))

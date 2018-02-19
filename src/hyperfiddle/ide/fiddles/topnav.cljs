(ns hyperfiddle.ide.fiddles.topnav
  (:require [cats.core :as cats]
            [cats.monad.either :as either]
            [cuerdas.core :as string]
            [hypercrud.browser.auto-fiddle :as auto-fiddle]
            [hypercrud.browser.base :as base]
            [hypercrud.browser.browser-ui :as browser-ui]
            [hypercrud.browser.link :as link]
            [hypercrud.client.tx :as tx]
            [hypercrud.react.react-fragment :refer [react-fragment]]
            [hypercrud.ui.control.markdown-rendered :refer [markdown]]
            [hypercrud.ui.radio :as radio]
            [hypercrud.ui.result :as result]
            [hypercrud.ui.tooltip :refer [tooltip]]
            [hypercrud.util.core :as util :refer [unwrap]]
            [hypercrud.util.reactive :as reactive]
            [hyperfiddle.foundation :as foundation :refer [staging]]
            [hyperfiddle.foundation.actions :as foundation-actions]
            [hyperfiddle.ide.fiddles.topnav-bindings :as topnav-bindings]
            [hyperfiddle.runtime :as runtime]
            [reagent.core :as reagent]))


(defn stateless-login-url [ctx]
  (let [{:keys [domain client-id]} (get-in ctx [:hypercrud.browser/repository :repository/environment :auth0 (:hyperfiddle-hostname ctx)])
        callback-url (str "http://" (:hostname ctx) foundation/auth0-redirect-path)]
    (str domain "/login?client=" client-id "&callbackURL=" callback-url)))

; inline sys-link data when the entity is a system-fiddle
(defn shadow-fiddle [fiddle ctx]
  (let [[e a] (get-in ctx [:route :request-params])
        system-fiddle? (auto-fiddle/system-fiddle? (:db/id e))]
    (if system-fiddle?
      (->> (auto-fiddle/hydrate-system-fiddle (:db/id e))
           (cats/fmap (fn [fiddle]
                        (-> fiddle
                            (update :fiddle/bindings #(or (-> % meta :str) %))
                            (update :fiddle/renderer #(or (-> % meta :str) %))
                            (update :fiddle/request #(or (-> % meta :str) %)))))
           unwrap)
      fiddle)))

; ugly hacks to recursively fix the ui for sys links
(defn hijack-renderer [fiddle fes links ctx]
  (let [ctx (dissoc ctx :user-renderer)
        f-mode-config (browser-ui/f-mode-config)
        ui-fn (-> (base/fn-from-mode f-mode-config (:fiddle ctx) ctx)
                  (cats/mplus (either/right (:default f-mode-config)))
                  deref)]
    (ui-fn (shadow-fiddle fiddle ctx) fes links ctx)))

(defn any-loading? [peer]
  (some (comp not nil? :hydrate-id) @(runtime/state peer [::runtime/partitions])))

(defn loading-spinner [ctx]
  (if @(reactive/track any-loading? (:peer ctx))
    [:span {:style {:height "20px"
                    :width "23px"
                    :float "left"
                    :margin-right "1px"
                    :background-color "white"
                    :position "relative"
                    :z-index 0}}
     [:div {:style {:height "1em"
                    :margin-top "-2px"
                    :position "absolute"
                    :z-index -1}}
      [re-com.core/throbber :size :smaller]]]))

(defn -renderer [fiddle ordered-fes links ctx]
  (let [display-mode @(runtime/state (:peer ctx) [:display-mode])
        dirty? (not (empty? @(runtime/state (:peer ctx) [:stage])))
        fiddle (shadow-fiddle fiddle ctx)
        ; hack until hyperfiddle.net#156 is complete
        link-index (->> links
                        (filter :link/rel)                  ; cannot lookup nil idents
                        (mapv (juxt #(-> % :link/rel) identity)) ; [ repeating entity attr ident ]
                        (into {}))
        fake-managed-anchor (fn [ident ctx label & args]
                              ; mostly copied from browser-ui
                              (let [kwargs (util/kwargs args)
                                    link (-> (get link-index ident) (assoc :link/managed? true))
                                    props (-> (link/build-link-props link ctx true)
                                              #_(dissoc :style) #_"custom renderers don't want colored links")]
                                [(:navigate-cmp ctx) props label (:class kwargs)]))]
    [:div.hyperfiddle-topnav
     [:div.hyperfiddle-topnav-root-controls
      (fake-managed-anchor :domain ctx (get-in ctx [:target-domain :domain/ident]))
      " / "
      (fake-managed-anchor :fiddle-more (assoc ctx :user-renderer hijack-renderer) (string/prune (:fiddle/name fiddle) 20 ""))
      " · "
      (fake-managed-anchor :links (assoc ctx :user-renderer hijack-renderer) "links")
      (fake-managed-anchor :ui (assoc ctx :user-renderer hijack-renderer) "view")
      (fake-managed-anchor :stage ctx "stage" :class (if dirty? "stage-dirty"))

      (let [change! #(runtime/dispatch! (:peer ctx) (foundation-actions/set-display-mode %))]
        [:span.radio-group
         (radio/option {:label "data" :tooltip "Edit data directly" :target :xray :value display-mode :change! change!})
         (radio/option {:label "view" :tooltip "View end-user UI" :target :user :value display-mode :change! change!})])

      [:div.right-nav {:key "right-nav"}                    ; CAREFUL; this key prevents popover flickering
       [loading-spinner ctx]
       ; ignore results; don't display the fiddle's data, just the anchors
       ((:browse ctx) :repo-picker ctx :class "hyperfiddle-topnav-repo-picker"
         (fn [result]
           [:span
            "repo: " (->> result
                          (map #(get % "?e"))
                          (filter #(= (:uri ctx) (:dbhole/uri %)))
                          ((comp :dbhole/name first)))]))
       ((:anchor ctx) :new-fiddle ctx "new-fiddle")
       (if (:user-profile ctx)
         ((:anchor ctx) :account ctx (get-in ctx [:user-profile :email]))
         [:span.nav-link.auth [:a {:href (str (stateless-login-url ctx) "&state=" (runtime/encode-route (:peer ctx) (:target-route ctx)))} "Login"]])]]
     [:div.hyperfiddle-topnav-fiddle-controls
      (result/result-renderer fiddle ordered-fes links ctx)
      ]]))

(defn- update-spacer [topnav]
  (let [measuredHeight (-> topnav (aget "fixed") .-offsetHeight)]
    (-> topnav (aget "spacer") (aget "style") (aset "height" (str measuredHeight "px")))))

(def renderer
  (reagent/create-class
    {:render (fn [this]
               [:div.hyperfiddle-topnav-container
                [:div.spacer {:ref (fn [!el] (aset this "spacer" !el))}]
                [:div.fixed {:ref (fn [!el] (aset this "fixed" !el))}
                 (let [[_ & args] (reagent/argv this)]
                   (apply vector -renderer args))]])
     :component-did-mount update-spacer
     :component-did-update update-spacer}))

(defn ^:export qe-picker-control [field props ctx]
  (let [enums [:query :entity :blank]
        change! #((:user-with! ctx) (tx/update-entity-attr @(:cell-data ctx) (:attribute ctx) %))
        options (->> enums
                     (map #(radio/option
                             {:label (case % :query "query" :entity "pull" :blank "blank")
                              :target %
                              :value @(:value ctx)
                              :change! change!})))]
    [:span.qe.radio-group (apply react-fragment :_ options)]))

(defn ^:export stage-ui [result ordered-fes links ctx]
  [:div.hyperfiddle-topnav-stage
   (result/view result ordered-fes links ctx)               ; for docstring
   (let [anonymous? (nil? (:user-profile ctx))
         stage @(runtime/state (:peer ctx) [:stage])
         disabled? (or anonymous? (empty? stage))]
     ; tooltip busted
     [tooltip (cond anonymous? {:status :warning :label "please login"}
                    (empty? stage) {:status :warning :label "no changes"})
      [:button {:disabled disabled?
                :style (if disabled? {:pointer-events "none"})
                :on-click (fn []
                            ; specifically dont use the SplitRuntime protocol here. the only thing that makes sense is whats in the context from the route
                            (let [target-repo (->> (:target-domain ctx)
                                                   :domain/code-databases
                                                   (filter #(= (:dbhole/name %) (get-in ctx [:target-route :code-database])))
                                                   first
                                                   (into {}))
                                  nil-branch-aux {:hyperfiddle.ide/foo "page"}]
                              (runtime/dispatch! (:peer ctx) (foundation-actions/transact! (:peer ctx) target-repo nil-branch-aux))))}
       "transact!"]])
   [staging (:peer ctx)]
   [:div.markdown (markdown "Hyperfiddle always generates valid transactions, if it doesn't, please file a bug.

*WARNING:* Datomic schema alterations cannot be used in the same transaction, for now you'll
need to transact the schema before using, see [#6](https://github.com/hyperfiddle/hyperfiddle/issues/6).")]])

(defn bindings [ctx] (topnav-bindings/bindings ctx))

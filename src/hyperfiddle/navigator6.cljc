(ns hyperfiddle.navigator6
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-forms5 :as forms]
            [hyperfiddle.api :as hf]
            [hyperfiddle.hfql2 :as hfql]
            [hyperfiddle.router5 :as router]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.ui.tooltip :as tooltip]
            [hyperfiddle.navigator6.rendering :as rendering]
            [hyperfiddle.navigator6.state :as state :refer [with-local-state]]
            [hyperfiddle.navigator6.search :as search :refer [with-local-search]]
            [hyperfiddle.navigator6.selection :as selection :refer [with-local-selection]]
            [hyperfiddle.navigator6.selection2 :as selection2]
            [hyperfiddle.navigator6.matrix :refer [RenderMatrix]]
            [hyperfiddle.navigator6.utils :as utils :refer [rebooting freezing glitch-guard]]
            [contrib.css :as cssx]
            [contrib.data :refer [#?(:clj array?)]]
            [hyperfiddle.hfql2.utils :refer [imap?]]
            [dustingetz.str :as strx]
            [dustingetz.loader :as loader :refer [Offload]]
            [hyperfiddle.ct.function :refer [flip]]
            [hyperfiddle.ct.either :refer [either right left? from-left from-right]]
            [hyperfiddle.ct.comonad :refer [extract]]
            [hyperfiddle.ct.cofree :refer [unwrap]]
            [contrib.assert :refer [is]])
  #?(:cljs (:require-macros hyperfiddle.navigator6)))

(e/declare ^:dynamic *navigation-depth*)

(defn- augment-scope-with-%-from-route-args [args stepT]
  (if (seq args)
    (let [scope (hfql/scope stepT)
          % (hfql/hfql-resolve* (first args))] ; TODO move to ednish?
      (hfql/seed (assoc scope '% %) stepT))
    stepT))

(defn- resolve-route [sitemap [f & args :as _route]]
  (when-some [stepT (get sitemap f)]
    (augment-scope-with-%-from-route-args args stepT)))

(defn- normalize-router-query [router-query]
  (cond
    (vector? router-query) (seq router-query)
    (ident? router-query) (list router-query)
    :else router-query))

(defn- recursion-end? [hfqlT]
  (or (and (hfql/multiplicity-many? hfqlT) (hfql/recursion-end?! hfqlT))
      (hfql/recursion-end?! hfqlT)))

(defn- effective-render-type
  "Recurse through traversals to find the effective render type.
   Returns ::pull, ::star, ::recursion-end, or nil (leaf)."
  [hfqlT]
  (cond
    (hfql/star? hfqlT) ::star
    (hfql/pull? hfqlT) ::pull
    (hfql/traversal? hfqlT) (if (hfql/multiplicity-many? hfqlT)
                              (if (recursion-end? hfqlT) ::recursion-end ::star)
                              (effective-render-type (first (hfql/next-steps hfqlT))))
    :else nil))

(defn- associative-value?
  "True when the raw value at point is associative (record or map, not a collection).
   Records implement IPersistentMap, so (imap? v) covers them."
  [v]
  (or (imap? v)
      #?@(:clj [(and (some? v) (not (instance? clojure.lang.IObj v)) (not (array? v)))])))

(defn- suggest-and-graft
  "Nav the value at `hfqlT`, auto-suggest columns from runtime type, graft
  the suggested tree onto `hfqlT` as parent. Returns suggested HFQLT or nil.
  Shared logic between SuggestAndRender (rendering) and resolve-selection-coordinates (selection)."
  [hfqlT]
  (let [either-value (hfql/nav* hfqlT)
        value (either identity identity either-value)]
    (either
      (constantly nil)
      (fn [suggestedT]
        (when (and (some? suggestedT) (not= hfqlT suggestedT))
          (-> suggestedT
            ((flip hfql/seed) (assoc (hfql/scope hfqlT) (symbol "%") (right value)))
            (hfql/set-parent hfqlT))))
      (hfql/suggest* value))))

(e/defn SuggestAndRender
  "Unguided rendering — no HFQL query structure available.
   Nav the value, auto-suggest columns from runtime type, render as matrix.
   Used at recursion-end (recursion budget exhausted) and leaf traversals
   (no static structure definition: escaping sitemap)."
  [hfqlT]
  (let [either-value ((glitch-guard hfql/nav*) hfqlT)
        value (either identity identity either-value)
        associative? (associative-value? value)
        ;; suggest-and-graft handles the happy path (Right). For errors (Left),
        ;; we render the error as a value instead of silently returning nil.
        stepT (or ((glitch-guard suggest-and-graft) hfqlT)
                (when (left? either-value)
                  (-> (hfql/hfql [*] (right (from-left nil either-value)))
                    ((glitch-guard hfql/set-parent) hfqlT))))]
    (if (some? stepT)
      (e/fn [_] (RenderMatrix stepT :associative? associative?))
      (e/fn [& _]))))

(e/defn ResolveRenderer [hfqlT]
  (e/server
    (e/Reconcile
      (case (e/Offload #((glitch-guard effective-render-type) hfqlT))
        ;; Structured node (pull or star) — the outer step (possibly a traversal wrapper) is
        ;; passed to RenderMatrix. hfql/nav inside RenderMatrix executes any traversal IO.
        ;; pull → associative (single object, transposed), star → not associative (collection).
        ::pull (e/fn [stepT] (RenderMatrix stepT :associative? true))
        ::star (e/fn [stepT] (RenderMatrix stepT :associative? false))
        ::recursion-end (e/Reconcile (SuggestAndRender hfqlT))
        (e/Reconcile (SuggestAndRender hfqlT))))))

(defn inject-dynamic-bindings [e-bindings stepT]
  (hfql/update-scope stepT update `hfql/*bindings* merge e-bindings))

#?(:clj
   (defn- resolve-selection-coordinates [current-point symbolic-selection-coordinates bindings]
     (with-bindings bindings
       (let [selectable-point (hyperfiddle.navigator6.selection2/find-selectable-point current-point)]
         (or (and (some? selectable-point)
               (hyperfiddle.navigator6.selection2/resolve-selection-coordinates
                 selectable-point
                 symbolic-selection-coordinates))
           ;; current-point is a leaf traversal (e.g. .getName → String). ResolveRenderer
           ;; would have nav'd + suggested to build a synthetic pull (e.g. String's Java
           ;; methods). Replicate that suggestion so we can resolve selection against the
           ;; same tree structure the user sees on screen.
           (let [suggested-stepT (suggest-and-graft current-point)
                 selectable-from-suggested (when suggested-stepT
                                            (hyperfiddle.navigator6.selection2/find-selectable-point suggested-stepT))]
             (when selectable-from-suggested
               (hyperfiddle.navigator6.selection2/resolve-selection-coordinates
                 selectable-from-suggested
                 symbolic-selection-coordinates))))))))

(defn- object-level-identity [obj]
  #?(:clj (System/identityHashCode obj)
     :cljs (goog/getUid obj)))

(e/declare NavigationCard)

(e/defn Navigate* [sitemap stepT]
  (e/client
    (dom/div (dom/props {:style {:display :contents}}) ; HACK – presence of this div circumvent a crash where `mount-point` will call "parent.appendChild(nil)"
      (let [id (e/server (with-bindings* (or e/*bindings* {}) ; Inject bindings must be active before thunk forcing
                           (fn [] (hash [((glitch-guard hfql/id) stepT) ; HACK
                                         (object-level-identity ((glitch-guard hfql/value-at-point) stepT)) ; HACK to compensate for 𝓞(1) selection always hydrating the first instance of a multiplicity many result TODO revise.
                                         ]))))]
        (NavigationCard *navigation-depth*
          (e/fn []
            (rebooting id ; glitch guard
              (e/server
            (freezing [sitemap stepT]
              (e/Reconcile
                (e/When ((complement (fnil zero? 0)) id) ; guard navigate on nil
                  (with-local-state ::state/route ; injects *local-state and interpret state write commands
                    (with-local-search ::search   ; injects *local-search and interpret search commands
                      (with-local-selection ::selection ; injects *local-selection and interpret selection commands
                            (e/server #(do #_(prn "coords:" %) ((glitch-guard resolve-selection-coordinates) stepT % e/*bindings*)))
                          (e/amb
                            (e/call (ResolveRenderer stepT) ; call renderer for current point
                              ;; For associative views (::pull), exclude *local-search* from HFQL scope injection.
                              ;; Otherwise, search changes re-seed the HFQLT → cascade cache bust → IO reruns (e.g. .listFiles).
                              ;; See plans/associative-search-cell-recomputation.md for full analysis.
                              (e/server (let [bindings (if (= ::pull ((glitch-guard effective-render-type) stepT))
                                                         (dissoc e/*bindings* (find-var `search/*local-search))
                                                         e/*bindings*)]
                                          ((glitch-guard (flip inject-dynamic-bindings)) stepT bindings))))
                            (let [next-childT (e/server (hyperfiddle.navigator6.selection2/find-next-navigable-point sitemap #_stepT selection/*local-selection))] ; Navigation must prioritizes recursive children over regular children at point
                              (e/Reconcile
                                (when (e/server (some? next-childT))
                                  (e/server
                                    (binding [selection/*parent-selection-symbolic selection/*local-selection-symbolic
                                              selection/*parent-selection selection/*local-selection
                                              *navigation-depth* (inc *navigation-depth*)]
                                      (Navigate* sitemap next-childT)))))
                              (e/amb)))))))))))))))))

(e/defn NavigationCard [depth Body]
  (e/client
    (dom/div
      (dom/props {:class "hyperfiddle-navigation-card"
                  :style {:--navigation-depth depth
                          :z-index (+ 10 depth)}
                  :data-depth depth
                  :role "dialog"
                  :aria-modal (zero? depth) ; Only root is non-modal
                  :aria-label (str "Navigation level " (inc depth))
                  :tabindex "-1"}) ; Focusable for keyboard nav

      ;; Chrome bar — left/right slots are filled by views via dynamic vars
      (let [[chrome-left chrome-right]
            (dom/div
              (dom/props {:class "navigation-card__chrome"})
              ;; Click chrome to focus this card (trashes deeper children)
              (let [chrome-clicked (dom/On "click"
                                    (fn [^js e]
                                      (when-not (.. e -target (closest ".navigation-card__close-button"))
                                        (.stopPropagation e)
                                        true))
                                    nil)]
                (e/Reconcile
                  (when chrome-clicked
                    [`selection/SetSelection! nil])))
              ;; Left slot: title + count pill (filled by RenderMatrix)
              (let [left (dom/div (dom/props {:class "navigation-card__chrome-left"}) dom/node)]
                ;; Right slot: search input (filled by RenderMatrix)
                (let [right (dom/div (dom/props {:class "navigation-card__chrome-right"}) dom/node)]
                  ;; Close button — rightmost in chrome bar
                  (when (pos? depth)
                    (dom/button
                      (dom/props {:class "navigation-card__close-button"
                                  :aria-label "Close"
                                  :title "Close (ESC)"})
                      (dom/text "\u2715")
                      (dom/On "click"
                        (fn [e]
                          (.preventDefault e)
                          (.stopPropagation e)
                          [`selection/SetSelection! nil])
                        nil)))
                  [left right])))]

        ;; ESC key handler
        (when (pos? depth)
          (let [esc-pressed (dom/On "keydown"
                              (fn [^js e]
                                (when (= "Escape" (.-key e))
                                  (.preventDefault e)
                                  (.stopPropagation e)
                                  true))
                              {:node dom/node})]
            (e/Reconcile
              (when esc-pressed
                [`selection/SetSelection! nil]))))

        ;; Content area — bind chrome slots so views can inject header content
        (dom/div
          (dom/props {:class "navigation-card__content"})
          (binding [utils/*card-chrome-left* chrome-left
                    utils/*card-chrome-right* chrome-right]
            (Body)))))))

;; (e/defn RenderMeta [stepT]
;;   (e/server
;;     (when-some [m ((glitch-guard (comp extract meta)) stepT)]
;;       (dom/details
;;         (dom/props {:open true})
;;         (dom/summary (dom/text "Meta"))
;;         (dom/pre (dom/text (strx/pprint-str m)))))))


;;;;;;;;;;;;;;;;
;; Entrypoint ;;
;;;;;;;;;;;;;;;;

(declare css)

(e/defn Inject ; ^::e/export
  "Route handler: calls setup-fn with route args, merges returned bindings
   into e/*bindings*, then calls Continuation to render the sub-route."
  [setup-fn args Continuation]
  (e/server
    (let [extra-bindings (e/Offload #(apply setup-fn args))]
      (binding [e/*bindings* (merge e/*bindings* extra-bindings)]
        (e/client (Continuation))))))

(e/defn Route-handlers []
  "Route symbols that dispatch to e/defn handlers instead of sitemap HFQL entries.
   Handlers receive [setup-fn args Continuation] where Continuation renders the
   sub-route (already popped). Handlers control the continuation."
  {'Inject Inject})

(e/defn RouteDispatch
  "Dispatches the current route segment. If the segment symbol is a route handler
   (e.g. Inject), calls it with setup-fn, args, and a continuation that pops the
   route and re-dispatches. Otherwise resolves the segment in the sitemap as an
   HFQL entry and navigates.
   setup-fn is never nil — HfqlRoot normalizes absent setup-fn to (constantly {})."
  [sitemap entrypoints setup-fn]
  (e/client
    (let [query (normalize-router-query (first router/route))]
      (if-not query
        (router/ReplaceState! ['. [(first entrypoints)]])
        (let [[f & args] query]
          (if-some [Handler (get (Route-handlers) f)]
            ;; Route handler — call with args and continuation (popped route)
            (Handler setup-fn args
              (e/fn [] (router/pop (RouteDispatch sitemap entrypoints setup-fn))))
            ;; HFQL sitemap entry — normal dispatch
            (do
              (set! (.-title js/document) (str (some-> f name (str " \u2013 ")) "Hyperfiddle"))
              (dom/props {:class (cssx/css-slugify f)})
              (binding [*navigation-depth* 0]
                (Navigate* sitemap (e/server (is (with-bindings* (or e/*bindings* {}) resolve-route sitemap query)
                                                 some? (str "Route not found in sitemap: " (pr-str (first query))))))))))))))

(e/defn HfqlRoot
  "HFQL navigator entrypoint. Renders styles, tooltips, and dispatches routes.

   setup-fn — multi-arity (fn [& args] -> bindings-map) for dependency injection.
              Zero-arity is called once at root — returned map is merged into e/*bindings*.
              N-arity is called per Inject route segment, e.g. /(Inject,'mbrainz-full')/.
              nil is fine (defaults to (constantly {}))."
  ([sitemap entrypoints] (HfqlRoot sitemap entrypoints nil))
  ([sitemap entrypoints setup-fn]
   (e/server
     (let [setup-fn (or setup-fn (constantly {}))]
       (binding [#_#_e/*exports* (e/exports)
                 e/*bindings* (merge
                                {#'rendering/*server-pretty {}
                                 #'rendering/*tooltip-fn* nil}
                                (#(or e/*bindings* {}))
                                (e/Offload #(setup-fn)))]
         (binding [rendering/*server-pretty (get e/*bindings* #'rendering/*server-pretty)
                   rendering/*tooltip-fn* (get e/*bindings* #'rendering/*tooltip-fn*)]
           (e/client
             (dom/style (dom/text css tooltip/css))
             (dom/link (dom/props {:rel :stylesheet, :href (e/server (str hf/*base-static-resource-path* "electric-forms.css"))}))
             (dom/link (dom/props {:rel :stylesheet, :href (e/server (str hf/*base-static-resource-path* "matrix.css"))}))
             (dom/link (dom/props {:rel :stylesheet, :href (e/server (str hf/*base-static-resource-path* "matrix_vertical.css"))}))
             (dom/link (dom/props {:rel :stylesheet, :href (e/server (str hf/*base-static-resource-path* "matrix_horizontal.css"))}))
             (dom/link (dom/props {:rel :stylesheet, :href (e/server (str hf/*base-static-resource-path* "matrix_cards.css"))}))
             (forms/Checkbox* false {:class "data-loader__enabled" :style {:position :absolute, :inset-block-start "1dvw", :inset-inline-end "1dvw"}})
             #_(Index sitemap entrypoints) ;; replaced by in-band HFQL pages index
             (tooltip/TooltipArea
               (e/fn []
                 (tooltip/Tooltip)
                 (dom/div
                   (dom/props {:class "Browser"})
                   (RouteDispatch sitemap entrypoints setup-fn)))))))))))



(def table-block-css
  "
.hyperfiddle-navigator6__block table { display: grid; grid-template-columns: auto repeat(var(--column-count), 1fr);  grid-template-rows: var(--row-height);}

.hyperfiddle-navigator6__block table thead { display: contents; }
.hyperfiddle-navigator6__block table thead tr { display: grid; grid-row: 1; grid-column: 1 / -1; grid-template-columns: subgrid;}
.hyperfiddle-navigator6__block table thead tr th { white-space: nowrap; text-overflow: ellipsis; overflow: hidden; }

.hyperfiddle-navigator6__block .hyperfiddle-electric-forms5__table-picker { grid-column: 1 / -1; grid-template-columns: subgrid; contain: size;}
.hyperfiddle-navigator6__block thead + .hyperfiddle-electric-forms5__table-picker { grid-row: 2; grid-column: 1 / -1; grid-template-columns: subgrid; contain: size; }
")

(def css
  (str
    "@import url('https://fonts.googleapis.com/css2?family=Open+Sans:wght@300;400;500;600;700;800&display=swap');
*:has(.Browser), .Browser, .Browser * { box-sizing: border-box; }
body:has(.Browser) { font-family: 'Open Sans', Arial, Verdana, sans-serif; background-color: rgb(248 250 252); }
html:has(.Browser) { scrollbar-gutter: stable; }
"
    forms/css
    loader/css
    table-block-css

    "
/* cosmetic defaults */
.hyperfiddle-navigator6__block legend .title {font-weight:600;}
.hyperfiddle-navigator6__block { padding: 0; background-color: white; }
.hyperfiddle-navigator6__block table { border: 0px solid #f2f2f2; border-top-left-radius: 0rem; border-top-right-radius: 0rem; }
.hyperfiddle-navigator6__block table thead tr { background-color: #f2f2f2; border-bottom: 1px lightgray solid; }
.hyperfiddle-navigator6__block table thead tr th { white-space: nowrap; text-overflow: ellipsis; overflow: hidden; }
.hyperfiddle-navigator6__block table thead tr th { font-weight: 500; }
.hyperfiddle-navigator6__block table thead tr th:not(:first-child) { border-left: 1px lightgray solid; }
.hyperfiddle-navigator6__block table :is(th, td) { padding: 0 0.25em; }
/* --------- */

/* query monitor */
.hyperfiddle-query-monitor {
  position: fixed;
  top: 10px;
  right: 10px;
  height: 200px;
  width: 500px;
  overflow-y: auto;
  z-index: 999;
  background-color: rgba(44, 62, 80, 0.85);
  color: #ecf0f1;
  border-radius: 6px;
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.2);
  font-family: 'Consolas', 'Monaco', monospace;
  font-size: 13px;
  padding: 8px 0;
  border: 1px solid rgba(255, 255, 255, 0.1);
  scrollbar-width: thin;
  scrollbar-color: rgba(255, 255, 255, 0.3) transparent;
}
"))


;; for reference
#_(e/defn RoundTrip [v] (identity (e/server (identity v)))) ; Awesome debug tool! Forces a roundtrip to diagnose how many roundtrips it takes to ack a value!
;; (RoundTrip (RoundTrip :foo)) ; two roundtrips

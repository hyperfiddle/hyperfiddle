(ns hyperfiddle.navigator6
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-forms5 :as forms]
            [hyperfiddle.api :as hf]
            [hyperfiddle.hfql2 :as hfql]
            [hyperfiddle.entrypoint2 :refer [Index]]
            [hyperfiddle.router5 :as router]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.ui.tooltip :as tooltip]
            [hyperfiddle.navigator6.rendering :as rendering]
            [hyperfiddle.navigator6.state :as state :refer [with-local-state]]
            [hyperfiddle.navigator6.search :as search :refer [with-local-search]]
            [hyperfiddle.navigator6.selection :as selection :refer [with-local-selection]]
            [hyperfiddle.navigator6.selection2 :as selection2]
            [hyperfiddle.navigator6.matrix :refer [RenderMatrix]]
            [hyperfiddle.navigator6.utils :refer [rebooting freezing glitch-guard]]
            [contrib.css :as cssx]
            [contrib.data :refer [#?(:clj array?)]]
            [hyperfiddle.hfql2.utils :refer [imap?]]
            [dustingetz.str :as strx]
            [clojure.pprint]
            [dustingetz.loader :as loader]
            [hyperfiddle.ct.function :refer [flip]]
            [hyperfiddle.ct.maybe :refer [just nothing from-maybe]]
            [hyperfiddle.ct.either :refer [either right from-right]]
            [hyperfiddle.ct.comonad :refer [extract]]
            [hyperfiddle.ct.cofree :refer [unwrap]]
            [hyperfiddle.hfql2.analyzer :as ana]
            [hyperfiddle.hfql2.ast :as ast]
            [dustingetz.loader :refer [Offload]]
            [contrib.assert :refer [is]])
  #?(:cljs (:require-macros hyperfiddle.navigator6)))

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
   Returns ::pull, ::star, or nil (leaf)."
  [hfqlT]
  (cond
    (hfql/star? hfqlT) ::star
    (hfql/pull? hfqlT) ::pull
    (hfql/traversal? hfqlT) (if (hfql/multiplicity-many? hfqlT)
                              ::star ; collection traversal → render as collection
                              (effective-render-type (first (hfql/next-steps hfqlT))))
    :else nil))

(defn- associative-value?
  "True when the raw value at point is associative (record or map, not a collection)."
  [v]
  (or (record? v) (and (imap? v) (not (record? v)))
      #?@(:clj [(and (some? v) (not (instance? clojure.lang.IObj v)))]))) ; java object

(e/defn ResolveRenderer [hfqlT]
  (e/server
    (e/Offload #(when ((glitch-guard recursion-end?) hfqlT) ::recursion-end))
    (case (e/Offload #((glitch-guard effective-render-type) hfqlT))
      ;; Structured node (pull or star) — the outer step (possibly a traversal wrapper) is
      ;; passed to RenderMatrix. hfql/nav inside RenderMatrix executes any traversal IO.
      ;; pull → associative (single object, transposed), star → not associative (collection).
      ::pull (e/fn [stepT] (RenderMatrix stepT :associative? true))
      ::star (e/fn [stepT] (RenderMatrix stepT :associative? false))
      ;; Leaf (traversal with no structured child) → nav, suggest, render directly.
      ;; Compute step as plain data (either callbacks must not contain e/fn).
      (e/Reconcile
        (let [either-value ((glitch-guard hfql/nav*) hfqlT) ; runs IO
              value (either identity identity either-value)
              associative? (associative-value? value)
              stepT (either
                      (fn [suggestion-err]
                        (-> (hfql/hfql [*] (right suggestion-err))
                          ((glitch-guard hfql/set-parent) hfqlT)))
                      (fn [suggestedT]
                        (when (and (some? suggestedT) (not= hfqlT suggestedT))
                          (-> suggestedT
                            ((glitch-guard (flip hfql/seed))
                             (assoc ((glitch-guard hfql/scope) hfqlT) (symbol "%") (right value)))
                            ((glitch-guard hfql/set-parent) hfqlT))))
                      (hfql/suggest* value))]
          (if (some? stepT)
            (e/fn [_] (RenderMatrix stepT :associative? associative?))
            (e/fn [& _])))))))

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
           ;; this point is not selectable
           nil)))))

(defn- object-level-identity [obj]
  #?(:clj (System/identityHashCode obj)
     :cljs (goog/getUid obj)))

(e/defn Navigate* [sitemap stepT]
  (e/client
    (dom/div (dom/props {:style {:display :contents}}) ; HACK – presence of this div circumvent a crash where `mount-point` will call "parent.appendChild(nil)"
      (let [id (e/server (with-bindings* (or e/*bindings* {}) ; Inject bindings must be active before thunk forcing
                           (fn [] (hash [((glitch-guard hfql/id) stepT) ; HACK
                                         (object-level-identity ((glitch-guard hfql/value-at-point) stepT)) ; HACK to compensate for 𝓞(1) selection always hydrating the first instance of a multiplicity many result TODO revise.
                                         ]))))]
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
                              (e/server ((glitch-guard (flip inject-dynamic-bindings)) stepT e/*bindings*)))
                            (let [next-childT (e/server (hyperfiddle.navigator6.selection2/find-next-navigable-point sitemap #_stepT selection/*local-selection))] ; Navigation must prioritizes recursive children over regular children at point
                              (e/Reconcile
                                (when (e/server (some? next-childT))
                                  (e/server
                                    (binding [selection/*parent-selection-symbolic selection/*local-selection-symbolic
                                              selection/*parent-selection selection/*local-selection]
                                      (Navigate* sitemap next-childT)))))
                              (e/amb)))))))))))))))

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
              (Navigate* sitemap (e/server (is (with-bindings* (or e/*bindings* {}) resolve-route sitemap query)
                                               some? (str "Route not found in sitemap: " (pr-str (first query)))))))))))))

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
"
  )

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

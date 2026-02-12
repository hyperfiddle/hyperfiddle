(ns hyperfiddle.navigator6
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-forms5 :as forms]
            [hyperfiddle.api :as hf]
            [hyperfiddle.hfql2 :as hfql]
            [hyperfiddle.entrypoint2 :refer [Index]]
            [hyperfiddle.router5 :as router]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.ui.tooltip :as tooltip]
            [hyperfiddle.navigator6.state :as state :refer [with-local-state]]
            [hyperfiddle.navigator6.search :as search :refer [with-local-search]]
            [hyperfiddle.navigator6.selection :as selection :refer [with-local-selection]]
            [hyperfiddle.navigator6.selection2 :as selection2]
            [hyperfiddle.navigator6.matrix :refer [RenderObject RenderCollection]]
            [hyperfiddle.navigator6.utils :refer [rebooting freezing glitch-guard]]
            [contrib.css :as cssx]
            [contrib.data :refer [#?(:clj array?)]]
            [dustingetz.str :as strx]
            [clojure.pprint]
            [dustingetz.loader :as loader]
            [hyperfiddle.ct.function :refer [flip]]
            [hyperfiddle.ct.maybe :refer [just nothing from-maybe]]
            [hyperfiddle.ct.either :refer [either right]]
            [hyperfiddle.ct.comonad :refer [extract]]
            [hyperfiddle.ct.cofree :refer [unwrap]]
            [hyperfiddle.hfql2.analyzer :as ana]
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

(defn- render-type [hfqlT]
  (let [metas (hfql/metas hfqlT)]
    (or (and (hfql/multiplicity-many? hfqlT)
          ::star)
      (and (hfql/recursion-end?! hfqlT) ; will run IO
        ::recursion-end)
      (case (::ana/type metas)
        ::ana/star ::star
        ::ana/pull ::pull
        ::ana/traversal (render-type (first (hfql/next-steps hfqlT)))
        ::unknown))))

(e/defn ResolveRenderer [hfqlT]
  (e/server
    (get {::pull RenderObject
          ::star RenderCollection}
      (e/Offload #((glitch-guard render-type) hfqlT))
      (e/fn [& _]))))

(e/defn ReshapeNavigationStep [hfqlT]
  (e/server
    (e/Reconcile
      (case (e/Offload #((glitch-guard render-type) hfqlT))
        (::pull ::star) hfqlT
        ::recursion-end hfqlT
        (let [either-value ((glitch-guard hfql/nav*) hfqlT) ; runs IO
              value (either identity identity either-value)]
          (either
            (fn [suggestion-err]
              (-> (hfql/hfql [*] (right suggestion-err))
                ((glitch-guard hfql/set-parent) hfqlT)))
            (fn [suggestedT]
              (if (and (some? suggestedT) (not= hfqlT suggestedT))
                (-> suggestedT
                  ((glitch-guard (flip hfql/seed)) (assoc ((glitch-guard hfql/scope) hfqlT) (symbol "%") (right value)))
                  ((glitch-guard hfql/set-parent) hfqlT))
                hfqlT))
            (hfql/suggest* value)))))))

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
      (let [id (e/server (hash [(::ana/id ((glitch-guard hfql/metas) stepT)) ; HACK
                                (object-level-identity ((glitch-guard hfql/value-at-point) stepT)) ; HACK to compensate for 𝓞(1) selection always hydrating the first instance of a multiplicity many result TODO revise.
                                ]))]
        (rebooting id ; glitch guard
          (e/server
            (freezing [sitemap stepT]
              (e/Reconcile
                (e/When ((complement (fnil zero? 0)) id) ; guard navigate on nil
                  (with-local-state ::state/route ; injects *local-state and interpret state write commands
                    (with-local-search ::search   ; injects *local-search and interpret search commands
                      (let [stepT (e/server (ReshapeNavigationStep stepT))]
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
                              (e/amb))))))))))))))))

;; (e/defn RenderMeta [stepT]
;;   (e/server
;;     (when-some [m ((glitch-guard hfql/metas) stepT)]
;;       (dom/details
;;         (dom/props {:open true})
;;         (dom/summary (dom/text "Meta"))
;;         (dom/pre (dom/text (strx/pprint-str m)))))))


;;;;;;;;;;;;;;;;
;; Entrypoint ;;
;;;;;;;;;;;;;;;;

(declare css)
(e/defn HfqlRoot [sitemap entrypoints]
  (e/server
    (binding [e/*bindings* (#(or e/*bindings* {}))]
      (e/client
        (dom/style (dom/text css tooltip/css))
        (dom/link (dom/props {:rel :stylesheet, :href (e/server (str hf/*base-static-resource-path* "matrix.css"))}))
        (dom/link (dom/props {:rel :stylesheet, :href (e/server (str hf/*base-static-resource-path* "matrix_vertical.css"))}))
        (dom/link (dom/props {:rel :stylesheet, :href (e/server (str hf/*base-static-resource-path* "matrix_horizontal.css"))}))
        (Index sitemap entrypoints)
        (tooltip/TooltipArea
          (e/fn []
            (tooltip/Tooltip)
            (dom/div
              (dom/props {:class "Browser"})
              (let [query (normalize-router-query (first router/route))]
                (if-not query
                  (router/ReplaceState! ['. [(first entrypoints)]])
                  (do
                    (let [[f & _args] query]
                      (set! (.-title js/document)  (str (some-> f name (str " – ")) "Hyperfiddle"))
                      (dom/props {:class (cssx/css-slugify f)}))
                    (Navigate* sitemap (e/server (is (with-bindings* e/*bindings* resolve-route sitemap query)
                                                    some? (str "Route not found in sitemap: " (pr-str (first query))))))))))))))))


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
  (str forms/css
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

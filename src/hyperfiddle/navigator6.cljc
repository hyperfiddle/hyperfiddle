(ns hyperfiddle.navigator6
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-forms5 :as forms]
            [hyperfiddle.hfql2 :as hfql]
            [hyperfiddle.hfql2.protocols :refer [hfql-resolve]]
            [hyperfiddle.entrypoint2 :refer [Index]]
            [hyperfiddle.router5 :as router]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.ui.tooltip :as tooltip]
            [hyperfiddle.navigator6.state :as state :refer [with-local-state]]
            [hyperfiddle.navigator6.search :as search :refer [with-local-search]]
            [hyperfiddle.navigator6.selection :as selection :refer [with-local-selection]]
            [hyperfiddle.navigator6.matrix :refer [RenderMatrix]]
            [hyperfiddle.navigator6.utils :refer [rebooting freezing glitch-guard]]
            [contrib.css :as cssx]
            [contrib.data :refer [#?(:clj array?)]]
            [dustingetz.str :as strx]
            [clojure.pprint]
            [dustingetz.loader :as loader]
            [hyperfiddle.ct.function :refer [flip]]
            [hyperfiddle.ct.maybe :refer [just nothing from-maybe]]
            [hyperfiddle.ct.comonad :refer [extract]]
            [hyperfiddle.ct.cofree :refer [unwrap]]
            [hyperfiddle.hfql2.analyzer :as ana]
            [dustingetz.loader :refer [Offload]])
  #?(:cljs (:require-macros hyperfiddle.navigator6)))

(defn- augment-scope-with-%-from-route-args [args stepT]
  (if (seq args)
    (let [scope (hfql/scope stepT)
          % (hfql-resolve (first args))] ; TODO move to ednish?
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

(defn- multiplicity-many-value? [object] ; TODO promote to HFQL
  ;; `coll?` is not enough. E.g. datomic.api.EntityMap is `coll?` but isn't `map?`.
  (or (sequential? object) (set? object) #?(:clj (contrib.data/array? object))))

;; TODO refactor this fn. Logic is convoluted.
(letfn [(select-recursion-point [stepT] ":: stepT → Maybe (stepT | nil)" ; because we want a different default based on no stepT vs no children
          (if stepT
            (if (::ana/recursion-point (hfql/metas stepT))
              (if-let [recursive-child (first (filter (comp ::ana/recursive-child hfql/metas) (unwrap stepT)))]
                (just (if (empty? (unwrap recursive-child))
                        nil
                        recursive-child))
                (just stepT))
              (just stepT))
            nothing))
        (expand-collection [stepT]
          (let [val-at-point (hfql/nav stepT)
                recursive? (::ana/recursion-point (hfql/metas stepT))
                can-recur? (>= (::ana/recursion-limit (hfql/metas stepT) 0) 1)
                many? (multiplicity-many-value? val-at-point)
                ]
            (->
              (cond
                (and recursive? can-recur? many?) (from-maybe (hfql/hfql {* [*]} val-at-point) (select-recursion-point stepT))
                (and recursive? (not can-recur?) many?) (hfql/hfql {* [*]} val-at-point) ; loses static columns from recursion
                (and (not recursive?) many?) (hfql/hfql {* [*]} val-at-point)
                (not many?) stepT
                :else (throw (ex-info "Unknown case" {:state [recursive? can-recur? many?]})))
              (hfql/set-parent stepT))
            ))]
  (defn- select-next-child [sitemap stepT [column rowT]]
    (if rowT
      (or (when column
            (try (expand-collection (or (hfql/find-child column (try (hfql/augment-with-suggestions! rowT)
                                                                     (catch #?(:clj Throwable, :cljs :default) t
                                                                       #?(:clj (clojure.tools.logging/debug t))
                                                                       rowT)))
                                      rowT))
                 (catch #?(:clj Throwable, :cljs :default) _ (prn _))))
        (some->> (resolve-route sitemap (normalize-router-query (::hfql/select (hfql/metas rowT))))
          (hfql/seed (hfql/scope rowT)))
        (from-maybe rowT (select-recursion-point rowT)))
      (when column
        (try (hfql/find-child column (hfql/augment-with-suggestions! stepT)) (catch #?(:clj Throwable, :cljs :default) _ nil))))))

(defn- render-type [hfqlT]
  (let [metas (hfql/metas hfqlT)]
    (if (= ::ana/many (::ana/multiplicity metas))
      ::star
      (case (::ana/type metas)
        ::ana/star ::star
        ::ana/pull ::pull
        ::ana/traversal (render-type (first (unwrap hfqlT)))
        ::ana/ifn ::ifn
        ::unknown))))

(e/defn ResolveRenderer [hfqlT]
  (e/server
    (if hfqlT
      (get {::pull (e/fn [stepT] (RenderMatrix stepT :transpose? true, :single? true))
            ::star RenderMatrix}
        (e/Offload #((glitch-guard render-type) hfqlT))
        (e/fn [& _]))
      (e/fn [& _]))))


(e/defn ReshapeNavigationStep [hfqlT]
  (e/server
    (e/Reconcile
      (case (e/Offload #((glitch-guard render-type) hfqlT))
        (::pull ::star) hfqlT
        (let [value ((glitch-guard hfql/nav) hfqlT)
              suggestedT (hfql/suggest value)
              newStepT (-> suggestedT
                         ((glitch-guard (flip hfql/seed)) (assoc ((glitch-guard hfql/scope) hfqlT) '% value))
                         ((glitch-guard hfql/set-parent) hfqlT))]
          (e/Reconcile
            (if (#(and (some? suggestedT) (not= hfqlT suggestedT)))
              newStepT
              hfqlT)))))))

(defn inject-dynamic-bindings [e-bindings stepT]
  (hfql/update-scope stepT update `hfql/*bindings* merge e-bindings))

(e/defn Navigate [sitemap stepT]
  (e/client
    (dom/div (dom/props {:style {:display :contents}}) ; HACK – presence of this div circumvent a crash where `mount-point` will call "parent.appendChild(nil)"
      (let [id (e/server (hash [(::ana/id ((glitch-guard hfql/metas) stepT)) ; HACK
                                ((glitch-guard hfql/value-at-point) stepT) ; HACK to compensate for 𝓞(1) selection always hydrating the first instance of a multiplicity many result TODO revise.
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
                            ;; Today we find "blue" rows by scanning the collection/object at point in 𝓞(n). We have an opportunity to resolve in 𝓞(1), e.g. entities.
                            (e/server #(selection/resolve-selected-object-BIG-O-N % e/*bindings* ((glitch-guard selection/get-rows-container) stepT)))
                          (e/amb
                            (e/call (ResolveRenderer stepT) ; call renderer for current point
                              (e/server ((glitch-guard (flip inject-dynamic-bindings)) stepT e/*bindings*)))
                            (let [next-childT (e/server (do #_(prn "local selection" selection/*local-selection)
                                                            (select-next-child sitemap stepT selection/*local-selection)))] ; Navigation prioritizes recursive children over regular children at point
                              (e/Reconcile
                                (when (e/server (some? next-childT))
                                  (e/server
                                    (binding [selection/*parent-selection selection/*local-selection]
                                      (Navigate sitemap next-childT)))))
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
                    (Navigate sitemap (e/server (with-bindings* e/*bindings* resolve-route sitemap query)))))))))))))


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

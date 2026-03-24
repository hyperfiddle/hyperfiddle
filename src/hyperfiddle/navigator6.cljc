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
            [hyperfiddle.navigator6.sorting :as sort :refer [with-local-sort]]
            [hyperfiddle.navigator6.arg :as arg :refer [with-local-arg]]
            [hyperfiddle.navigator6.selection :as selection :refer [with-local-selection]]
            [hyperfiddle.navigator6.selection2 :as selection2]
            [hyperfiddle.navigator6.matrix :refer [RenderMatrix]]
            [hyperfiddle.navigator6.utils :as utils :refer [in rebooting glitch-guard]]
            [dustingetz.css :as cssx]
            [dustingetz.data :refer [#?(:clj array?)]]
            [hyperfiddle.hfql2.utils :refer [imap?]]
            [dustingetz.str :as strx]
            [dustingetz.loader :as loader :refer [Offload]]
            [hyperfiddle.ct.function :refer [flip]]
            [hyperfiddle.ct.either :refer [either right left? from-left from-right]]
            [hyperfiddle.ct.comonad :refer [extract]]
            [hyperfiddle.ct.cofree :refer [unwrap]]
            [dustingetz.check :refer [is]]
            #?(:clj [clojure.tools.logging :as log]))
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
                              ;; Recursive traversals have self-similar children — no inner
                              ;; pull/star to find. Route to SuggestAndRender instead of SO.
                              (if (hfql/recursion-point? hfqlT)
                                nil
                                (effective-render-type (first (hfql/next-steps hfqlT)))))
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
        ;; Errors (Left) skip suggestion — exceptions aren't meaningful values to
        ;; suggest columns from, and suggest-and-graft would graft onto the static
        ;; sitemap context (wrong selection targets). Error path severs the tree.
        stepT (#(if (left? either-value)
                  (-> (hfql/hfql [*] (from-left nil either-value))
                    ((glitch-guard hfql/set-parent) hfqlT)
                    ;; Flag error so RenderMatrix applies red-table CSS (matrix.cljc:333).
                    (vary-meta assoc ::hfql/error (from-left nil either-value)))
                  ((glitch-guard suggest-and-graft) hfqlT)))]
    (if (some? stepT)
      (e/fn [_] (RenderMatrix stepT :associative? associative?))
      (e/fn [& _]))))

(e/defn ResolveRenderer [hfqlT]
  (e/server
    (e/Reconcile
      ;; IO errors (e.g. *db* nil) are silently converted to empty collections
      ;; by the interpreter's star/pull traversal. Check nav* first: if Left,
      ;; return ::left so the default branch (SuggestAndRender) renders a red table.
      (case (e/Offload #(if (some-> hfqlT hfql/nav* left?)
                           ::left
                           ((glitch-guard effective-render-type) hfqlT)))
        ::pull (e/fn [stepT] (RenderMatrix stepT :associative? true))
        ::star (e/fn [stepT] (RenderMatrix stepT :associative? false))
        ::recursion-end (e/Reconcile (SuggestAndRender hfqlT))
        (e/Reconcile (SuggestAndRender hfqlT))))))

(defn inject-dynamic-bindings [e-bindings stepT]
  (hfql/update-scope stepT update `hfql/*bindings* merge e-bindings))

#?(:clj
   (defn- seed-step-with-args
     "Seed unbound HFQL parameters from the arg map into stepT's scope.
      Needed in two places: resolve-fn (selection resolution on click) and
      NavigationCard body (rendering). Without this the resolve-fn operates
      on the raw stepT and partial calls fail with 'Unbound HFQL parameter'."
     [stepT arg-map]
     (if (and (some? arg-map) (map? arg-map))
       (try
         ;; Seed raw values as-is. Hydration (e.g. '*db* → <db>) is the
         ;; interpreter's job via ::hfql/defaults, not the navigator's.
         (reduce-kv
           (fn [st sym raw-val]
             (hfql/update-scope st assoc sym (right raw-val)))
           stepT
           arg-map)
         (catch Exception _ stepT))
       stepT)))

#?(:clj
   (defn- resolve-selection-coordinates [current-point symbolic-selection-coordinates bindings]
     (with-bindings bindings
       (let [selectable-point (hyperfiddle.navigator6.selection2/find-selectable-point current-point)]
         (or (and (some? selectable-point)
               (not (left? (hfql/nav* current-point))) ;; IO errors skip static tree (children have ::hfql/select
                                                       ;; pointing to sitemap targets like attribute-entity-detail).
                                                       ;; Fall through to suggest-and-graft which discovers exception
                                                       ;; properties for drill-down as plain data.
                                                       ;; nav* result is cached by mem-reader — this is a cache hit.
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
                           (fn [] (try (hash [((glitch-guard hfql/id) stepT) ; HACK
                                              ;; Partial steps can't execute — use nil as identity component.
                                              (when ((glitch-guard hfql/fullfilled?) stepT)
                                                (object-level-identity ((glitch-guard hfql/value-at-point) stepT))) ; HACK to compensate for 𝓞(1) selection always hydrating the first instance of a multiplicity many result TODO revise.
                                              ])
                                       (catch Throwable t
                                         #?(:clj (log/warn t "Navigate* id computation failed"))
                                         (object-level-identity stepT))))))]
        (rebooting id ; glitch guard
          (let [sitemap (e/server (e/Snapshot sitemap))
                stepT (e/server (e/Snapshot stepT))]
            (e/Reconcile
              (e/When ((complement (fnil zero? 0)) id) ; guard navigate on nil
                (with-local-state ::state/route ; injects *local-state and interpret state write commands
                  (with-local-search ::search   ; injects *local-search and interpret search commands
                    (with-local-sort        ; injects *sort-spec* and intercepts Sort! commands
                      (with-local-arg ::arg        ; injects *arg and interpret SetArg! commands
                        (with-local-selection ::selection ; injects *local-selection and interpret selection commands
                          ;; Seed stepT with arg/*arg before selection resolution —
                          ;; otherwise partial calls (e.g. d/query) fail with
                          ;; "Unbound HFQL parameter" because the raw stepT
                          ;; has no arg bindings in its scope yet.
                          ;; clojure.core/meta checked first: selection2's resolve always
                          ;; returns the row-point fallback (even when cell isn't found),
                          ;; so the `or` fallback in resolve-selection-coordinates is
                          ;; unreachable. Metadata synthesis must run before normal resolution.
                          (e/server #(try (let [;; Inject DI bindings into HFQL scope so the interpreter's
                                                  ;; defaults pipeline can hydrate symbolic args (e.g. '*db* → <db>).
                                                  ;; Must run before seed-step-with-args — same order as card body.
                                                  stepT ((glitch-guard (flip inject-dynamic-bindings)) stepT (or e/*bindings* {}))
                                                  stepT (seed-step-with-args stepT arg/*arg)]
                                            (or (when (= 'clojure.core/meta (first %))
                                                  (with-bindings (or e/*bindings* {})
                                                      ;; Create (hfql meta) and forward the parent scope —
                                                      ;; the tree applies clojure.core/meta to % from parent,
                                                      ;; giving it a proper form for title derivation.
                                                      (when ((glitch-guard hfql/fullfilled?) stepT)
                                                        ;; Pass nav result as body — the hfql macro binds it as %,
                                                        ;; so (meta %) resolves naturally. The form `clojure.core/meta`
                                                        ;; gives point-symbolic-name a proper title.
                                                        (let [nav-result ((glitch-guard hfql/nav) stepT)]
                                                          (when (not-empty (dissoc (meta nav-result)
                                                                             :hyperfiddle.hfql2.protocols/obj))
                                                            (hfql/hfql clojure.core/meta nav-result))))))
                                              ((glitch-guard resolve-selection-coordinates) stepT % e/*bindings*)))
                                          (catch Throwable t
                                            #?(:clj (log/warn t "Selection resolution failed"))
                                            nil)))
                          ;; Hoist next-childT above e/amb so NavigationCard can receive has-child?.
                          ;; This lets the three-state card collapse toggle (indeterminate/open/closed) know
                          ;; the positional default before the user interacts.
                          (let [next-childT (e/server
                                              ((fn []
                                                 ;; with-bindings needed so column-level identity
                                                 ;; resolution (hfql-resolve*) has access to *db* etc.
                                                 (try (with-bindings (or e/*bindings* {})
                                                        (hyperfiddle.navigator6.selection2/find-next-navigable-point sitemap #_stepT selection/*local-selection))
                                                      (catch #?(:clj Throwable :cljs :default) t
                                                        #?(:clj (log/warn t "find-next-navigable-point failed"))
                                                        nil)))))
                                ;; Injected stepT — hoisted so nav-err? Offload and ResolveRenderer
                                ;; share the same Store object → mem-reader cache hit on second nav*.
                                ;; Excludes *local-search* for ::pull views to avoid cascade cache bust
                                ;; on search changes (see plans/associative-search-cell-recomputation.md).
                                injected (e/server (let [bindings (#(if (= ::pull ((glitch-guard effective-render-type) stepT))
                                                                      (dissoc e/*bindings* (find-var `search/*local-search))
                                                                      e/*bindings*))
                                                         stepT ((glitch-guard (flip inject-dynamic-bindings)) stepT bindings)
                                                         stepT (seed-step-with-args stepT arg/*arg)]
                                                     stepT))
                                ;; Reboot key: error state of injected step. When nav* flips Left↔Right,
                                ;; rebooting destroys NavigationCard (including chrome div) — bypasses
                                ;; Electric mount-point cleanup bug where redirected chrome DOM persists.
                                nav-err? (e/server (e/Offload #(some-> injected hfql/nav* left?)))
                                ;; Disable sort/search handles when a child card is visible (not just
                                ;; selected — selection may fail to resolve). Replaces (some? *local-selection).
                                has-child? (e/server (some? next-childT))]
                            (binding [sort/*disabled?* has-child?]
                              (rebooting nav-err?
                                (e/amb
                                  ;; Branch 1: card chrome + matrix — sibling of child, not wrapper.
                                  ;; NavigationCard renders into the display:contents div, becoming a
                                  ;; direct child of .Browser alongside child cards.
                                  (NavigationCard *navigation-depth* has-child?
                                    (e/fn []
                                      (let [is-partial? (e/server (hfql/partial? stepT))
                                            ;; Metadata pill — appears when the result carries Clojure metadata.
                                            ;; hfql/nav is memoized (mem-reader) — cache hit after RenderMatrix's Offload.
                                            has-meta? (e/server (#(try (when ((glitch-guard hfql/fullfilled?) injected)
                                                                         (some? (not-empty
                                                                                  (dissoc (meta ((glitch-guard hfql/nav) injected))
                                                                                    :hyperfiddle.hfql2.protocols/obj))))
                                                                       (catch #?(:clj Throwable :cljs :default) _ false))))
                                            ;; Row coordinate for meta selection — uses selection2 for proper identification
                                            meta-row (e/server (#(or (try (when-some [selectable (selection2/find-selectable-point injected)]
                                                                            (second (selection2/symbolic-selection-coordinates selectable nil)))
                                                                          (catch #?(:clj Throwable :cljs :default) _ nil))
                                                                   '(hfql/current-point))))]
                                        (e/amb
                                          ;; Parameter form — one field per call arg (free=editable, bound=disabled).
                                          ;; Skip when % is the only param — implicit body value, not user-facing.
                                          (when is-partial?
                                            (let [params (e/server (hfql/form-parameters stepT))
                                                  ;; Defaults fn returns symbolic, serializable values (e.g. '*db*).
                                                  ;; with-bindings* so it can access DI vars for computation.
                                                  ;; No hydration here — symbols cross the wire as-is.
                                                  defaults (e/server (with-bindings* (or e/*bindings* {})
                                                                       #(try (hfql/form-defaults injected)
                                                                             (catch Throwable _ nil))))]
                                              (when-not (e/server (and (= 1 (count params))
                                                                    (= '% (:name (first params)))))
                                                (arg/ArgInput! params :defaults defaults))))
                                          ;; (i) metadata pill in card chrome — navigates to (meta value) on click.
                                          ;; Dispatches [meta row-form] as selection coordinates.
                                          ;; The resolve-fn detects `meta` and synthesizes a child card.
                                          (when has-meta?
                                            (in utils/*card-chrome*
                                              (dom/div
                                                (dom/props {:class "meta-pill"
                                                            ;; Tooltip shows pretty-printed metadata for quick inspection
                                                            :data-tooltip (e/server (rendering/format-query-meta
                                                                                      (meta ((glitch-guard hfql/nav) injected))))})
                                                (dom/text "^meta")
                                                (let [[t _err] (e/Token (dom/On "click"
                                                                          (fn [^js e]
                                                                            (.stopPropagation e) ; don't toggle card collapse
                                                                            e) ; return event object — unique per click so e/Token fires each time
                                                                          nil {}))]
                                                  (e/When t [t [`state/ReplaceState! assoc ::selection ['clojure.core/meta meta-row]]])))))
                                          (e/call (ResolveRenderer injected)
                                            injected)))))
                                  ;; Branch 2: child navigation — sibling of card, gets full viewport width.
                                  ;; InterceptCommand converts child's CloseCard! → SetSelection! nil so
                                  ;; this card's with-local-selection clears selection → child unmounts.
                                  (e/Reconcile
                                    (e/When has-child?
                                      (binding [selection/*parent-selection-symbolic selection/*local-selection-symbolic
                                                selection/*parent-selection selection/*local-selection
                                                *navigation-depth* (inc *navigation-depth*)]
                                        (utils/InterceptCommand `selection/CloseCard!
                                          (e/fn [_] [`selection/SetSelection! [nil (e/fn [])]])
                                          (Navigate* sitemap next-childT))))))))))))))))))))))

(e/defn NavigationCard [depth has-child? Body]
  (e/client
    (dom/div
      (dom/props {:class "hyperfiddle-navigation-card"
                  :style {:--navigation-depth depth}
                  :data-depth depth
                  :role "region"
                  :aria-label (str "Navigation level " (inc depth))
                  :tabindex "0"}) ; TAB-navigable for keyboard focus and ESC handling

      ;; Track which card has focus, persisting through window blur.
      ;; CSS :focus-within can't do this — it clears when the browser window
      ;; loses focus (e.g. switching to another OS app). We need the indicator
      ;; to persist so the user knows which card ESC will close.
      ;; dom/Focused-in? with a custom handler returns nil (= no state change,
      ;; via squarewave's some->) when relatedTarget is null (window blur).
      (let [card-node dom/node
            focused? (dom/Focused-in? card-node
                       (fn [event-type ^js event]
                         ;; relatedTarget = where focus came from (focusin) or is going to
                         ;; (focusout). null when the browser window itself loses focus.
                         (let [related (.-relatedTarget event)]
                           (cond
                             ;; Focus moved between elements inside this card — no change
                             (and (some? related) (.contains card-node related)) nil
                             ;; Focus entered this card from outside
                             (= "focusin" event-type) true
                             ;; Focus left to a specific element outside this card
                             (and (= "focusout" event-type) (some? related)) false
                             ;; Window blur (relatedTarget is null) — retain current state
                             :else nil))))]
        (dom/props {:data-focused (str focused?)}))

      (let [chrome (dom/div
                     (dom/props {:class "navigation-card__chrome"})
                     ;; Chrome click toggles card visibility.
                     ;; Tri-state via aria-expanded: absent = CSS positional default,
                     ;; "true" = user wants expanded, "false" = user wants collapsed.
                     ;; Skip for inputs and buttons (search field, close button).
                     (dom/On "click"
                       (fn [^js e]
                         (when-not (or (.closest (.-target e) "input")
                                       (.closest (.-target e) "button"))
                           (let [card (.closest (.-currentTarget e) ".hyperfiddle-navigation-card")
                                 current (.getAttribute card "aria-expanded")]
                             (.setAttribute card "aria-expanded"
                               (case current
                                 "true"  "false"
                                 "false" "true"
                                 "true")))))
                       nil {})
                     dom/node)]
        ;; All command sources merged via e/amb as the LAST expression of the
        ;; outer dom/div. Electric's do-semantics drain all but the last expr,
        ;; so commands must live here to propagate out of NavigationCard.
        (e/amb
          ;; Close button click → CloseCard! (delegated on chrome node).
          ;; CloseCard! propagates past this card's with-local-selection up to the
          ;; parent, where InterceptCommand converts it to SetSelection! nil →
          ;; parent clears selection → this card unmounts.
          (e/When (pos? depth)
            (in chrome
              (dom/button
                (dom/props {:class "navigation-card__close-button"
                            :aria-label "Close"
                            :title "Close (ESC)"})
                (dom/text "\u2715")
                (let [[t _err] (e/Token (dom/On "click"
                                          (fn [^js e]
                                            (.preventDefault e)
                                            (.stopPropagation e)
                                            true)
                                          nil {}))]
                  (e/When t [t [`selection/CloseCard!]])))))
          ;; Mousedown on non-focusable areas → focus this card.
          ;; Updates the data-focused indicator so the user knows which card ESC targets.
          (dom/On "mousedown"
            (fn [^js e]
              (when-not (or (instance? js/HTMLInputElement (.-target e))
                            (instance? js/HTMLButtonElement (.-target e))
                            (instance? js/HTMLAnchorElement (.-target e))
                            (.closest (.-target e) "[tabindex]"))
                (.focus dom/node)))
            nil {})

          ;; ESC key → CloseCard! (same rationale as close button)
          (e/When (pos? depth)
            (let [[t _err] (e/Token (dom/On dom/node "keydown"
                                      (fn [^js e]
                                        (when (= "Escape" (.-key e))
                                          (.preventDefault e)
                                          (.stopPropagation e)
                                          true))
                                      nil {}))]
              (e/When t [t [`selection/CloseCard!]])))

          ;; Content area — commands from Body propagate through e/amb.
          ;; Inner wrapper ensures the grid always has 1 child — the 0fr→1fr
          ;; collapse animation breaks with multiple children (padding inflates
          ;; implicit grid rows past 0).
          (dom/div
            (dom/props {:class "navigation-card__content"})
            (dom/div
              (binding [utils/*card-chrome* chrome]
                (Body)))))))))


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
body:has(.Browser) { margin: 0; font-family: 'Open Sans', Arial, Verdana, sans-serif; background-color: rgb(248 250 252); }
html:has(.Browser) { scrollbar-gutter: stable; /* native page-level scroll is authoritative */ }
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

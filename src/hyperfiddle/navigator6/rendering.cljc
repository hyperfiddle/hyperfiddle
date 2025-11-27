(ns hyperfiddle.navigator6.rendering
  (:require [clojure.string :as str]
            [clojure.pprint :refer [cl-format]]
            [contrib.data :refer [#?(:clj array?)]]
            [dustingetz.str :as strx]
            [geoffreygaillard.debug-tools :refer [debug-exceptions]]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.hfql2.analyzer :as ana :refer [unquote*]]
            [hyperfiddle.ct.cofree :refer [unwrap]]
            [hyperfiddle.hfql2 :as hfql]
            [hyperfiddle.navigator6.extensions :as extensions]
            [hyperfiddle.navigator6.selection :as selection]
            [hyperfiddle.navigator6.utils :as utils :refer [glitch-guard Swallow-empty-diffs]]
            [hyperfiddle.router5 :as router]))

(e/declare *server-pretty)

#?(:clj (defn- map-keep-coll [f coll]
          (debug-exceptions `map-keep-coll
            (let [xform (comp (take 1) (map f))
                  coll-of-same-type (into (empty coll) xform coll)]
              (cond
                (vector? coll) coll-of-same-type
                (reversible? coll) (rseq coll-of-same-type)
                (seq? coll) (reverse coll-of-same-type)
                :else coll-of-same-type
                )))))

#?(:clj (defn str-insert-before-last[^String s ^Character c]
          (let [sb (StringBuilder. s)
                len (.length sb)]
            (.insert sb (dec len) c)
            (.toString sb))))

#?(:clj
   (defn- str-inline-coll [coll server-pretty]
     (try
       (debug-exceptions `str-inline-coll
         (let [coll-count (count coll)
               base (binding [*print-length* 1, *print-level* 2]
                      ;; `symbol` removes double quotes from the strings inside the collection
                      (if (map? coll)
                        (if (empty? coll)
                          "{}"
                          (let [[[k v] _] (seq coll)]
                            (str "{"
                              ((get server-pretty (class k) strx/pprint-str) k)
                              ((get server-pretty (class v) strx/pprint-str) v)
                              "}")))
                        (-> (map-keep-coll #(symbol ((get server-pretty (class %) strx/pprint-str) %)) coll)
                          str
                          (str/replace (str \newline) (str \space))
                          (str/trim))))]
           (cond-> base
             (> coll-count 1) (str-insert-before-last \…)
             (> coll-count 1) (str " " coll-count " elements")
             )))
       (catch Throwable t ; glitch guard
         (ex-message t)))))

#?(:clj
   (defn- pretty-value [x server-pretty]
     (debug-exceptions `pretty-value
       (case x
         ::not-found ""
         ::empty-cell ""
         (if (or (set? x) (sequential? x) (array? x) (map? x))
           (str-inline-coll x server-pretty)
           ((get server-pretty (class x) pr-str) x))))))

(defn as-coll [v] (if (or (array? v) (coll? v)) v [v])) ; TODO removeme, glitch guard

(defn relativize-link-path [symbolic-link-href]
  (if (vector? symbolic-link-href)
    symbolic-link-href
    ['. [symbolic-link-href]]))

(e/defn RenderInlineColl [_object edge value]
  (let [pretty-v (e/Offload #(str-inline-coll (as-coll value) *server-pretty))
        symbolic-link-href (::hfql/link (meta edge))]
    (e/Reconcile
      (if (Swallow-empty-diffs (some? symbolic-link-href))
        (router/link (relativize-link-path symbolic-link-href) (dom/text pretty-v))
        (dom/text pretty-v))) ))

(e/defn Resolve [fq-sym fallback] (get e/*exports* fq-sym fallback))

(e/defn DefaultColumnHeaderTooltip [object edgeT value]
  (e/server (str (hfql/describe-formatted edgeT))))

(e/defn RenderTooltip* [tooltip-ident object edgeT value]
  (let [tooltip-qualified-ident (hyperfiddle.hfql2.analyzer/unquote* tooltip-ident) ; FIXME syntax wart
        Tooltip (Resolve tooltip-qualified-ident (get {::hfql/ColumnHeaderTooltip DefaultColumnHeaderTooltip} tooltip-ident))]
    (e/Reconcile
      (when (Swallow-empty-diffs (some? Tooltip))
        (let [show? (dom/Mouse-over?)]
          (dom/props
            {:data-tooltip
             (e/Reconcile (when show? (Tooltip object edgeT value)))}))))) )

(e/declare *rowT)

(e/defn RenderTooltip [tooltip-kind-ident object edgeT value]
  (RenderTooltip* (get ((glitch-guard hfql/metas) edgeT) tooltip-kind-ident
                    (get ((glitch-guard hfql/metas) *rowT) tooltip-kind-ident
                      tooltip-kind-ident))
    object edgeT value))

(defn template-link [denv symbolic-link]
  (let [path-elements #{'/ '.. '.}]
    (if (vector? symbolic-link)
      (mapv (fn [path-segment]
              (if (path-elements path-segment)
                path-segment
                (template-link denv path-segment)))
        symbolic-link)
      (if (coll? symbolic-link)
        (replace denv symbolic-link)
        (get denv symbolic-link symbolic-link)))))

(defn- link-href [object edgeT value]
  (let [denv {'% (hfql/identify object), (hfql/symbolic-edge edgeT) (hfql/identify value), '%v (or (hfql/identify value) value)}]
    (when-let [symbolic-link-href (unquote* (::hfql/link ((glitch-guard hfql/metas) edgeT)))]
      (template-link denv symbolic-link-href))))

(e/defn DefaultCellRenderer [object edgeT value]
  (RenderTooltip ::hfql/Tooltip object edgeT value)
  (let [pretty-v (e/Offload #(pretty-value value *server-pretty))]
    (e/Reconcile
      #_(e/client (e/drain (e/Tap-diffs (.setAttribute dom/node "data-empty" (e/server (= ::not-found value)))))) ; FIXME not showing up
      (let [link-href (link-href object edgeT value)]
        (if (Swallow-empty-diffs (some? link-href))
          (router/link (relativize-link-path link-href) (dom/text pretty-v))
          (dom/text pretty-v))))))

(e/declare Render)
(e/defn RenderCell [object edgeT value]
  (binding [Render DefaultCellRenderer]
    (e/Reconcile (e/call (Resolve (unquote* (::hfql/Render ((glitch-guard hfql/metas) edgeT))) ; FIXME syntax wart
                           Render) object edgeT #_(vary-meta edge dissoc ::hfql/Render) value))))

(defn- get-leafT [stepT] ; TODO questionable logic
  ;; Idea here is to display "leaf values" in the following case:
  ;; [{fs/jfile-kind name}] => "dir" instead of `::fs/dir`.
  (case (::ana/multiplicity (hfql/metas stepT))
    ::ana/many stepT
    (or (first (unwrap stepT)) stepT)))

(e/defn Render [object edge value] (RenderCell object edge value))
(e/defn Render* [stepT]
  (Render (e/Offload #(force ((glitch-guard hfql/value-at-point) stepT)))
    stepT
    (e/Offload #(if stepT
                  (hfql/nav (get-leafT stepT))
                  ::empty-cell))))



(defn format-collection-count
  ;; FIXME Today we can't differentiate collections strictly equal to
  ;; `COLLECTION-LIMIT` from collections larger than `COLLECTION-LIMIT`.
  [coll-count COLLECTION-LIMIT]
  {:pre [(number? coll-count) (number? COLLECTION-LIMIT)]
   :post [(string? %)]}
  (debug-exceptions `format-collection-count
    (let [reached-max-in-memory-lenght? (>= coll-count COLLECTION-LIMIT)]
      (cl-format nil "(~:[~;≥~]~d item~:p~:[~; · truncated~])"
        reached-max-in-memory-lenght? coll-count reached-max-in-memory-lenght?))))

(defn format-map-entries-count [coll-count COLLECTION-LIMIT]
  (-> (format-collection-count coll-count COLLECTION-LIMIT)
    (str/replace "items" "entries")
    (str/replace "item" "entry")))

(defn format-query-meta [query-meta] (some-> query-meta not-empty (dustingetz.str/pprint-str :print-length 10 :print-level 15)))




#?(:clj (defn- table-title-tooltip-content [stepT]
          {:post [(or (string? %) (nil? %))]}
          (when stepT
            (-> (hfql/describe-formatted stepT)
              (str "\n\n" (format-query-meta (meta (hfql/nav stepT))))
              (str/trim)
              (strx/blank->nil)))))

(defn- render-step-title [stepT *parent-selection]
  (pr-str (or
            (some-> stepT hfql/symbolic-edge-computed)
            (let [[col rowT] *parent-selection] col)
            (when-let [map-entry-pointT (and stepT (hfql/lookup ::ana/form-resolved stepT `val))]
              (hfql/identify (hfql/value-at-point map-entry-pointT)))
            (when-let [parentT (and stepT (hfql/parent stepT))]
              (hfql/identify (hfql/value-at-point parentT)))
            )))

(e/defn Step-Title [stepT]
  (dom/span
    (dom/props {:class "title"
                :style {:order -3}
                :data-tooltip (e/server (table-title-tooltip-content stepT))})
    (dom/text (e/server (render-step-title stepT selection/*parent-selection)))))

(comment
  (hfql/describe-column (get dustingetz.namespace-explorer/sitemap `all-ns))
  )
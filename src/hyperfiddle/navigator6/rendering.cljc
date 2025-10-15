(ns hyperfiddle.navigator6.rendering
  (:require [clojure.string :as str]
            [contrib.data :refer [#?(:clj array?)]]
            [dustingetz.str :as strx]
            [geoffreygaillard.debug-tools :refer [debug-exceptions]]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.hfql1.analyzer :as ana :refer [unquote*]]
            [hyperfiddle.hfql1.cofree :refer [unwrap]]
            [hyperfiddle.hfql2 :as hfql]
            [hyperfiddle.navigator6.extensions :as extensions]
            [hyperfiddle.navigator6.utils :as utils :refer [glitch-guard]]
            [hyperfiddle.router5 :as router]))

(e/declare *server-pretty)

#?(:clj
   (defn- pretty-value [x server-pretty]
     (debug-exceptions `pretty-value
       (case x
         ::not-found ""
         ::empty-cell ""
         ((get server-pretty (class x) pr-str) x)))))

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

#?(:clj
   (defn- str-inline-coll [coll server-pretty]
     (try
       (debug-exceptions `str-inline-coll
         (let [coll-count (count coll)
               base (binding [*print-length* 1, *print-level* 2]
                      ;; `symbol` removes double quotes from the strings inside the collection
                      (-> (map-keep-coll #(symbol ((get server-pretty (class %) strx/pprint-str) %)) coll)
                        str
                        (str/replace (str \newline) (str \space))
                        (str/trim)))]
           (cond-> base (> coll-count 1) (str " " coll-count " elements"))))
       (catch Throwable t ; glitch guard
         (ex-message t)))))


(defn as-coll [v] (if (or (array? v) (coll? v)) v [v])) ; TODO removeme, glitch guard

(e/defn RenderInlineColl [_object edge value]
  (let [pretty-v (str-inline-coll (as-coll value) *server-pretty)
        symbolic-link-href (::hfql/link (meta edge))]
    (e/Reconcile
      (if (some? symbolic-link-href)
        (router/link ['. [symbolic-link-href]] (dom/text pretty-v))
        (dom/text pretty-v))) ))

(e/defn Resolve [fq-sym fallback] (get e/*exports* fq-sym fallback))

(e/defn RenderTooltip [tooltip-ident object edgeT value]
  (let [tooltip-ident (get ((glitch-guard hfql/metas) edgeT) tooltip-ident nil)
        tooltip-qualified-ident (hyperfiddle.hfql1.analyzer/unquote* tooltip-ident) ; FIXME syntax wart
        Tooltip (Resolve tooltip-qualified-ident nil)]
    (e/Reconcile
      (when (some? Tooltip)
        (let [show? (dom/Mouse-over?)]
          (dom/props
            {:data-tooltip
             (e/Reconcile (when show? (Tooltip object edgeT value)))}))))) )

(e/defn DefaultCellRenderer [object edgeT value]
  (RenderTooltip ::hfql/Tooltip object edgeT value)
  (e/Reconcile
    (if (#(or (set? value) (sequential? value) (array? value)))
      (RenderInlineColl object edgeT value)
      (do (e/client (.setAttribute dom/node "data-empty" (e/server (= ::not-found value))))
          (let [pretty-v (pretty-value value *server-pretty)
                denv {'% (hfql/identify object), (hfql/symbolic-edge edgeT) value, '%v (#(or (hfql/identify value) value))}
                symbolic-link-href (unquote* (::hfql/link ((glitch-guard hfql/metas) edgeT)))]
            (e/Reconcile
              (if (some? symbolic-link-href)
                (router/link ['. [(replace denv symbolic-link-href)]] (dom/text pretty-v))
                (dom/text pretty-v))) )))))

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
  (Render ((glitch-guard hfql/value-at-point) stepT)
    stepT
    (#(if stepT
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
      (str " ("
        (when reached-max-in-memory-lenght?
          ">=") ; FIXME we'd like to display ">" instead
        coll-count
        " items"
        (when reached-max-in-memory-lenght?
          " · truncated")
        ")"))))

(defn format-query-meta [query-meta] (some-> query-meta not-empty dustingetz.str/pprint-str))




#?(:clj (defn- table-title-tooltip-content [stepT]
          {:post [(or (string? %) (nil? %))]}
          (when stepT
            (-> (hfql/describe-formatted stepT)
              (str "\n\n" (format-query-meta (meta (hfql/nav stepT))))
              (str/trim)
              (strx/blank->nil)))))

(e/defn Step-Title [stepT]
  (dom/span
    (dom/props {:class "title"
                :style {:order -3}
                :data-tooltip (e/server (table-title-tooltip-content stepT))})
    (dom/text (e/server (pr-str ((glitch-guard hfql/symbolic-edge-computed) stepT))))))

(comment
  (hfql/describe-column (get dustingetz.namespace-explorer/sitemap `all-ns))
  )
(ns hyperfiddle.navigator6.rendering
  (:require [clojure.string :as str]
            [clojure.pprint :refer [cl-format]]
            #?(:clj [clojure.tools.logging :as log])
            [contrib.data :refer [realize #?(:clj array?)]]
            [dustingetz.str :as strx]
            [geoffreygaillard.debug-tools :refer [debug-exceptions]]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.hfql2.analyzer :as ana]
            [hyperfiddle.hfql2.ast :as-alias ast]
            [hyperfiddle.ct.functor :refer [fmap]]
            [hyperfiddle.ct.cofree :refer [unwrap]]

            [hyperfiddle.ct.either :refer [either either? left? from-left from-right right left]]
            [hyperfiddle.hfql2 :as hfql]
            [hyperfiddle.hfql2.utils :refer [imap? original-object]]
            [hyperfiddle.navigator6.extensions :as extensions]
            [hyperfiddle.navigator6.selection :as selection]
            [hyperfiddle.navigator6.selection2 :as selection2]
            [hyperfiddle.navigator6.utils :as utils :refer [glitch-guard Swallow-empty-diffs]]
            [hyperfiddle.router5 :as router]
            [missionary.core :as m])
  #?(:cljs (:import [goog.math Long])))

(e/declare ^:dynamic *server-pretty)
(e/declare ^:dynamic *tooltip-fn*)

(defn- fixed-size-number? [obj]
  #?(:clj
     (and (number? obj)
       (or (instance? Long obj)
         (instance? Integer obj)
         (instance? Short obj)
         (instance? Byte obj)
         (instance? Double obj)
         (instance? Float obj)))
     :cljs
     (or (number? obj)
       ;; if your codebase uses goog.math.Long explicitly
       (instance? goog.math.Long obj))))

(defn- bounded-size-value? [obj]
  (or (nil? obj)
    (boolean? obj)
    (char? obj) ; Note in cljs chars are strings on length one.
    (keyword? obj) ; generated keywords or syms could be arbitrarily large (like string) but this is not a real-world use case. We decide to ignore it.
    (symbol? obj)
    (fixed-size-number? obj)))

(defn- user-pretty-printer-handler [obj] (get #?(:clj (if (bound? #'*server-pretty) *server-pretty {}) :cljs *server-pretty) (type obj)))

(defmulti pretty-printer-dispatch type)
(defmethod pretty-printer-dispatch :default [obj]
  (cond
    (some? (user-pretty-printer-handler obj)) (type obj)
    (bounded-size-value? obj) ::bounded-size-value
    (string? obj) ::string
    (number? obj) ::unbounded-number
    :else (type obj)))

(defmulti -pretty-printer pretty-printer-dispatch)
(defmethod -pretty-printer :default [obj]
  (let [user-handler (user-pretty-printer-handler obj)]
    (m/timeout (m/via m/blk
                 (try (right ((or user-handler pr-str) obj))
                      #?(:clj (catch Exception ex (left ex)))
                      #?(:clj (catch Error err (left err))) ; Probably not safe. TODO Verify.
                      #?(:cljs (catch :default err (left err)))))
      1000
      (left (ex-info "Rendering timeout" {:type (type obj)})))))

(defmethod -pretty-printer ::bounded-size-value [obj] (m/sp (right (pr-str obj))))
(defmethod -pretty-printer ::string [obj] (m/sp (right (let [len (count obj)] (pr-str (subs obj 0 (min len 100)))))))
(defmethod -pretty-printer ::unbounded-number [obj] (m/sp (right (str (type obj))))) ; for now just render the type. TODO print a truncated, version if possible.

(defn- map-keep-coll [f coll]
  (debug-exceptions `map-keep-coll
    (let [xform (comp (take 1) (map f))
          coll-of-same-type (into (try (empty coll)
                                       (catch #?(:clj Throwable, :cljs :default) _
                                         (cond (imap? coll) {}
                                               (set? coll) #{}
                                               :else  [])))
                              xform coll)]
      (cond
        (vector? coll) coll-of-same-type
        (reversible? coll) (realize (rseq coll-of-same-type)) ; Linear but max coll size is 1
        (seq? coll) (realize (reverse coll-of-same-type)) ; Linear but max coll size is 1
        :else coll-of-same-type
        ))))

#?(:clj (defn str-insert-before-last[^String s ^String suffix]
          (let [sb (StringBuilder. s)
                len (.length sb)]
            (doseq [[idx ^Character c] (map-indexed vector (seq suffix))]
              (.insert sb (+ (dec len) idx) c))
            (.toString sb))))


(defn- str-inline-coll [coll server-pretty]
  (try
    (right
      #?(:cljs (str (type coll)) ; TODO implement.
         :clj
         (debug-exceptions `str-inline-coll
           (let [coll-count-upper-bound 1000
                 coll-count (bounded-count (inc coll-count-upper-bound) coll)
                 items-qualifier (if (map? coll) "entries" "elements")
                 base (binding [*print-length* 1, *print-level* 2]
                        ;; `symbol` removes double quotes from the strings inside the collection
                        (if (map? coll)
                          (if (empty? coll)
                            "{}"
                            (let [[[k v] _] (seq coll)]
                              (str "{"
                                ((get server-pretty (type k) strx/pprint-str) k)
                                ((get server-pretty (type v) strx/pprint-str) v)
                                "}")))
                          (-> (map-keep-coll #(symbol ((get server-pretty (type %) (comp str/trim strx/pprint-str)) %)) coll) ; not linear
                            str
                            (str/replace (str \newline) (str \space))
                            (str/trim)
                            (str/replace #"^$" "()") ; empty coll can appear as nil and produce and empty string. Replace empty string by empty collection.
                            )))
                 max-preview 60
                 base (if (> (count base) max-preview)
                        (str (subs base 0 (dec max-preview)) (last base)) ; truncate but keep closing bracket
                        base)]
             (cond-> base
               (> coll-count 1) (str-insert-before-last " …")
               (<= 2 coll-count coll-count-upper-bound) (str " " coll-count " " items-qualifier)
               (> coll-count coll-count-upper-bound) (str " " coll-count-upper-bound "+ " items-qualifier)
               )))))
    (catch #?(:clj Throwable, :cljs :default) t ; glitch guard
      (left t))))

(defn- pretty-printer [obj user-handlers]
  (binding [*server-pretty user-handlers]
    (condp = obj
      ::not-found (m/sp (right ""))
      ::empty-cell (m/sp (right ""))
      (if (or (set? obj) (sequential? obj) (array? obj) (map? obj)
               #?(:clj (instance? java.util.Collection obj)))
        (m/sp (str-inline-coll obj user-handlers))
        (-pretty-printer obj)))))

(defn as-coll [v] (if (or (array? v) (coll? v)) v [v])) ; TODO removeme, glitch guard

(defn relativize-link-path [symbolic-link-href]
  (if (vector? symbolic-link-href)
    symbolic-link-href
    ['. [symbolic-link-href]]))

(e/defn Resolve [fq-sym fallback] (get e/*exports* fq-sym fallback))

(e/defn DefaultColumnHeaderTooltip [object edgeT value]
  (e/server (str (hfql/describe-formatted edgeT))))

;; ── Generic callback components for agent DI ────────────────────
;; Agents can't define new e/defn components at runtime (Electric program
;; must be synchronized across all agents + cloud proxy). Instead, agents
;; pass Clojure callback fns in e/*bindings* and reference these generic
;; components in their sitemap metadata.

(e/defn ^::e/export FnTooltip [entity edge value]
  (e/server (when *tooltip-fn* (*tooltip-fn* entity edge value))))

(e/defn ^::e/export HrefCell
  "Render a string value as an external hyperlink (<a href=... target=_blank>).
   Use via ::hfql/Render metadata on URL-valued fields in sitemaps."
  [_object _edgeT value]
  (e/client
    (let [v (e/server value)]
      (dom/a (dom/props {:href v :target "_blank" :rel "noopener"})
        (dom/text v)))))

(e/defn RenderTooltip* [tooltip-ident object edgeT value]
  (let [tooltip-qualified-ident (ana/unquote* tooltip-ident) ; FIXME syntax wart
        Tooltip (Resolve tooltip-qualified-ident (get {::hfql/ColumnHeaderTooltip DefaultColumnHeaderTooltip} tooltip-ident (e/fn [_object _edge _value])))
        show? (dom/Mouse-over?)] ; TODO perf – factor out. Today it mounts two event handlers per cell (mouseenter + mouseleave).
    (dom/props
      {:data-tooltip
       (e/Reconcile (when show? (Tooltip object edgeT value)))})) )

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
  (when-let [symbolic-link-href (ana/unquote* (::hfql/link ((glitch-guard hfql/metas) edgeT)))]
    (let [denv {'% (hfql/identify object), (hfql/symbolic-edge edgeT) (hfql/identify value), '%v (or (hfql/identify value) value)}]
      (template-link denv symbolic-link-href))))

(e/declare ^:dynamic *error*)

(e/defn DefaultCellRenderer [object edgeT value]
  (let [value (#(or (some-> *error* ex-message not-empty)
                  (some-> *error* type)
                  value))]
    (RenderTooltip ::hfql/Tooltip object edgeT value)
    (let [either-rendered-str (e/Task (pretty-printer value *server-pretty))
          pretty-v (either #(str (pr-str (type %)) ". " (ex-message %)) identity either-rendered-str)]
      (dom/props {:data-error (#(or (some? *error*) nil))}) ; nil removes the attribute. We want that over `false`.
      (e/Reconcile
        #_(e/client (e/drain (e/Tap-diffs (.setAttribute dom/node "data-empty" (e/server (= ::not-found value)))))) ; FIXME not showing up
        (let [link-href (link-href object edgeT value)]
          (if (Swallow-empty-diffs (some? link-href))
            (router/link (relativize-link-path link-href) (dom/text pretty-v))
            (dom/text pretty-v)))))))

(e/declare Render)

(e/defn RenderCell [either-object edgeT either-value]
  (binding [Render DefaultCellRenderer
            *error* (from-left nil either-value)]
    (e/Reconcile (e/call (Resolve (ana/unquote* (::hfql/Render ((glitch-guard hfql/metas) edgeT))) ; FIXME syntax wart
                           Render)
                   ((glitch-guard some? either identity identity) either-object)
                   edgeT
                   #_(vary-meta edge dissoc ::hfql/Render)
                   ((glitch-guard some? either identity identity) either-value)))))

(defn- get-leafT [stepT] ; TODO questionable logic
  ;; Idea here is to display "leaf values" in the following case:
  ;; [{fs/jfile-kind name}] => "dir" instead of `::fs/dir`.
  (let [metas (hfql/metas stepT)]
    (or
      (and (= ::ast/many (::ast/multiplicity metas))
        stepT)
      (and (hfql/recursion-end?! stepT) ; runs IO, but IO will run anyway as part of this rendering phase.
        stepT)
      (first (unwrap stepT))
      stepT)))

(e/defn Render [object edge value] (RenderCell object edge value))
(e/defn Render* [stepT]
  (Render (e/Offload #((glitch-guard hfql/value-at-point) stepT))
    stepT
    (e/Offload #(if stepT
                  (fmap original-object (hfql/nav* (get-leafT stepT)))
                  (right ::empty-cell)))))



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

(defn format-query-meta [query-meta]
  (some-> query-meta
    not-empty
    (dissoc :hyperfiddle.hfql2.protocols/obj)
    (dustingetz.str/pprint-str :print-length 10 :print-level 15)))



#?(:clj (defn- table-title-tooltip-content [stepT]
          {:post [(or (string? %) (nil? %))]}
          (when stepT
            (-> (hfql/describe-formatted stepT)
              (str "\n\n" (format-query-meta (meta (hfql/nav stepT))))
              (str/trim)
              (strx/blank->nil)))))

(defn- render-step-title [stepT *parent-selection-symbolic] (selection2/point-symbolic-name stepT *parent-selection-symbolic))

(e/defn Step-Title [stepT]
  (dom/span
    (dom/props {:class "title"
                :style {:order -3}
                :data-tooltip (e/server (table-title-tooltip-content stepT))})
    (dom/text (e/server (render-step-title stepT selection/*parent-selection-symbolic)))))
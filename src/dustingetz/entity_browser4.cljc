(ns dustingetz.entity-browser4
  (:require [contrib.css :as cssx]
            [contrib.data :as datax]
            [contrib.debug :as dbg]
            [dustingetz.str :as strx]
            [peternagy.hfql #?(:clj :as :cljs :as-alias) hfql]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-forms4 :as forms]
            [hyperfiddle.ui.tooltip :as tooltip]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.router4 :as router]
            [clojure.string :as str]
            [clojure.pprint]
            [clojure.walk :as walk])
  #?(:cljs (:require-macros dustingetz.entity-browser4)))

(defmacro rebooting [sym & body] `(e/for [~sym (e/diff-by identity (e/as-vec ~sym))] ~@body))

(e/declare *hfql-bindings *mode *update *sitemap-writer *sitemap !sitemap)
(declare css)

#?(:clj
   (defn ->sort-comparator [sort-spec]
     (let [[[k _asc-desc]] sort-spec]   ; TODO support multi-sort
       (fn [a b] (neg? (compare (get a k) (get b k)))))))

(comment
  (sort (->sort-comparator [[:k :asc]]) [{:k 1} {:k 0} {:k 2}]))

(e/defn IDE-mode? [] (= *mode :ide))

(defn infer-block-type [x]
  (cond
    (set? x)                                             :set
    (sequential? x)                                      :table
    (or (number? x) (string? x) (boolean? x) (ident? x)) :scalar
    (fn? x)                                              :query
    :else                                                :object))

(defn pretty-name [x]
  (cond
    (seq? x) (let [[qs & args] x] (list* (datax/unqualify qs) args))
    () (str x)))

(defn pretty-value [x] (pr-str x))

(defn pretty-title [query] (cons (datax/unqualify (first query)) (next query)))

(e/declare whitelist)
(e/defn Resolve [fq-sym fallback] (get whitelist fq-sym fallback))

#?(:clj
   (defn remove-opt [spec k]
     (if-some [opts (hfql/opts spec)]
       (if (contains? opts k)
         (hfql/props (hfql/unwrap spec) (dissoc opts k))
         spec)
       spec)))

(defn column-appender [s] (fn [v] (str (subs v 0 (- (count v) 2)) "\n " s "]\n")))

(e/defn Suggestions [o]
  (e/client
    (e/When (IDE-mode?)
      (dom/div
        (dom/text "suggestions:")
        (let [suggestions (e/server (or (with-bindings *hfql-bindings (hfql/suggest o))
                                      (when (or (coll? o) (sequential? o))
                                        (with-bindings *hfql-bindings (hfql/suggest (nth o 0 nil))))))]
          (e/When suggestions
            (dom/div
              (e/for [s (e/diff-by {} suggestions)]
                (dom/button
                  (dom/text s)
                  (let [[t] (e/Token (dom/On "click" (fn [_] true) nil))]
                    (e/When t [t s])))))))))))

(e/declare Render)

#?(:clj
   (defn str-inline-coll [coll]
     (let [coll-count (count coll)
           base (binding [*print-length* 1
                          *print-level* 2]
                  (-> (strx/pprint-str coll)
                    (str/replace (str \newline) (str \space))
                    (str/trim)))]
       (if (pos? (dec coll-count))
         (str base (clojure.pprint/cl-format false " ~d element~:p" coll-count))
         base))))

(e/defn RenderInlineColl [v o spec]
  (let [pretty-v (str-inline-coll v)
        opts (hfql/opts spec)]
    (if-some [query (::hfql/link opts)]
      (router/link ['. [query]] (dom/text pretty-v))
      (dom/text pretty-v))))

(e/defn RenderCell [v o spec]
  (let [opts (hfql/opts spec)]
    (dom/td                          ; custom renderer runs in context of a cell
      (let [K (e/fn [v o spec]
                (when-some [Tooltip (Resolve (::hfql/Tooltip opts) nil)]
                  (let [show? (dom/Mouse-over?)]
                    (dom/props {:data-tooltip (when show? (Tooltip v o spec))})))
                (if (coll? v)
                  (RenderInlineColl v o spec)
                  (let [pretty-v (pretty-value v)
                        denv {'% o, (hfql/unwrap spec) v}]
                    (if-some [query (::hfql/link opts)]
                      (router/link ['. [(replace denv query)]] (dom/text pretty-v))
                      (dom/text pretty-v)))))]
        (binding [Render K]
          (e/call (Resolve (::hfql/Render opts) K) v o (remove-opt spec ::hfql/Render)))))))

(e/defn Render [v o spec] (RenderCell v o spec))

(e/defn Search []
  (dom/input
    (dom/props {:type "search"})
    (dom/On "input" #(-> % .-target .-value) "")))

(e/defn ObjectRow [[k v] o spec]
  (dom/td (dom/text (e/server (pretty-name k))))
  (Render v o spec))

(e/defn ObjectBlock [query o spec _args]
  (e/client
    (dom/fieldset
      (dom/props {:class "entity dustingetz-entity-browser3__block"})
      (let [search (dom/legend
                     (dom/span (dom/props {:class "title"}) (dom/text (e/server (pretty-title query)) " "))
                     (Search))          ; TODO search can crash with useless exception (no on-message handler)
            data (e/server (filterv (fn [kv] (strx/any-matches? kv search)) (hfql/pull *hfql-bindings spec o)))
            row-count (e/server (count data)), row-height 24]
        (dom/props {:style {:--column-count 2 :--row-height row-height}})
        (forms/TablePicker! ::selection nil row-count
          (e/fn [index] (e/server (some-> (nth data index nil) (ObjectRow o (nth spec index {})))))
          :row-height row-height
          :column-count 2)))))

(defn ->short-keyword-map [cols-available!]
  (let [k* (filterv keyword? cols-available!)
        freq (frequencies (mapv datax/unqualify k*))]
    (into {} (map #(let [unq (datax/unqualify %)] [% (if (= 1 (freq unq)) unq %)]))
      k*)))

(defn column-shortener [symbolic-columns]
  (let [short-keyword-map (->short-keyword-map symbolic-columns)]
    (fn [symbolic-column]
      (cond
        (keyword? symbolic-column) (short-keyword-map symbolic-column)
        (seq? symbolic-column) (let [[qs & args] symbolic-column] (list* (datax/unqualify qs) args))
        () (str symbolic-column)))))

(e/defn TableRow [cols col->spec o m]
  (e/for [col cols]
    (Render (get m col) o (e/server (col->spec col)))))

(e/defn QueryBlock [query o-fn spec args]
  (e/client
    (dom/fieldset
      (dom/props {:class "entity-children dustingetz-entity-browser3__block"})
      (let [!sort-spec (atom [[(e/server (-> spec first hfql/unwrap)) :asc]]), sort-spec (e/watch !sort-spec)
            !search (atom ""), search (e/watch !search)
            data (e/server (o-fn search sort-spec))
            row-count (e/server (count data)), row-height 24
            cols (e/server (e/diff-by {} (mapv hfql/unwrap spec)))
            col->spec (e/server (into {} (map (fn [x] [(hfql/unwrap x) x])) spec))
            column-count (e/server (e/Count cols))]
        (dom/legend
          (dom/span
            (dom/props {:class "title"})
            (dom/text (e/server (pretty-title query)))
            (dom/text " ")
            (reset! !search (Search))
            (dom/text " (" row-count " items)")))
        (dom/table
          (dom/props {:style {:--row-height (str row-height "px"), :--column-count column-count}})
          (dom/thead
            (dom/tr
              (let [shorten (column-shortener (e/as-vec cols))]
                (e/for [col cols]
                  (dom/th
                    (dom/props {:title (str col)})
                    (when (keyword? col)
                      (dom/On "click" #(reset! !sort-spec [[col :asc]]) nil))
                    (dom/text (shorten col)))))))
          (forms/TablePicker! ::selection nil row-count
            (e/fn [index] (e/server (some->> (nth data index nil)
                                      (hfql/pull *hfql-bindings spec)
                                      (TableRow cols col->spec data))))
            :row-height row-height
            :column-count column-count
            :as :tbody))))))

#?(:clj (defn ->query-fn [coll]
          (fn [search sort-spec]
            (vec
              (sort (->sort-comparator sort-spec)
                (eduction (filter #(strx/includes-str? % search)) coll))))))

(e/defn TableBlock [query o spec args]
  (QueryBlock query (e/server (->query-fn o)) spec args))

#?(:clj (defn sitemapify [spec]
          (walk/postwalk
            (fn [x] (if (hfql/opts x)
                      (list 'hfql/props (hfql/unwrap x) (hfql/opts x))
                      x))
            spec)))

(defn append-to-query [sitemap query added-spec]
  (reduce-kv (fn [ac k v] (assoc ac k (cond-> v (= (first k) (first query)) (conj added-spec))))
    {} sitemap))

(e/defn Block [query o spec]
  (when-some [F (e/server (case (infer-block-type o)
                            :object ObjectBlock
                            :table TableBlock
                            :query QueryBlock
                            #_else nil))]
    (when (IDE-mode?)
      (let [update-text (dom/textarea
                          (dom/props {:rows 10, :cols 80})
                          (dom/text (e/server (strx/pprint-str (sitemapify spec))))
                          (fn [f] (set! (.-value dom/node) (f (.-value dom/node)))))]
        update-text
        (e/for [[t v] (Suggestions o)]
          (update-text (column-appender v))
          (case (e/server
                  (*sitemap-writer
                    (sitemapify
                      (swap! !sitemap append-to-query query v)))
                  #_(*sitemap-writer (sitemapify (update *sitemap (list* query) conj v))))
            (t)))))
    (forms/Interpreter {::selection (e/fn [_] #_(prn 'selected v))}
      (F query o spec (next router/route)))))

(defn find-sitemap-spec [sitemap f$]
  (reduce-kv (fn [_ [k$] v]
               (when (= f$ k$) (reduced v))) nil sitemap))

(e/defn HfqlRoot [sitemap default]
  (e/client
    (dom/style (dom/text css tooltip/css))
    (tooltip/TooltipArea
      (e/fn []
        (tooltip/Tooltip)
        (dom/div
          (dom/props {:class "Browser"})
          (binding [*mode (if (dom/div
                                (dom/label
                                  (dom/text "IDE")
                                  (dom/input
                                    (dom/props {:type "checkbox"})
                                    (dom/On "change" #(-> % .-target .-checked) false))))
                            :ide :crud)]
            (let [[query] router/route]
              (rebooting query
                (if-not query
                  (router/ReplaceState! ['. default])
                  (let [[f$ & args] query
                        f (e/server (hfql/resolve! f$))
                        o (e/server (with-bindings *hfql-bindings (apply f args)))]
                    (set! (.-title js/document) (str (some-> f$ name (str " – ")) "Hyperfiddle"))
                    (dom/props {:class (cssx/css-slugify f$)})
                    (Block query o (e/server (find-sitemap-spec sitemap f$)))))))))))))

(def table-block-css
"
.dustingetz-entity-browser3__block table { display: grid; grid-template-columns: repeat(var(--column-count), 1fr);  grid-template-rows: var(--row-height);}

.dustingetz-entity-browser3__block table thead { display: contents; }
.dustingetz-entity-browser3__block table thead tr { display: grid; grid-row: 1; grid-column: 1 / -1; grid-template-columns: subgrid;}
.dustingetz-entity-browser3__block table thead tr th { white-space: nowrap; text-overflow: ellipsis; overflow: hidden; }

.dustingetz-entity-browser3__block .hyperfiddle-electric-forms4__table-picker { grid-row: 2; grid-column: 1 / -1; grid-template-columns: subgrid; }

"
)

(def css
  (str forms/css
    table-block-css

    "
/* cosmetic defaults */
.dustingetz-entity-browser3__block legend .title {font-weight:600;}
.dustingetz-entity-browser3__block table { background-color: white; border: 1px lightgray solid; border-top-left-radius: 0.25rem; border-top-right-radius: 0.25rem; }
.dustingetz-entity-browser3__block table thead tr { border-bottom: 1px lightgray solid; }
.dustingetz-entity-browser3__block table thead tr th { white-space: nowrap; text-overflow: ellipsis; overflow: hidden; }
.dustingetz-entity-browser3__block table thead tr th { font-weight: 500; }
.dustingetz-entity-browser3__block table thead tr th:not(:first-child) { border-left: 1px lightgray solid; }
.dustingetz-entity-browser3__block table :is(th, td) { padding: 0 0.25em; }
/* --------- */

"

    ))

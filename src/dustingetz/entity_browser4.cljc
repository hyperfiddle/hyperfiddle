(ns dustingetz.entity-browser4
  (:require [contrib.css :as cssx]
            [contrib.assert :as ca]
            [contrib.data :as datax]
            [contrib.debug :as dbg]
            [dustingetz.str :as strx]
            [peternagy.hfql #?(:clj :as :cljs :as-alias) hfql]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-forms4 :as forms]
            [hyperfiddle.ui.tooltip :as tooltip]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.router4 :as router]
            [hyperfiddle.nav0 :as hfp]
            [clojure.datafy :as datafy]
            [clojure.string :as str]
            [clojure.pprint]
            [clojure.walk :as walk])
  #?(:cljs (:require-macros dustingetz.entity-browser4)))

(defmacro rebooting [sym & body] `(e/for [~sym (e/diff-by identity (e/as-vec ~sym))] ~@body))

(e/declare *hfql-bindings *mode !mode *update *sitemap-writer *sitemap !sitemap *page-defaults *block-opts *depth)
(declare css)

#?(:clj
   (defn ->sort-comparator [sort-spec]
     (let [[[k asc?]] sort-spec
           order (if asc? neg? pos?)]   ; TODO support multi-sort
       (when k
         (fn [a b] (order (compare (get a k) (get b k))))))))

(comment
  (sort (->sort-comparator [[:k true]]) [{:k 1} {:k 0} {:k 2}])
  (sort (->sort-comparator [[:k false]]) [{:k 1} {:k 0} {:k 2}])
  )

(e/defn IDE-mode? [] (= *mode :ide))
(e/defn Browse-mode? [] (= *mode :browse))

(defn infer-block-type [x]
  (cond
    (set? x)                                             :set
    (sequential? x)                                      :collection
    (or (number? x) (string? x) (boolean? x) (ident? x)) :scalar
    (fn? x)                                              :query
    :else                                                :object))

(defn unqualify [sym] (symbol (name sym)))
(defn de-clojure-core [x] (->> x (walk/postwalk #(cond-> % (and (symbol? %) (= (namespace %) "clojure.core")) unqualify))))
(defn pretty-name [x]
  (let [x (de-clojure-core x)]
    (cond
      (seq? x) (let [[qs & args] x] (list* (datax/unqualify qs) args))
      () (str x))))

(defn pretty-value [x] (pr-str x))

(defn pretty-title [query]
  (let [?f$ (first query)]
    (cons (cond-> ?f$ (qualified-symbol? ?f$) datax/unqualify)
      (mapv #(or (hfp/identify %) %) (next query)))))

(e/declare whitelist)
(e/defn Resolve [fq-sym fallback] (get whitelist fq-sym fallback))

#?(:clj (defn remove-opt [spec k] (hfql/props-update-opts spec #(dissoc % k))))

(defn column-appender [s] (fn [v] (str (subs v 0 (- (count v) 2)) "\n " s "]\n")))

#?(:clj (defn collect-suggestions [o bindings]
          (with-bindings bindings
            (let [innate* (or (hfql/suggest o)
                            (when (or (coll? o) (sequential? o))
                              (hfql/suggest (nth o 0 nil))))
                  jvm* (hfql/suggest-jvm o)]
              (into innate* jvm*)))))

(e/defn Suggestions [o]
  (e/client
    (e/When (IDE-mode?)
      (dom/div
        (dom/text "suggestions:")
        (let [suggestions (e/server (collect-suggestions o *hfql-bindings)
                                    #_(or (with-bindings *hfql-bindings (hfql/suggest o))
                                      (when (or (coll? o) (sequential? o))
                                        (with-bindings *hfql-bindings (hfql/suggest (nth o 0 nil))))))]
          (e/When suggestions
            (dom/div
              (e/for [{:keys [label entry]} (e/diff-by {} suggestions)]
                (dom/button
                  (dom/text label)
                  (let [[t] (e/Token (dom/On "click" (fn [_] true) nil))]
                    (e/When t [t entry])))))))))))

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
                (when-some [Tooltip (Resolve (::hfql/Tooltip opts) (Resolve (::hfql/Tooltip *block-opts) nil))]
                  (let [show? (dom/Mouse-over?)]
                    (dom/props {:data-tooltip (when show? (Tooltip v o spec))})))
                (if (coll? v)
                  (RenderInlineColl v o spec)
                  (let [pretty-v (pretty-value v)
                        denv {'% o, (hfql/unwrap spec) v, '%v v}]
                    (if-some [query (::hfql/link opts)]
                      (router/link ['. [(replace denv query)]] (dom/text pretty-v))
                      (dom/text pretty-v)))))]
        (binding [Render K]
          (e/call (Resolve (::hfql/Render opts) K) v o (remove-opt spec ::hfql/Render)))))))

(e/defn Render [v o spec] (RenderCell v o spec))

(e/defn Search! [saved-search]
  (let [edit (forms/Input! ::search saved-search)]
    (e/When (not= forms/nil-t (::forms/token edit))
      edit)))

(defn find-index [pred x*]
  (transduce (keep-indexed (fn [idx x] (when (pred x) idx))) (fn ([v] v) ([_ac nx] (reduced nx))) nil x*))

(defn find-if [pred x*]
  (transduce (keep (fn [x] (when (pred x) x))) (fn ([v] v) ([_ac nx] (reduced nx))) nil x*))

(defn find-sitemap-spec [sitemap f$]
  (ca/is (reduce-kv (fn [_ [k$] v]
                      (when (= f$ k$) (reduced v))) nil sitemap)
    some? (str "couldn't find sitemap definition for " (pr-str f$))))

#?(:clj (defn find-spec-prop [raw-spec raw-k]
          (transduce (keep #(when (= raw-k (hfql/unwrap %)) %)) (fn ([v] v) ([_ac nx] (reduced nx))) nil raw-spec)))

#?(:clj (defn query->object [hfql-bindings query]
          (let [[f$ & args] query
                f (hfql/resolve! f$)]
            (with-bindings hfql-bindings (apply f args)))))

#?(:clj (defn add-suggestions [spec suggest*]
          (hfql/props-update-k spec
            (fn [raw-spec]
              (reduce (fn [raw-spec {nx :entry}]
                        (if (find-if #(= nx (hfql/unwrap %)) raw-spec)
                          raw-spec
                          (conj raw-spec nx)))
                raw-spec suggest*)))))

(e/defn ObjectRow [[k v] o spec shorten]
  (dom/td (dom/text (e/server (pretty-name (shorten k)))))
  (Render v o spec))

(e/declare Block)

(e/defn NextBlock [query-template next-x o]
  (e/server
    (rebooting o
      (e/client
        (let [query (e/server (replace {'% (hfp/identify o), '%v (hfp/identify next-x)} query-template))
              next-o (e/server (query->object *hfql-bindings query))]
          (binding [*depth (inc *depth)]
            (Block query next-o (e/server (find-sitemap-spec *sitemap (first query-template))))))))))

#?(:clj (defn not-entity-like? [x] (or (nil? x) (boolean? x) (string? x) (number? x) (ident? x) (vector? x) (.isArray (class x)))))

(e/defn AnonymousBlock [selection next-x]
  (e/server
    (rebooting next-x
      ;; similarity with `infer-block-type`
      ;; maybe blocks should decide if they handle this object?
      (when (or (sequential? next-x) (set? next-x) (seq (hfql/suggest next-x)))
        (e/client
          (binding [*depth (inc *depth)]
            (Block [selection] next-x (e/server []))))))))

#?(:clj (defn find-default-page [page-defaults o] (some #(% o) page-defaults)))

(defn ->short-map [cols-available! filterer]
  (let [k* (filterv filterer cols-available!)
        freq (frequencies (mapv datax/unqualify k*))]
    (into {} (map #(let [unq (datax/unqualify %)] [% (if (= 1 (freq unq)) unq %)]))
      k*)))

(defn column-shortener [symbolic-columns filterer]
  (let [short-map (->short-map symbolic-columns filterer)]
    (fn [symbolic-column]
      (cond
        (filterer symbolic-column) (short-map symbolic-column)
        (seq? symbolic-column) (let [[qs & args] symbolic-column] (list* (datax/unqualify qs) args))
        () symbolic-column))))

(e/defn Nav [coll k v] (e/server (with-bindings *hfql-bindings (datafy/nav coll k v))))

#?(:clj (defn find-key-spec [spec k] (find-if #(= k (hfql/unwrap %)) spec)))
#?(:clj (defn ?unlazy [o] (cond-> o (seq? o) list*)))

(e/defn ObjectBlock [query o spec effect-handlers args]
  (e/client
    (let [{saved-search ::search, saved-selection ::selection} args
          opts (e/server (hfql/opts spec))
          browse? (Browse-mode?)
          ;; TODO remove Reconcile eventually? Guards mount-point bug in forms4/Picker!
          spec2 (e/server (e/Reconcile (cond-> spec browse? (add-suggestions (hfql/suggest o)))))
          raw-spec (e/server (hfql/unwrap spec2))
          shorten (e/server (column-shortener (mapv hfql/unwrap raw-spec) symbol?))
          default-select (e/server (::hfql/select opts))
          !search (atom nil), search (e/watch !search)
          pulled (e/server (hfql/pull *hfql-bindings raw-spec o))
          data (e/server
                 (into [] (keep (fn [kspec]
                                  (let [k (hfql/unwrap kspec)
                                        v (get pulled k)]
                                    (when (or (strx/includes-str? (?unlazy k) saved-search)
                                            (strx/includes-str? v saved-search))
                                      (datax/map-entry k v)))))
                   raw-spec))
          row-count (e/server (count data)), row-height 24]
      (binding [*block-opts (e/server (hfql/opts spec))]
        (dom/fieldset
          (dom/props {:class "entity dustingetz-entity-browser3__block"})
          (dom/legend
            (dom/span (dom/props {:class "title"}) (dom/text (e/server (pretty-title query)) " "))
            ;; TODO remove ugly workaround, solves bug where search travels across navigation
            (reset! !search (update (Search! saved-search) ::forms/token forms/unify-t
                              (fn ([] (reset! !search nil)) ([err] (prn 'err err))))))
          (dom/props {:style {:--column-count 2 :--row-height row-height}})
          (forms/Interpreter effect-handlers
            (e/amb
              (e/When search search)
              (forms/Intercept
                (e/fn [index] (forms/TablePicker! ::selection index row-count
                                (e/fn [index] (e/server
                                                (when-some [kv (nth data index nil)]
                                                  (ObjectRow kv o (find-key-spec raw-spec (key kv)) shorten))))
                                :row-height row-height
                                :column-count 2))
                saved-selection
                (e/fn ToIndex [selection] (e/server (find-index (fn [[k]] (= k selection)) data)))
                (e/fn ToSaved [index] (let [x (e/server (nth data index nil))
                                            row-select (e/server (-> (nth raw-spec index nil) hfql/opts ::hfql/select))
                                            select (or row-select default-select)]
                                        (e/When (e/server (or browse? (and x select))) (e/server (first x))))))))))
      (when saved-selection
        (let [next-x (e/server (when-some [nx (find-if (fn [[k _v]] (= k saved-selection)) pulled)] ; when-some because glitch
                                 (Nav o (key nx) (val nx))))
              row-select (e/server (-> (find-spec-prop raw-spec saved-selection) hfql/opts ::hfql/select))
              select (or row-select default-select)]
          (rebooting select
            (if select
              (NextBlock select next-x o)
              (when browse?
                (e/server
                  (let [query-template (find-default-page *page-defaults next-x)]
                    (rebooting query-template
                      (e/client
                        (if query-template
                          (NextBlock query-template next-x o)
                          (AnonymousBlock saved-selection next-x))))))))))))))

(e/defn TableRow [cols col->spec o m]
  (e/for [col cols]
    (Render (get m col) o (e/server (col->spec col)))))

(defn toggle-column-sort [sort-spec for-col]
  (when-some [[col asc?] (first sort-spec)]
    [[for-col (if (= col for-col) (not asc?) true)]]))

(e/defn TableHeader [cols !sort-spec]
  (dom/thead
    (dom/tr
      (let [shorten (column-shortener (e/as-vec cols) ident?)]
        (e/for [col cols]
          (dom/th
            (dom/props {:title (str col)})
            (dom/On "click" #(swap! !sort-spec toggle-column-sort col) nil)
            (dom/text (pretty-name (shorten col)))))))))

(e/defn TableTitle [query !search saved-search row-count spec suggest*]
  (dom/legend
    (dom/span
      (dom/props {:class "title"})
      (dom/text (pretty-title query))
      (dom/text " ")
      ;; TODO remove ugly workaround, solves bug where search travels across navigation
      (e/client (reset! !search (update (Search! saved-search)
                                  ::forms/token forms/unify-t
                                  (fn ([] (reset! !search nil)) ([err] (prn 'err err)))))
                nil)
      (dom/text " (" row-count " items) ")
      (let [k* (into #{} (map hfql/unwrap) (hfql/unwrap spec))
            pre-checked (empty? k*)
            new-suggest* (into [] (comp (map :entry) (remove k*)) suggest*)
            shorten (column-shortener (into k* new-suggest*) ident?)
            selected (e/as-vec
                       (e/for [entry (e/diff-by {} new-suggest*)]
                         (let [id (e/client (random-uuid))]
                           (e/amb
                             (dom/input
                               (dom/props {:type "checkbox", :id id})
                               (e/client (set! (.-checked dom/node) pre-checked))
                               (e/When (dom/On "change" #(-> % .-target .-checked) pre-checked)
                                 entry))
                             (dom/label
                               (dom/props {:for id})
                               (dom/text (shorten entry) " "))))))]
        (e/for [k (e/diff-by {} (mapv hfql/unwrap (hfql/unwrap spec)))]
          (let [id (e/client (random-uuid))]
            (dom/input
              (dom/props {:type "checkbox", :checked true, :disabled true, :id id}))
            (dom/label
              (dom/props {:for id})
              (dom/text (shorten k) " "))))
        (hfql/props-update-k spec (fn [raw-spec] (into raw-spec selected)))))))

(e/defn TableBody [row-count row-height cols data raw-spec saved-selection select]
  (let [col->spec (e/server (into {} (map (fn [x] [(hfql/unwrap x) x])) raw-spec))]
    (forms/Intercept (e/fn [index]
                       (forms/TablePicker! ::selection index row-count
                         (e/fn [index] (e/server (some->> (nth data index nil)
                                                   (Nav data nil)
                                                   (hfql/pull *hfql-bindings raw-spec)
                                                   (TableRow cols col->spec data))))
                         :row-height row-height
                         :column-count (e/server (count raw-spec))
                         :as :tbody))
      saved-selection
      (e/fn ToIndex [selection] (e/server
                                  (find-index #{selection}
                                    (eduction (map #(or (hfp/identify %) %)) data))))
      (e/fn ToSaved [index] (e/When (or select (Browse-mode?))
                              (let [x (e/server (nth data index nil))
                                    symbolic-x (e/server (hfp/identify x))]
                                (e/When symbolic-x symbolic-x)))))))

#?(:clj (defn eager-pull-search-sort [data spec hfql-bindings search sort-spec]
          (let [enriched (hfql/pull hfql-bindings spec data)
                filtered (eduction (filter #(strx/any-matches? (vals %) search)) enriched)]
            (vec (if-some [sorter (->sort-comparator sort-spec)]
                   (try (sort sorter filtered) (catch Throwable _ filtered))
                   filtered)))))

(e/defn CollectionTableBody [row-count row-height cols data raw-spec saved-selection select]
  (let [col->spec (e/server (into {} (map (fn [x] [(hfql/unwrap x) x])) raw-spec))]
    (forms/Intercept (e/fn [index]
                       (forms/TablePicker! ::selection index row-count
                         (e/fn [index] (e/server (some->> (nth data index nil)
                                                   (TableRow cols col->spec data))))
                         :row-height row-height
                         :column-count (e/server (count raw-spec))
                         :as :tbody))
      saved-selection
      (e/fn ToIndex [selection] (e/server
                                  (find-index #{selection}
                                    (eduction (map #(let [o (-> % meta ::hfql/origin)]
                                                      (or (hfp/identify o) o))) data))))
      (e/fn ToSaved [index] (e/When (or select (Browse-mode?))
                              (let [x (e/server (-> (nth data index nil) meta ::hfql/origin))
                                    symbolic-x (e/server (hfp/identify x))]
                                (e/When symbolic-x symbolic-x)))))))

;; CollectionBlock is naive, doing N+1 queries and doing work in memory
(e/defn CollectionBlock [query unpulled spec effect-handlers args]
  (e/client
    (let [{saved-search ::search, saved-selection ::selection} args
          select (e/server (::hfql/select (hfql/opts spec)))
          !sort-spec (atom [[(e/server (some-> (hfql/unwrap spec) first hfql/unwrap)) true]]), sort-spec (e/watch !sort-spec)
          !search (atom nil), search (e/watch !search)
          !row-count (atom 0), row-count (e/watch !row-count)]
      ;; cycle back first column as sort in browse mode
      (dom/fieldset
        (dom/props {:class "entity-children dustingetz-entity-browser3__block"})
        (let [spec2 (e/server
                      (TableTitle query !search saved-search row-count spec
                        (when (Browse-mode?) (hfql/suggest (Nav unpulled nil (first unpulled))))))
              raw-spec2 (e/server (hfql/unwrap spec2))
              data (e/server (eager-pull-search-sort
                               ((fn [] (with-bindings *hfql-bindings (mapv #(datafy/nav unpulled nil %) unpulled))))
                               raw-spec2 *hfql-bindings saved-search sort-spec))
              row-height 24
              cols (e/server (e/diff-by {} (mapv hfql/unwrap raw-spec2)))
              column-count (e/server (count raw-spec2))]
          (reset! !row-count (e/server (count data)))
          (when (and (Browse-mode?) (e/server (nil? (some-> (hfql/unwrap spec) first))))
            (reset! !sort-spec [[(e/server (some-> (hfql/unwrap spec2) first hfql/unwrap)) true]]))
          (dom/table
            (dom/props {:style {:--row-height (str row-height "px"), :--column-count column-count}})
            (TableHeader cols !sort-spec)
            (forms/Interpreter effect-handlers
              (e/amb
                (e/When search search)
                (CollectionTableBody row-count row-height cols data raw-spec2 saved-selection select))))))
      (when saved-selection
        (let [next-x (e/server
                       (with-bindings *hfql-bindings
                         (some #(let [navd (datafy/nav unpulled nil %)]
                                  (when (= saved-selection (or (hfp/identify %) %))
                                    navd))
                           unpulled)))]
          (rebooting select
            (if select
              ;; In ObjectBlock we pass the selected object and the root object.
              ;; Here the root object is a query fn, or the filtered collection.
              ;; Do we want/need to pass it? Should we bind it to `%`?
              (NextBlock select next-x next-x)
              (when (Browse-mode?)
                (e/server
                  (let [query-template (find-default-page *page-defaults next-x)]
                    (rebooting query-template
                      (e/client
                        (if query-template
                          (NextBlock query-template next-x next-x)
                          (AnonymousBlock saved-selection next-x))))))))))))))

#?(:clj (defn sitemapify [spec]
          (walk/postwalk
            (fn [x] (if (hfql/opts x)
                      (list 'hfql/props (hfql/unwrap x) (hfql/opts x))
                      x))
            spec)))

(defn append-to-query [sitemap query added-spec]
  (reduce-kv (fn [ac k v] (assoc ac k (cond-> v (= (first k) (first query)) (conj added-spec))))
    {} sitemap))

(def default-mode :browse)

(e/defn Block [query o spec]
  (when-some [F (e/server (case (infer-block-type o)
                            :object ObjectBlock
                            :collection CollectionBlock
                            :set CollectionBlock
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
    (let [effect-handlers
          {::search (e/fn [s]
                      ;; `update` doesn't work, route is a lazyseq (?)
                      (router/ReplaceState! ['. (into (conj (into [] (take *depth) router/route)
                                                        (assoc (nth router/route *depth {}) ::search s))
                                                  (drop (inc *depth) router/route))])
                      [::forms/ok])
           ::selection (e/fn [symbolic-x]
                         (router/ReplaceState! ['. (conj (into [] (take *depth) router/route)
                                                     (assoc (nth router/route *depth {}) ::selection symbolic-x))])
                         [::forms/ok])}]
      (reset! !mode (or (e/server (-> spec hfql/opts ::hfql/mode)) default-mode))
      (F query o spec effect-handlers (nth router/route *depth {})))))

(e/defn ModePicker [mode]
  (let [edit (forms/RadioPicker! nil (name mode) :Options (e/fn [] (e/amb "crud" "ide" "browse")))]
    (if (e/Some? edit)
      (keyword (::forms/value edit))
      mode)))

(e/defn HfqlRoot [sitemap default]
  (e/client
    (dom/style (dom/text css tooltip/css))
    (tooltip/TooltipArea
      (e/fn []
        (tooltip/Tooltip)
        (dom/div
          (dom/props {:class "Browser"})
          (binding [!mode (atom default-mode)]
            (let [mode (e/watch !mode)]
              (binding [*mode mode #_(ModePicker mode)
                        *depth 1]
                (let [[query] router/route]
                  (rebooting query
                    (if-not query
                      (router/ReplaceState! ['. default])
                      (let [f$ (first query)
                            o (e/server (query->object *hfql-bindings query))]
                        (set! (.-title js/document) (str (some-> f$ name (str " – ")) "Hyperfiddle"))
                        (dom/props {:class (cssx/css-slugify f$)})
                        (Block query o (e/server (find-sitemap-spec sitemap f$)))))))))))))))

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
.dustingetz-entity-browser3__block { padding: 0 0.5em; background-color: white; }
.dustingetz-entity-browser3__block table { border: 1px solid #f2f2f2; border-top-left-radius: 0.25rem; border-top-right-radius: 0.25rem; }
.dustingetz-entity-browser3__block table thead tr { background-color: #f2f2f2; border-bottom: 1px lightgray solid; }
.dustingetz-entity-browser3__block table thead tr th { white-space: nowrap; text-overflow: ellipsis; overflow: hidden; }
.dustingetz-entity-browser3__block table thead tr th { font-weight: 500; }
.dustingetz-entity-browser3__block table thead tr th:not(:first-child) { border-left: 1px lightgray solid; }
.dustingetz-entity-browser3__block table :is(th, td) { padding: 0 0.25em; }
/* carefully override table-picker - invert default row striping so gray thead is followed by white row first */
.dustingetz-entity-browser3__block tbody tr:nth-child(odd):not(:is([aria-selected=true],[aria-checked=true])):not(:hover) td { background-color: #f2f2f2; }
.dustingetz-entity-browser3__block tbody tr:nth-child(even):not(:is([aria-selected=true],[aria-checked=true])):not(:hover) td { background-color: white; }
/* --------- */

"

    ))

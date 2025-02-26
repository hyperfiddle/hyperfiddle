(ns dustingetz.entity-browser3
  (:require [clojure.datafy :refer [datafy]]
            [clojure.core.protocols :refer [nav]]
            clojure.set
            [contrib.data :refer [index-by unqualify index-of map-entry]]
            [dustingetz.str :refer [pprint-str]]
            [contrib.css :refer [css-slugify]]
            [hyperfiddle.nav0 :refer [identify]]
            [electric-fiddle.fiddle-index :refer [pages NotFoundPage]]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric3-contrib :as ex]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.electric-forms4 :as forms :refer [Intercept Interpreter Checkbox TablePicker!]]
            [hyperfiddle.router4 :as router]
            [hyperfiddle.rcf :refer [tests]]
            #?(:clj [markdown.core :as md])
            #?(:clj [peternagy.pull-walker :refer [walker]])
            [contrib.debug :as dbg]
            #?(:clj [dustingetz.hfql11 :refer [hf-pull hf-pull2 hf-pull3 hf-nav2 hfql-search-sort]])
            [hyperfiddle.ui.tooltip :as tooltip :refer [TooltipArea Tooltip]]))

(e/declare ; ^:dynamic
 *hfql-bindings)

(defn pretty-name [x]
  (cond
    (seq? x) (let [[qs & args] x] (list* (unqualify qs) args))
    () (str x)))

(defn pretty-value [x]
  (cond
    (coll? x) (->> (mapv #(or (identify %) %) x) (into (empty x)) pr-str)
    () (pr-str (or (identify x) x))))

(e/declare Render)

(e/defn TreeRow [hfql-cols! [path value branch? :as row]]
  (e/server
    (let [k (peek path)]
      (dom/td (dom/props {:style {:padding-left (some-> path count dec (* 15) (str "px"))}})
        (dom/span (dom/text (e/server (pretty-name k))))) ; todo attr schema tooltip if available
      (when-not branch? (Render {k value} k value hfql-cols!))))) ; includes semantic tooltip if available

(e/defn Search! [authoritative-search]
  (let [edit (forms/Input! ::search authoritative-search)]
    (e/When (not= forms/nil-t (::forms/token edit))
      edit)))

(defn build-selection [select ctx]
  `[:page ~@(replace ctx select)])

(defn symbolic-title [sexpr]
  ;; G: not sure if this produces the correct semantic representation
  ;;   But less visual noise is good for demos – previous impl rendered `(foo %)`, this one renders `foo`.
  ;;   Will revisit
  (if (vector? sexpr) ; sequential?
    (let [[x & args] sexpr]
      (if (ident? x)
        (cons (unqualify x) args) ; todo tooltip
        (seq sexpr)))
    sexpr))

(defn find-index [pred x*]
  (transduce (keep-indexed (fn [idx x] (when (pred x) idx))) (fn ([v] v) ([_ac nx] (reduced nx))) nil x*))

(e/defn TreeBlock
  [field-name kv state hfql-cols!
   & {:keys [TreeRow]
      :or {TreeRow TreeRow}}]
  (e/client
    (dom/fieldset (dom/props {:class "entity dustingetz-entity-browser3__block"})
      (let [{selected ::selection, authoritative-search ::search} state
            hfql-cols! (e/server (or hfql-cols! ['*]))
            search (dom/legend (dom/span (dom/props {:class "title"}) (dom/text (e/server (pr-str (symbolic-title (key kv))) #_"use sitemap page name") " "))
                     (Search! authoritative-search))
            x (e/server (e/for [x (e/diff-by identity (e/as-vec (val kv)))] x)) ; safe meta
            xs! (e/server #_(ex/Offload-latch (fn []))
                  (when x (-> (hf-pull3 *hfql-bindings hfql-cols! x)
                            (walker hfql-cols! (fn [& kv] (dustingetz.str/any-matches? kv authoritative-search)))
                            vec)))
            row-count (e/server (count xs!)), row-height 24]
        (dom/props {:style {:--column-count 2 :--row-height row-height}})
        (e/amb
          search
          (Intercept (e/fn [index] (TablePicker! ::selection index row-count
                                     (e/fn [index] (e/server (some->> (nth xs! index nil)
                                                               (TreeRow hfql-cols!)))) ; no ColumnPicker
                                     :row-height row-height
                                     :column-count 2))
            selected
            (e/fn Unparse [selected]
              (e/server (if (= :page (first selected))
                          (let [default-select (-> hfql-cols! meta :hf/select)
                                selected? (fn [[path _v _branch? :as row]]
                                            (let [x (reduce hf-nav2 x path)
                                                  card-many? (or (sequential? x) (set? x))
                                                  ?s (when-not card-many? (identify x))]
                                              (= selected
                                                (build-selection
                                                  (or (-> row meta :hf/select) default-select)
                                                  {'% (or ?s x)}))))]
                            (find-index selected? xs!))
                          (find-index (comp #{selected} first) xs!))))
            (e/fn Parse [index] (e/server
                                  (when-some [[path v branch? :as row] (nth xs! index nil)]
                                    (let [x (reduce hf-nav2 x path) ; hydrated
                                          card-many? (or (sequential? x) (set? x))
                                          component? (map? x) ; ?
                                          row-select (-> row meta :hf/select)
                                          default-select (or (-> x meta :hf/select) (-> hfql-cols! meta :hf/select))
                                          ?s (when-not card-many? (identify x))]
                                      #_(prn 'TreeBlockSelect ?s card-many? component? select x)
                                      (cond ; guard illegal navs
                                        row-select (build-selection row-select {'% (or ?s x)})
                                        (and ?s default-select) (build-selection default-select {'% ?s}) ; FIXME wrong '% - should be e not v. DJG: fixed maybe?
                                        ; dev mode can traverse unidentified values/objects by path descent
                                        ; some objects, such as #{:a :b} (Class :flags) are not HFQL-valid.
                                        ; These objects will route but crash in TableBlock HFQL pull. Should HFQL handle them?
                                        (and ?s (not default-select)) path ; dev mode?
                                        (and component? default-select) path #_ (build-selection default-select {'% v}) ; hf/default-select identified objects only ?
                                        (and component? (not default-select)) path ; dev mode ?
                                        card-many? path ; always navigable, dev mode? uses path descent not identified uri
                                        () nil)))))) )))))

(e/declare whitelist)
(e/defn Resolve [fq-sym fallback] (get whitelist fq-sym fallback))

(e/declare Render)

#?(:clj
   (defn str-inline-coll [coll]
     (let [coll-count (count coll)
           base (binding [*print-length* 1
                          *print-level* 2]
                  (-> (pprint-str coll)
                    (clojure.string/replace (str \newline) (str \space))
                    (clojure.string/trim)))]
       (if (pos? (dec coll-count))
         (str base (clojure.pprint/cl-format false " ~d element~:p" coll-count))
         base))))

(e/defn RenderInlineColl [?e a v pull-expr]
  (let [{:keys [hf/link]} (meta pull-expr)
        v-str (e/server (str-inline-coll v))] ; summarize
    (if-some [[qsym & args] link]
      (router/link ['. `[[~qsym ~@args]]] (dom/text v-str))
      (dom/text v-str))))

(defn safe-vary-meta [obj f & args]
  (if #?(:clj (instance? clojure.lang.IObj obj) :cljs (satisfies? IMeta obj))
    (vary-meta obj f args)
    obj))

(e/defn RenderCell [?e a v hfql-col] ; both Collection and Tree
  (let [{:keys [hf/link hf/Render hf/Tooltip]} (meta hfql-col)]
    (dom/td ; custom renderer runs in context of table cell
      (let [Continuation (e/fn [?e a v pull-expr]
                           (when ?e ; expensive n×m ; what is this check useful for?
                             (let [Tooltip (Resolve Tooltip nil)] ; used to be unglitched with e/for - seems harmless now - perf boost
                               #_(e/for [Tooltip (e/diff-by identity (e/as-vec (Resolve Tooltip nil)))])
                               (when (and Tooltip (dom/Mouse-over?)) (dom/props {:data-tooltip (Tooltip ?e a v pull-expr)})))
                             (if (coll? v)
                               (RenderInlineColl ?e a v pull-expr)
                               (let [v-sym (or (identify v) v) ; strip server refs, route must serialize ; if user put a link on some non-identifiable thing it will generate a bad route.
                                     v-str (#_str pr-str v)] ; render identified not ref, is that right?
                                 (if-some [[qsym & args] link]
                                   (router/link ['.. `[[~qsym ~@(replace (assoc ?e a v-sym) args)]]] (dom/text v-str))
                                   (dom/text v-str))))))]
        (binding [dustingetz.entity-browser3/Render Continuation]
          (e/$ (Resolve Render Continuation) ?e a v (safe-vary-meta hfql-col dissoc :hf/Render)))))))

(e/defn Render [?e a v hfql-col] (RenderCell ?e a v hfql-col))

(e/defn CollectionRow [hfql-cols! picked-cols ?x]
  ; ?x cannot be a simple value e.g. keyword, objects/records only
  (e/server
    (let [hfql-col-index (index-by (fn [x & _] x) hfql-cols!)
          cols! (e/as-vec picked-cols)
          ?e #_(ex/Offload-latch (fn [])) (when ?x (hf-pull3 *hfql-bindings cols! ?x))]
      (e/for [a picked-cols] ; n × m
        (Render ?e a (get ?e a) (get hfql-col-index a))))))

(defn- id->idx [id xs!] ; TODO unify with id->index
  (first (eduction (map-indexed vector)
           (keep (fn [[i v]] (when (= id (or (identify v) v)) i)))
           (take 1)
           xs!)))

(defn pull-top-level-attrs [pull-spec]
  (mapcat (fn [attr] (if (map? attr) (keys attr) (list attr))) pull-spec))

(defn has-top-level-splat? [pull-spec] (boolean (some #{'*} (pull-top-level-attrs pull-spec))))

(comment
  (has-top-level-splat? '[]) := false
  (has-top-level-splat? '[:a]) := false
  (has-top-level-splat? '[*]) := true
  (has-top-level-splat? '[:a *]) := true
  (has-top-level-splat? '[:a {* [:db/id]}])
  )

(defn column-inference-type [pull-spec]
  (if (has-top-level-splat? pull-spec)
    (if (= 1 (count pull-spec))
      ::all-inferred
      ::spec-selected-inferred)
    ::spec-selected))

(defn available-columns [pull-spec inferred-cols]
  (->> (case (column-inference-type pull-spec)
         ::all-inferred inferred-cols
         ::spec-selected-inferred (concat (pull-top-level-attrs pull-spec) inferred-cols)
         ::spec-selected (pull-top-level-attrs pull-spec))
    (remove #{'*})
    (distinct)))

(defn selected-columns [pull-spec inferred-cols]
  (->> (case (column-inference-type pull-spec)
         ::all-inferred inferred-cols
         ::spec-selected-inferred (pull-top-level-attrs pull-spec)
         ::spec-selected (pull-top-level-attrs pull-spec))
    (remove #{'*})
    (into #{})))

(defn ->short-keyword-map [cols-available!]
  (let [k* (filterv keyword? cols-available!)
        freq (frequencies (mapv unqualify k*))]
    (into {} (map #(let [unq (unqualify %)] [% (if (= 1 (freq unq)) unq %)]))
      k*)))

(defn column-shortener [symbolic-columns]
  (let [short-keyword-map (->short-keyword-map symbolic-columns)]
    (fn [symbolic-column]
      (cond
        (keyword? symbolic-column) (short-keyword-map symbolic-column)
        (seq? symbolic-column) (let [[qs & args] symbolic-column] (list* (unqualify qs) args))
        () (str symbolic-column)))))

(e/defn ColumnPicker [pull-spec col-opts!]
  (e/server
    (let [cols-available! (distinct (available-columns pull-spec col-opts!)) #_(->> (concat pull-spec col-opts!) (remove #{'*}) (distinct))
          selected? (selected-columns pull-spec col-opts!)
          shorten (e/client (column-shortener cols-available!))]
      (e/for [col (e/diff-by identity cols-available!)]
        (if (e/client (Checkbox (e/server (selected? col))
                        :label (shorten col)))
          col (e/amb))))))

(defn infer-cols [x]
  (let [x (datafy x)] ; *
    (cond
      (map? x) (keys x) ; keys crash on datoms type w/ no datafy
      () nil)))

(e/defn TableBlock2 ; Like TableBlock but takes xs! instead of (fn [search] xs!)
  [field-name kv state hfql-cols!
   & {:keys [Row]
      :or {Row CollectionRow}}]
  (e/client
    (dom/fieldset
      (dom/props {:class "entity-children dustingetz-entity-browser3__block"})
      (let [{selected ::selection, authoritative-search ::search} state
            select (e/server (-> hfql-cols! meta :hf/select))
            hfql-cols! (e/server (or hfql-cols! ['*]))
            !search (atom nil), search (e/watch !search)
            path (e/server (key kv)),
            ;; TODO filter should happen at query time, not here, tbd
            xs!-with-meta (e/server (val kv))
            xs! (e/server (hfql-search-sort *hfql-bindings hfql-cols! authoritative-search xs!-with-meta))
            row-count (e/server (count xs!)), row-height 24
            cols (dom/legend
                   (dom/span (dom/props {:class "title"}) (dom/text (e/server (pr-str (symbolic-title path))) " "))
                   (reset! !search (Search! authoritative-search))
                   (dom/text " (" row-count " items) ")
                   (e/server (ColumnPicker hfql-cols! #_(ex/Offload-latch (fn []))
                               (when (seq xs!) (infer-cols (nav xs! 0 (nth xs! 0)))))))
            column-count (e/server (e/Count cols))]
        (e/amb
          (e/When search search)
          (dom/table ; for table header - optional POC
            (dom/props {:style {:--row-height (str row-height "px"), :--column-count column-count}})
            (dom/thead ; POC - safe to comment out
              (dom/tr
                (let [shorten (column-shortener (e/as-vec cols))]
                  (e/for [col cols]
                    (dom/th (dom/props {:title (str col)})
                      (dom/text (shorten col)))))))
            (Intercept
              (e/fn [index] (TablePicker! ::selection index row-count
                              (e/fn [index] (e/server (some->> (nth xs! index nil)
                                                        (nav xs!-with-meta index)
                                                        (Row hfql-cols! cols))))
                              :row-height row-height
                              :column-count column-count
                              :as :tbody))
              selected
              ;; path->index
              (e/fn Unparse [p-next] (let [id (first p-next)]
                                       (e/server
                                         (->> (eduction (map #(or (identify %) %)) xs!)
                                           (find-index (if (= :page id)
                                                         (fn [x] (= p-next (build-selection select {'% x})))
                                                         #{id}))))))
              ;; index->path
              (e/fn Parse [index] (e/server (let [x (nth xs! index nil) ; maybe sym maybe object
                                                  !x (nav xs!-with-meta index x) ; hydrate object
                                                  symbolic-x (identify !x)] ; local-path
                                              #_(prn 'TableBlockSelect symbolic-x x !x)
                                              (e/When symbolic-x ; only nav if row is identifiable. TODO render EdnBlock if not identifiable.
                                                (cond
                                                  select (build-selection select {'% symbolic-x})
                                                  ()      [symbolic-x]))))))))))))

(def table-block-css
"
.dustingetz-entity-browser3__block table { display: grid; grid-template-columns: repeat(var(--column-count), 1fr);  grid-template-rows: var(--row-height);}

.dustingetz-entity-browser3__block table thead { display: contents; }
.dustingetz-entity-browser3__block table thead tr { display: grid; grid-row: 1; grid-column: 1 / -1; grid-template-columns: subgrid;}
.dustingetz-entity-browser3__block table thead tr th { white-space: nowrap; text-overflow: ellipsis; overflow: hidden; }

.dustingetz-entity-browser3__block .hyperfiddle-electric-forms4__table-picker { grid-row: 2; grid-column: 1 / -1; grid-template-columns: subgrid; }

"
)

(e/defn MarkdownBlock [field-name kv _selected & _]
  (dom/fieldset
    (dom/legend (dom/text (e/server (pr-str (symbolic-title (key kv))))))
    (dom/div
      (set! (.-innerHTML dom/node) (e/server (md/md-to-html-string (val kv)))))))

(defn infer-block-type [x]
  (cond
    (or (set? x) ; align with explorer-seq which indexes sets
      (sequential? x)) :table ; datafy does not alter cardinality, do not need to call it
    (string? x) :string
    (map? (datafy x)) :tree ; fixme gross, how to detect scalars? Can we not emit selection on scalars instead?
    () :scalar))

(e/declare *hfql-spec)

(e/defn Block [kv locus]
  (e/client
    (let [x (e/server #_datafy (val kv))]
      (when-some [F (e/server (case (infer-block-type x) :tree TreeBlock :table TableBlock2 :string MarkdownBlock :scalar nil nil))]
        (Interpreter {::selection (e/fn [path]
                                    (let [[state] router/route]
                                      (if path
                                        (router/Navigate! ['. [(assoc state ::selection path)]])
                                        (router/Navigate! ['. [(dissoc state ::selection)]])))
                                    [:hyperfiddle.electric-forms4/ok])
                      ::search (e/fn [search]
                                 (if (seq search)
                                   (router/Navigate! ['. (assoc-in (vec router/route) [0 ::search] search)])
                                   (router/Navigate! ['. (update (vec router/route) 0 dissoc ::search)])))}
          (F nil kv locus *hfql-spec))))))

(defn- id->index [id xs!] ; TODO unify with id->idx
  (first (eduction (map-indexed vector)
           (keep (fn [[i x]] (when (= id (or (identify x) x)) i)))
           (take 1)
           xs!)))

(e/declare *sitemap)

(e/defn BrowsePath [kv]
  (e/client
    (let [state (first router/route)]
      (Block kv state)
      (when (some? state)
        (router/pop
          (e/for [{::keys [selection]} (e/diff-by ::selection (e/as-vec state))] ; don't reuse DOM/IO frames across different objects
            (when selection
              (if (= :page (first selection))
                (let [k (vec (next selection))
                      [F$ & args] k]
                  (binding [*hfql-spec (e/server (get *sitemap F$))]
                    (BrowsePath
                      (e/server
                        (map-entry k
                          (e/Apply (get pages F$) args))))))
                (let [kv (e/server #_(ex/Offload-reset (fn []))
                                   (case (infer-block-type (val kv))
                                     :table (let [xs (val kv) ; preserve metas
                                                  index (or (id->index (first selection) (datafy xs)) (first selection))]
                                              (map-entry selection (hf-nav2 xs index)))
                                     (let [x (reduce hf-nav2 (val kv) selection)
                                           title selection #_(if *dev-mode* (pr-str (let [x (val kv)] (or (identify x) x))))
                                           ]
                                       (map-entry title x))))]
                  ;; stacked views should get '*. First `Block` renders above
                  (binding [*hfql-spec (e/server ['*])]
                    (BrowsePath kv)))))))))))

(e/defn BrowsePathWrapper [v]
  (let [k (first router/route)]
    (router/pop (BrowsePath (e/server (map-entry k v))))))

(declare css) ; careful, used twice

(e/defn HfqlRoot
  [sitemap
   & {:keys [default]
      :or {default nil}}]
  (e/client
    #_(dom/pre (dom/text (pr-str r/route)))
    (dom/style (dom/text css tooltip/css))
    (TooltipArea
      (e/fn [] (Tooltip)
        (dom/div (dom/props {:class "Browser"})
          (e/for [route (e/diff-by first (e/as-vec router/route))] ; reboot top-level page
            (binding [router/route route]
              (let [[fiddle & _] (first router/route)]
                (if-not fiddle
                  (router/ReplaceState! ['. default])
                  (let [Fiddle (get pages fiddle NotFoundPage)]
                    (set! (.-title js/document) (str (some-> fiddle name (str " – ")) "Hyperfiddle"))
                    (dom/props {:class (css-slugify fiddle)})
                    (binding [*sitemap (e/server (identity sitemap))
                              *hfql-spec (e/server (get sitemap fiddle []))] ; cols don't serialize perfectly yet fixme
                      (BrowsePathWrapper (e/server (e/Apply Fiddle (nfirst router/route)))))))))))))))

(def css
  (str hyperfiddle.electric-forms4/css
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

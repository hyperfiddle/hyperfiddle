(ns dustingetz.entity-browser3
  (:require [clojure.datafy :refer [datafy]]
            [clojure.core.protocols :refer [nav]]
            clojure.set
            [contrib.data :refer [index-by unqualify index-of map-entry]]
            [dustingetz.str :refer [pprint-str]]
            [hyperfiddle.nav0 :refer [identify]]
            [dustingetz.easy-table :refer [Load-css]] ; todo
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
  (if (vector? sexpr)
    (if (and (= 1 (count sexpr)) (ident? (first sexpr)))
      (first sexpr)
      (seq sexpr))
    sexpr))

(defn compose-edits [edits-vec !v]
  {::forms/token (let [tokens (mapv ::forms/token edits-vec)]
                   (fn token ; fresh if ts changes (should fields be disabled when commit pending?)
                     ([] (doseq [t tokens] (t)))
                     ([_err] #_(doseq [t tokens] (t err ::forms/keep)))))
   ::forms/name ::block
   ::forms/value (swap! !v (fn [v] (reduce (fn [v {::forms/keys [name value]}]
                                             (if value (assoc v name value) (dissoc v name)))
                                     v edits-vec)))
   ::forms/validation (not-empty (remove nil? (map ::forms/validation edits-vec)))})

(e/defn CollectBlockEdits [state edits]
  (let [!v (atom (e/snapshot state)), edits-vec (e/as-vec edits)]
    (e/When (seq edits-vec)
      (compose-edits edits-vec !v))))

(defn find-index [pred x*]
  (transduce (keep-indexed (fn [idx x] (when (pred x) idx))) (fn ([v] v) ([_ac nx] (reduced nx))) nil x*))

(e/defn TreeBlock
  [field-name kv state hfql-cols!
   & {:keys [TreeRow]
      :or {TreeRow TreeRow}}]
  (e/client
    (dom/fieldset (dom/props {:class "entity"})
      (let [{selected ::selection, authoritative-search ::search} state
            hfql-cols! (e/server (or hfql-cols! ['*]))
            search (dom/legend (dom/text (e/server (pr-str (symbolic-title (key kv))) #_"use sitemap page name") " ")
                               (Search! authoritative-search))
            x (e/server (e/for [x (e/diff-by identity (e/as-vec (val kv)))] x)) ; safe meta
            xs! (e/server #_(ex/Offload-latch (fn []))
                          (when x (-> (hf-pull3 *hfql-bindings hfql-cols! x)
                                    (walker hfql-cols! (fn [& kv] (dustingetz.str/any-matches? kv authoritative-search)))
                                    vec)))
            row-count (e/server (count xs!)), row-height 24]
        (dom/props {:style {:--col-count 2 :--row-height row-height}})
        (CollectBlockEdits
          state
          (e/amb
            search
            (Intercept (e/fn [index] (TablePicker! ::selection index row-count
                                       (e/fn [index] (e/server (some->> (nth xs! index nil)
                                                                 (TreeRow hfql-cols!)))) ; no ColumnPicker
                                       :row-height row-height))
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
                                          () nil)))))) ))))))

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
                           (when ?e ; expensive n×m
                             (e/for [Tooltip (e/diff-by identity (e/as-vec (Resolve Tooltip nil)))] ; glitch reboot
                               (when (and Tooltip (dom/Mouse-over?)) (dom/props {:data-tooltip (Tooltip ?e a v pull-expr)})))
                             (let []
                               (if (coll? v)
                                 (RenderInlineColl ?e a v pull-expr)
                                 (let [v-sym (or (identify v) v) ; strip server refs, route must serialize ; if user put a link on some non-identifiable thing it will generate a bad route.
                                       v-str (#_str pr-str v)] ; render identified not ref, is that right?
                                   (if-some [[qsym & args] link]
                                     (router/link ['.. `[[~qsym ~@(replace (assoc ?e a v-sym) args)]]] (dom/text v-str))
                                     (dom/text v-str)))))))]
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

(e/defn ColumnPicker [pull-spec col-opts!]
  (e/server
    (let [cols-available! (distinct (available-columns pull-spec col-opts!)) #_(->> (concat pull-spec col-opts!) (remove #{'*}) (distinct))
          selected? (selected-columns pull-spec col-opts!)
          short-keyword-map (->short-keyword-map cols-available!)]
      (e/for [col (e/diff-by identity cols-available!)]
        (if (e/client (Checkbox (e/server (selected? col))
                        :label (cond
                                 (keyword? col) (short-keyword-map col)
                                 (seq? col) (let [[qs & args] col] (list* (unqualify qs) args))
                                 () (str col))))
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
      (dom/props {:class "entity-children"})
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
                   (dom/text (e/server (pr-str (symbolic-title path))) " ")
                   (reset! !search (Search! authoritative-search))
                   (dom/text " (" row-count " items) ")
                   (e/server (ColumnPicker hfql-cols! #_(ex/Offload-latch (fn [])) (-> xs! first infer-cols))))]
        (dom/props {:style {:--col-count (e/server (e/Count cols)) :--row-height row-height}})
        (CollectBlockEdits
          state
          (e/amb
            (e/When search search)
            (Intercept
              (e/fn [index] (TablePicker! ::selection index row-count
                              (e/fn [index] (e/server (some->> (nth xs! index nil)
                                                        (nav xs!-with-meta index)
                                                        (Row hfql-cols! cols))))
                              :row-height row-height))
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
                                                  ()      [symbolic-x])))))) ))))))

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
        (Interpreter {::block (e/fn [block-state] (router/Navigate! ['. (if block-state [block-state] [])])
                                 [:hyperfiddle.electric-forms4/ok])}
          (F ::block kv locus *hfql-spec))))))

(defn- id->index [id xs!] ; TODO unify with id->idx
  (first (eduction (map-indexed vector)
           (keep (fn [[i v]] (when (= id (identify v)) i)))
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
                                     :table (let [value (vec (val kv))
                                                  index (or (id->index (first selection) (datafy value)) (first selection))]
                                              (map-entry selection (hf-nav2 value index)))
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
    (Load-css "dustingetz/easy_table.css") (dom/style (dom/text css tooltip/css))
    (TooltipArea
      (e/fn [] (Tooltip)
        (dom/div (dom/props {:class (str "Browser dustingetz-EasyTable")})
          (e/for [route (e/diff-by first (e/as-vec router/route))] ; reboot top-level page
            (binding [router/route route]
              (let [[fiddle & _] (first router/route)]
                (if-not fiddle
                  (router/ReplaceState! ['. default])
                  (let [Fiddle (get pages fiddle NotFoundPage)]
                    (set! (.-title js/document) (str (some-> fiddle name (str " – ")) "Hyperfiddle"))
                    (binding [*sitemap (e/server (identity sitemap))
                              *hfql-spec (e/server (get sitemap fiddle []))] ; cols don't serialize perfectly yet fixme
                      (BrowsePathWrapper (e/server (e/Apply Fiddle (nfirst router/route)))))))))))))))

(declare css)
(e/defn EntityBrowser2 [kv]
  (e/client (dom/style (dom/text css)) (Load-css "dustingetz/easy_table.css")
    (dom/div (dom/props {:class (str "Browser dustingetz-EasyTable")})
      (BrowsePath kv))))

(def css "
.Browser fieldset { position: relative; }
.Browser fieldset > .Viewport { height: calc(var(--row-height) * 15 * 1px); }
:where(.Browser fieldset.entity)          table { grid-template-columns: 15em auto; }
.Browser fieldset.entity-children table { grid-template-columns: repeat(var(--col-count), 1fr); }

/* table cell tooltips */
.Browser td {position: relative;}
.Browser .dustingetz-tooltip >       span { visibility: hidden; }
.Browser .dustingetz-tooltip:hover > span { visibility: visible; pointer-events: none; }
.Browser .dustingetz-tooltip > span {
  position: absolute; top: 20px; left: 10px; z-index: 2; /* interaction with row selection z=1 */
  margin-top: 4px; padding: 4px; font-size: smaller;
  box-shadow: 0 0 .5rem gray; border: 1px whitesmoke solid; border-radius: 3px; background-color: white; }")

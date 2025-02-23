(ns dustingetz.entity-browser2
  (:require [clojure.datafy :refer [datafy]]
            [clojure.core.protocols :refer [nav]]
            clojure.set
            [contrib.data :refer [index-by unqualify index-of map-entry]]
            [contrib.str]
            [hyperfiddle.nav0 :refer [identify]]
            [dustingetz.easy-table :refer [Load-css]]
            [dustingetz.treelister3 :as tl]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric3-contrib :as ex]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.electric-forms4 :refer [Intercept Interpreter Checkbox TablePicker!]]
            [hyperfiddle.router4 :as router]
            [hyperfiddle.rcf :refer [tests]]
            #?(:clj [markdown.core :as md])
            [contrib.debug :as dbg]
            #?(:clj [dustingetz.hfql11 :refer [hf-pull hf-pull2]])))

(defn datafy-nav-in [x path] ; datafy enables generic descent as an associative, self-describing tree
  #_(map-entry path)
  (loop [m (datafy x), path path]
    (if-some [[p & ps] (seq path)]
      (let [v (get m p)]
        (recur (nav m p v) ps))
      m)))

(tests
  (nav {:a 1} :a 1) := 1
  (datafy-nav-in {:a 1} [:a]) := 1
  (datafy-nav-in {:a {:b 2}} [:a :b]) := 2
  (datafy-nav-in {} nil) := {}
  (datafy-nav-in {} []) := {}
  (datafy-nav-in [] nil) := []
  (datafy-nav-in [] []) := [])

(defn datafy-pull-1 [x path] ; weirdo, hfql will be better
  (map-entry path (datafy-nav-in x path)))

(tests
  (datafy-pull-1 [] nil) := [nil []]
  (datafy-pull-1 {} nil) := [nil {}]
  (datafy-pull-1 {:a {:b 2}} [:a :b]) := (map-entry [:a :b] 2))

(e/defn TreeRow [[path value branch?]]
  #_(e/server (prn 'Q path value branch?)) ; all can be unserializable
  (let [name (e/server (peek path))]
    (e/client
      (dom/td (dom/props {:style {:padding-left (e/server (some-> path count dec (* 15) (str "px")))}})
        (dom/span (dom/props {:class "dustingetz-tooltip"}) ; hover anchor
          (dom/text (e/server (if (keyword? name) ; glitch
                                (unqualify name) (str name)))) ; label
          ; fixme tooltip bumps scrollHeight at bottom boundary by 4px, glitching scroll math
          #_(dom/span (dom/text (e/server (pr-str name))))))
      (dom/td
        (let [v-str (e/server (pr-str value))]
          (if (e/server (fn? value)) ; fns encode hyperlinks (on the server!)
            (dom/text "...")
            (dom/text (#(when-not branch? %) v-str))))))))

;; no unqualify of keywords
(e/defn TreeRow2 [[path value branch?]]
  #_(e/server (prn 'Q path value branch?)) ; all can be unserializable
  (let [name (e/server (peek path))]
    (e/client
      (dom/td (dom/props {:style {:padding-left (e/server (some-> path count dec (* 15) (str "px")))}})
        (dom/span (dom/props {:class "dustingetz-tooltip"}) ; hover anchor
          (dom/text (e/server (str name))) ; label
          ; fixme tooltip bumps scrollHeight at bottom boundary by 4px, glitching scroll math
          #_(dom/span (dom/text (e/server (pr-str name))))))
      (dom/td
        (let [v-str (e/server (pr-str value))]
          (if (e/server (fn? value)) ; fns encode hyperlinks (on the server!)
            (dom/text "...")
            (dom/text (#(when-not branch? %) v-str))))))))

(e/defn Search [] (dom/input (dom/On "input" #(-> % .-target .-value) (.-value dom/node))))

#?(:clj (defn title [m]
          (or (some-> m meta :clojure.datafy/obj identify)
            (some-> m meta :clojure.datafy/class str))))

(e/defn TreeBlock
  [field-name kv p-next
   & {:keys [cols TreeRow]
      :or {cols ['*], TreeRow TreeRow}}]
  (e/client
    (dom/fieldset (dom/props {:class "entity"})
      (let [search (dom/legend (dom/text (e/server (pr-str (key kv))) " ") (Search))
            x (e/server (e/for [x (e/diff-by identity (e/as-vec (val kv)))] x))
            xs! (e/server #_(ex/Offload-latch (fn [])) (when x (->> ((hf-pull2 cols) {'% #_(datafy) x}) ; control descent
                                                                 (tl/treelist
                                                                   tl/data-children
                                                                   #(contrib.str/includes-str? % search))
                                                                 (drop 1))))
            row-count (e/server (count xs!)), row-height 24
            selected-x (e/server (first (filter (fn [[path _ _]] (= p-next path)) xs!)))] ; slow, but the documents are small
        (dom/props {:style {:--col-count 2 :--row-height row-height}})
        (Intercept (e/fn [index] (TablePicker! field-name index row-count
                                   (e/fn [index] (e/server (some-> (nth xs! index nil) TreeRow)))
                                   :row-height row-height))
          selected-x
          (e/fn Unparse [x] (e/server (index-of xs! x)))
          (e/fn Parse [index] (e/server (first (nth xs! index nil))))))))) ; keep path, drop value

;; custom (children-fn search x), to control traversal
(e/defn TreeBlock2
  [field-name kv p-next children-fn
   & {:keys [cols TreeRow]
      :or {cols ['*], TreeRow TreeRow2}}]
  (e/client
    (dom/fieldset (dom/props {:class "entity"})
      (let [!search (atom ""), search (e/watch !search)
            x (e/server (e/for [x (e/diff-by identity (e/as-vec (val kv)))] x))
            xs! (e/server #_(ex/Offload-latch (fn [])) (when x (->> ((hf-pull2 cols) {'% #_(datafy) x}) ; control descent
                                                                 (children-fn search)
                                                                 (drop 1))))
            row-count (e/server (count xs!)), row-height 24
            selected-x (e/server (first (filter (fn [[path _ _]] (= p-next path)) xs!)))] ; slow, but the documents are small
        (dom/legend
          (dom/text (e/server (pr-str (key kv))) " ")
          (reset! !search (Search))
          (dom/text " (" row-count " items) "))
        (dom/props {:style {:--col-count 2 :--row-height row-height}})
        (Intercept (e/fn [index] (TablePicker! field-name index row-count
                                   (e/fn [index] (e/server (some-> (nth xs! index nil) TreeRow)))
                                   :row-height row-height))
          selected-x
          (e/fn Unparse [x] (e/server (index-of xs! x)))
          (e/fn Parse [index] (e/server (first (nth xs! index nil)))))))))

(e/declare whitelist)
(e/defn Resolve [fq-sym fallback] (get whitelist fq-sym fallback))

(e/declare Render)

(e/defn RenderCell [?e a v pull-expr]
  (let [{:keys [hf/link hf/Render hf/Tooltip]} (meta pull-expr)]
    (dom/td ; custom renderer runs in context of table cell
      (let [Continuation (e/fn [?e a v pull-expr]
                           (when ?e ; expensive n×m
                             (e/for [Tooltip (e/diff-by identity (e/as-vec (Resolve Tooltip nil)))] ; glitch reboot
                               (when Tooltip (dom/props {:data-tooltip (Tooltip ?e a v pull-expr)})))
                             (let [v-str (str #_pr-str v)]
                               (if-some [[qsym & args] link]
                                 (router/link ['.. `[~qsym ~@(replace (assoc ?e a v) args)]] (dom/text v-str))
                                 (dom/text v-str)))))]
        (binding [dustingetz.entity-browser2/Render Continuation]
          (e/$ (Resolve Render Continuation) ?e a v (vary-meta pull-expr dissoc :hf/Render)))))))

(e/defn Render [?e a v pull-expr] (RenderCell ?e a v pull-expr))

(e/defn CollectionRow [hfql-cols! picked-cols ?x]
  (e/server
    (let [index (index-by (fn [x & _] x) hfql-cols!)
          cols! (e/as-vec picked-cols)
          ?e #_(ex/Offload-latch (fn [])) (when ?x ((hf-pull2 cols!) {'% ?x}))]
      (e/for [a picked-cols] ; n × m
        (Render ?e a (get ?e a) (get index a))))))

(defn- id->idx [id xs!]
  (first (eduction (map-indexed vector)
           (keep (fn [[i v]] (when (= id (identify v)) i)))
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

(e/defn ColumnPicker [pull-spec col-opts!]
  (e/server
    (let [cols-available! (distinct (available-columns pull-spec col-opts!)) #_(->> (concat pull-spec col-opts!) (remove #{'*}) (distinct))
          selected? (selected-columns pull-spec col-opts!)]
      (e/for [col (e/diff-by identity cols-available!)]
        (if (e/client (Checkbox (e/server (selected? col))
                        :label (cond (keyword? col) (unqualify col) () (str col))))
          col (e/amb))))))

(defn infer-cols [x]
  (let [x (datafy x)] ; *
    (cond
      (map? x) (keys x) ; keys crash on datoms type w/ no datafy
      () nil)))

(e/defn TableBlock
  [field-name kv selected
   hfql-cols! ; server
   & {:keys [Row]
      :or {Row CollectionRow}}]
  (e/client
    (dom/fieldset
      (dom/props {:class "entity-children"})
      (let [!search (atom ""), search (e/watch !search)
            p (e/server (key kv)), !query (e/server (val kv))
            !query (e/server (e/for [!query (e/diff-by identity (e/as-vec !query))] !query))
            xs! (e/server #_(ex/Offload-latch (fn [])) ((fn [] (when !query (->> (!query search) (datafy) ; datafy java coll?
                                                                              (into []))))))
            row-count (e/server (count xs!)), row-height 24
            cols (dom/legend (dom/text (e/server (pr-str p)) " ")
                   (reset! !search (Search))
                   (dom/text " (" row-count " items) ")
                   (e/server (ColumnPicker hfql-cols! #_(ex/Offload-latch (fn [])) (-> xs! first infer-cols))))]
        (dom/props {:style {:--col-count (e/server (e/Count cols)) :--row-height row-height}})
        (Intercept
          (e/fn [index] (TablePicker! field-name index row-count
                       (e/fn [index] (e/server (some->> (nth xs! index nil) (Row hfql-cols! cols))))
                       :row-height row-height))
          selected
          ;; path->index
          (e/fn Unparse [p-next] (let [id (first p-next)]
                                   (e/server (id->idx id xs!))))
          ;; index->path
          (e/fn Parse [index] [(e/server (some-> (nth xs! index nil) identify) #_(when (contains? xs! index) index))]))))))

(e/defn MarkdownBlock [field-name kv _selected]
  (dom/fieldset
    (dom/legend (dom/text (e/server (pr-str (key kv)))))
    (dom/div
      (set! (.-innerHTML dom/node) (e/server (md/md-to-html-string (val kv)))))))

(defn infer-block-type [x]
  (cond
    (or (set? x) ; align with explorer-seq which indexes sets
      (sequential? x)) :table ; datafy does not alter cardinality, do not need to call it
    (string? x) :string
    (map? (datafy x)) :tree ; fixme gross, how to detect scalars? Can we not emit selection on scalars instead?
    () :scalar))

(e/defn Block [kv locus]
  (e/client
    (let [x (e/server #_datafy (val kv))]
      (when-some [F (e/server (case (infer-block-type x) :tree TreeBlock :table TableBlock :string MarkdownBlock :scalar nil nil))]
        (Interpreter {::select (e/fn [path] (router/Navigate! ['. (if path [path] [])])
                                 [:hyperfiddle.electric-forms4/ok])}
          (F ::select kv locus))))))

(e/defn BrowsePath [kv]
  (e/client
    (let [locus (first router/route)]
      (Block kv locus)
      (when (some? locus)
        (router/pop
          (e/for [locus (e/diff-by identity (e/as-vec locus))] ; don't reuse DOM/IO frames across different objects
            (let [kv (e/server #_(ex/Offload-reset (fn [])) (datafy-pull-1 (val kv) #_(do (prn 'nav locus kv) kv)
                                                            ;; TODO untangle
                                                            ;; in case of table we need to turn the id to an idx.
                                                            ;; But we only know if this is a table *after* nav-in,
                                                            ;; through infer-block-type.
                                                            ;; Probably the path should encode the table|tree information.
                                                            (if-some [id (id->idx (first locus) (datafy (val kv)))] [id] locus)))]
              (BrowsePath kv))))))))

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

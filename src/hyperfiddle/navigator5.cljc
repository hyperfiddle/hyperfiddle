(ns hyperfiddle.navigator5
  {:clj-kondo/config {:lint-as '{hyperfiddle.electric3/defn clojure.core/defn}}}
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-forms5 :as forms]
            [hyperfiddle.hfql1 :as hfql1]
            [hyperfiddle.entrypoint2 :refer [Index]]
            [hyperfiddle.router5 :as router]
            [hyperfiddle.electric-dom3 :as dom]

            [hyperfiddle.ui.tooltip :as tooltip]
            [contrib.css :as cssx]
            [dustingetz.str :as strx]
            [clojure.datafy]
            [clojure.string :as str]
            [clojure.pprint]
            [dustingetz.loader :as loader]
            [geoffreygaillard.predicates :as pred]
            #?(:clj [clojure.tools.logging :as log])
            [geoffreygaillard.debug-tools :refer [debug-exceptions]])
  #?(:cljs (:require-macros hyperfiddle.navigator5)))

(defmacro prn+ [& args]
  (let [{:keys [line column]} (meta &form)]
    `(println ~(str "[" line ":" column "]") (pr-str ~@args))))

(defmacro rebooting [sym & body] `(e/for [~sym (e/diff-by identity (e/as-vec ~sym))] ~@body))

(defn run-with-bindings [bindings f & args]
  #?(:clj (with-bindings bindings (apply f args))))

(e/declare *sitemap *block-opts *depth *server-pretty)
(e/declare ^:dynamic *search)
(declare css)

;;;;;;;;;;;;;
;; Sorting ;;
;;;;;;;;;;;;;

(defmulti comparable type)
(defmethod comparable :default [x] x)
#?(:clj (defmethod comparable java.lang.Class [^Class x] (.getName x)))
(defmethod comparable #?(:clj clojure.lang.Symbol :cljs cljs.core.Symbol) [sym] (str sym))
(defmethod comparable #?(:clj clojure.lang.Keyword :cljs cljs.core.Keyword) [kw] (str (namespace kw) "/" (name kw))) ; drop ":"

#?(:clj
   (defn- ->sort-comparator [sort-spec]
     (debug-exceptions `->sort-comparator
       (let [[[k asc?]] sort-spec
             order (if asc? neg? pos?)   ; TODO support multi-sort
             compare-fn (fn [a b]
                          (cond
                            (nil? a) false
                            (nil? b) true
                            :else (order (compare a b))))]
         (when k
           (fn [a b]
             (debug-exceptions `->sort-comparator-inner
               (compare-fn (comparable (get a k)) (comparable (get b k))))))))))

(comment
  (sort (->sort-comparator [[:k true]]) [{:k 1} {:k 0} {:k 2}])
  (sort (->sort-comparator [[:k false]]) [{:k 1} {:k 0} {:k 2}])
  )

(defn -fsym [sexpr]
  (when (seq? sexpr)
    (if (= 'quote (first sexpr))
      (-fsym (second sexpr))
      (first sexpr))))

(defn sexpr-comparator [a b]
  (debug-exceptions `sexpr-comparator
    (cond
      (nil? a) -1
      (nil? b) 1
      (and (keyword? a) (symbol? b)) (compare a (keyword b))
      (and (symbol? a) (keyword? b)) (compare (keyword a) b)
      (seq? a) (sexpr-comparator (-fsym a) b)
      (seq? b) (sexpr-comparator a (-fsym b))
      :else (compare a b))))

(defn ?sort-by [keyfn v*]
  (try (debug-exceptions `?sort-by
         (sort-by keyfn sexpr-comparator v*))
       (catch #?(:clj Throwable :cljs :default) e
         (let [log-message (str "Skipped sort: some elements are not comparable." (type e) " " (ex-message e))]
           #?(:clj (log/info log-message)
              :cljs (.info js/console log-message)))
         v*)))

;;;;;;;;;;;;;;;;;;;;;
;; Pretty Printing ;;
;;;;;;;;;;;;;;;;;;;;;

(defn- labelize [edge]
  (and edge
    (or (::hfql1/label (meta edge))
      (hfql1/short-form edge) ; accounts for form
      )))

#?(:clj
   (defn- pretty-value [x server-pretty]
     (debug-exceptions `pretty-value
       (if (= ::not-found x)
         ""
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
     (debug-exceptions `str-inline-coll
       (let [coll-count (count coll)
             base (binding [*print-length* 1, *print-level* 2]
                    ;; `symbol` removes double quotes from the strings inside the collection
                    (-> (map-keep-coll #(symbol ((get server-pretty (class %) strx/pprint-str) %)) coll)
                      str
                      (str/replace (str \newline) (str \space))
                      (str/trim)))]
         (cond-> base (> coll-count 1) (str " " coll-count " elements"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extensible Rendering ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti documentation-dispatch type)
(defmethod documentation-dispatch :default [x] (type x))
(defmethod documentation-dispatch (type :kw) [kw] kw)
(defmethod documentation-dispatch (type 'sym) [sym] sym)
(defmethod documentation-dispatch (type "str") [str] str)

(defmulti documentation documentation-dispatch)
(defmethod documentation :default [_] nil)
#?(:clj (defmethod documentation (type #'type) [var] (:doc (meta var))))

#?(:clj (defn- find-documentation [edge]
          (when-let [identifier (hfql1/edge-name edge)]
            (str/triml
              (str (documentation
                     (cond (qualified-symbol? identifier) (resolve identifier) ; FIXME violates HFQL's rule ② – don't resolve
                           ;; TODO extend ; FIXME don't extend, instead have HFQL provide all info ahead of time (e.g. doc, javadoc)
                           ;; (pred/java-method-symbol? identifier) ...
                           :else nil))
                "\n\n")))))

(defn as-coll [v] (if (coll? v) v [v])) ; TODO removeme, glitch guard

(e/defn RenderInlineColl [object edge value]
  (let [pretty-v (str-inline-coll (as-coll value) *server-pretty)
        symbolic-link-href (::hfql1/link (meta edge))]
    (e/Reconcile
      (if (some? symbolic-link-href)
        (router/link ['. [symbolic-link-href]] (dom/text pretty-v))
        (dom/text pretty-v))) ))

(e/defn Resolve [fq-sym fallback] (get e/*exports* fq-sym fallback))

(e/defn RenderTooltip [tooltip-ident object edge value]
  (let [tooltip-ident (get (meta edge) tooltip-ident (get *block-opts tooltip-ident))
        tooltip-qualified-ident (hfql1/unquote tooltip-ident) ; FIXME syntax wart
        Tooltip (Resolve tooltip-qualified-ident nil)]
    (prn+ 'Resolved Tooltip)
    (e/Reconcile
      (when (some? Tooltip)
        (let [show? (dom/Mouse-over?)]
          (dom/props
            {:data-tooltip
             (e/Reconcile (when show? (Tooltip object edge value)))}))))) )

(e/defn DefaultCellRenderer [object edge value]
  (RenderTooltip ::hfql1/Tooltip object edge value)
  (if (or (set? value) (sequential? value))
    (RenderInlineColl object edge value)
    (e/Reconcile
      (if (= ::not-found value)
        (dom/props {:data-empty true})
        (let [pretty-v (pretty-value value *server-pretty)
              denv {'% object, (hfql1/form edge) value, '%v value}
              symbolic-link-href (::hfql1/link (meta edge))]
          (e/Reconcile
            (if (some? symbolic-link-href)
              (router/link ['. [(replace denv symbolic-link-href)]] (dom/text pretty-v))
              (dom/text pretty-v))) )))))

(e/declare Render)
(e/defn RenderCell [object edge value]
  (dom/td                            ; custom renderer runs in context of a cell
    (binding [Render DefaultCellRenderer]
      (e/Reconcile (e/call (Resolve (::hfql1/Render (meta edge)) Render) object (vary-meta edge dissoc ::hfql1/Render) value)))))

(e/defn Render [object edge value] (RenderCell object edge value))

;;;;;;;;;;;;;;;;;;;
;; Edit Handlers ;;
;;;;;;;;;;;;;;;;;;;

(e/defn HandleSearch! [s]
  ;; `update` doesn't work, route is a lazyseq (?)
  (router/ReplaceState! ['. (into (conj (into [] (take *depth) router/route)
                                    (assoc (nth router/route *depth {}) ::search s))
                              (drop (inc *depth) router/route))])
  [::forms/ok])


(e/defn HandleSelect! [symbolic-x]
  (router/Navigate! ['. (conj (into [] (take *depth) router/route)
                          (assoc (nth router/route *depth {}) ::selection symbolic-x))])
  ;; Want the selected row to transition atomically? : (RoundTrip (RoundTrip [::forms/ok]))
  ;; Two roundtrids because:
  ;;  - one roundtrip because TablePicker! receives the selected index from server (latency)
  ;;  - second roundtrip because of an Electric bug leo is fixing
  ;;     - Electric produces an extra roundtrip on e/fn call.
  [::forms/ok])

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependency Injection ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(e/defn Searcher [saved]
  (e/client
    (let [!s (atom (e/Snapshot saved))] ; currently we're the only writer so snapshotting is OK
      [(e/fn [disabled?]
         (->> (forms/Input! ::search saved :type :search :maxlength 100 :disabled disabled?)
           (forms/Parse (e/fn [{search ::search}] (reset! !s search) [`Search! search]))))
       (e/watch !s)])))

(defn- state-stored-in-route [router-route *depth] (nth router-route *depth {}))

(e/defn InjectDependencies [Continuation]
  (let [effect-handlers {`Search! HandleSearch!
                         `Select! HandleSelect!}]
    (binding [*depth (inc *depth)]
      (let [{saved-search ::search} (state-stored-in-route router/route *depth)
            [Search search]         (Searcher saved-search)]
        (binding [*search      search
                  e/*bindings* (e/server (assoc e/*bindings* (find-var `*search) search))]
          (Continuation effect-handlers Search))))))

(e/defn Constantly [x] (e/fn [& _] x))

#?(:clj (defn is-array? [object] (hfql1/is-array? object)))

#?(:clj
   (defn infer-block-type [x]
     (debug-exceptions `infer-block-type
       (cond
         (set? x)                                             :set
         (or (sequential? x) (is-array? x))                   :collection
         (or (number? x) (string? x) (boolean? x) (ident? x) (nil? x)) :scalar
         (fn? x)                                              :query ; TODO remove
         :else                                                :object))))


(e/declare RenderSeqable RenderAssociative)

(e/defn ResolveRenderer [object]
  (e/server
    (list
      (get {:object RenderAssociative
            :collection RenderSeqable
            :set RenderSeqable}
        (infer-block-type object)
        (Constantly nil))
      object) ; HACK glitch guard: Ensures object and its corresponding Renderer update at the same time
    ))

;;;;;;;;;;;;;;;;
;; Navigation ;;
;;;;;;;;;;;;;;;;

(defn- get-%-from-symbolic-edge [symbolic-edge]
  (if (seq? symbolic-edge)
    (second symbolic-edge)
    nil))

(e/defn NavigateSitemapEntry
  ([symbolic-edge]
   (e/server
     (prn+ "NavigateSitemapEntry/1" symbolic-edge)
     (let [[traversal-edge pull-edge] (hfql1/find-traversal *sitemap symbolic-edge)
           %  (get-%-from-symbolic-edge symbolic-edge)
           %  (run-with-bindings e/*bindings* hfql1/nav % traversal-edge)]
       [traversal-edge pull-edge %])))
  ([symbolic-edge %]
   (e/server
     (let [[traversal-edge pull-edge] (hfql1/find-traversal *sitemap symbolic-edge)
           % (run-with-bindings e/*bindings* hfql1/nav % traversal-edge)]
       (prn+ "NavigateSitemapEntry/2" symbolic-edge traversal-edge pull-edge %)
       [traversal-edge pull-edge %]))))

(defn- should-navigation-happen? [object]
  (or (sequential? object)
    (set? object)
    #?(:clj (is-array? object))
    #?(:clj (some? (hfql1/suggest object)))))

(defn- selection->edge [selection]
  (hfql1/lift selection {::hfql1/form selection}))

(e/defn Navigate
  ([symbolic-edge]
   (e/client
     (InjectDependencies
       (e/fn [effect-handlers Search]
         (e/server
           (let [[traversal-edge pull-edge %] (NavigateSitemapEntry symbolic-edge)
                 [Renderer %] (ResolveRenderer %)]
             (prn+ "Navigate/1" symbolic-edge traversal-edge pull-edge (meta %))
             (e/client
               (e/call Renderer traversal-edge pull-edge % effect-handlers Search (state-stored-in-route router/route *depth)))))))))
  ([symbolic-edge %]
   (e/client
     (InjectDependencies
       (e/fn [effect-handlers Search]
         (e/server
           (let [[traversal-edge pull-edge %] (NavigateSitemapEntry symbolic-edge %)
                 traversal-edge (#(or traversal-edge (hfql1/build-pull (map selection->edge symbolic-edge))))
                 [Renderer %] (ResolveRenderer %)]
             (prn+ "Navigate/2" symbolic-edge traversal-edge pull-edge %)
             (e/client
               (e/call Renderer traversal-edge pull-edge % effect-handlers Search (state-stored-in-route router/route *depth))))))))))

;; #?(:clj (defn not-entity-like? [x]
;;           (debug-exceptions `not-entity-like?
;;             (or (nil? x) (boolean? x) (string? x) (number? x) (ident? x) (vector? x) (is-array? x)))))

#_(binding [e/Timing (e/fn [label f] (dustingetz.loader/Offload f {:label label}))])
(e/defn Timing [label f]
  #_(dustingetz.loader/Offload f {:label label}) ; query monitoring, currently unstable
  (e/Offload f)) ; can override from userland

#?(:clj
   (defn ^:deprecated nav* [bindings coll k v]
     ;; (prn "nav*" (meta coll) k v)
     (try (debug-exceptions `nav* (with-bindings bindings (clojure.datafy/nav coll k v)))
          (catch Throwable e (prn+ e) nil))))

(e/defn ^:deprecated Nav [coll k v] (e/server (Timing 'Nav #(nav* e/*bindings* coll k v))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Components ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

#?(:clj
   (defn- kvs-sort-by-ordered-keyset
     "Sort a map structure by keys. Static selected keys (from pull-edge) have
  priority and are laid out in order, as in an ordered map. Extra keys comes later and are sorted by
  `comparator`. Return a seq of map entries."
     [pull-edge kvs]
     {:pre [(hfql1/pull? pull-edge) (every? map-entry? kvs)]
      :post [(every? map-entry? %) (= (count %) (count kvs))]}
     (let [static-keys (distinct (map (comp hfql1/edge-name hfql1/form) (hfql1/value pull-edge)))
           static-keyset (set static-keys)
           [static-kvs dynamic-kvs] (contrib.data/group-by-pred (comp boolean static-keyset key) kvs)
           static-map (into {} static-kvs)]
       (concat (keep (fn [pullspec-key] (find static-map pullspec-key)) static-keys)
         (?sort-by key (remove (comp some? static-keyset key) dynamic-kvs))))))

#?(:clj (defn- realize ; TODO useful enough and hard-to-get-right enough to promote to HFQL
          "Realize a lazy-seq to a plain list. Values that are not a lazy-seq just passes
  through. Differs from `doall` in that `(str (doall seq))` will print an opaque
  object address, whereas `(str (realize seq))` will print as a plain list. Will
  hang forever on infinite seqs."
          [supposedly-lazy-seq]
          (if (seq? supposedly-lazy-seq) (list* supposedly-lazy-seq) supposedly-lazy-seq)))

#?(:clj (defn- kvs-filter-by-needle [kvs needle]
          {:pre [(every? map-entry? kvs) (or (string? needle) (nil? needle))]
           :post [(every? map-entry? %) (<= (count %) (count kvs))]}
          (if (empty? needle)
            kvs
            (filter (fn [[k v]] (or (strx/includes-str? (realize k) needle) (strx/includes-str? v needle)))
              kvs))))

(defn- find-index [pred x*]
  (debug-exceptions `find-index
    (transduce (keep-indexed (fn [idx x] (when (pred x) idx))) (fn ([v] v) ([_ac nx] (reduced nx))) nil x*)))

(defn- find-if [pred x*]
  (debug-exceptions `find-if
    (transduce (keep (fn [x] (when (pred x) x))) (fn ([v] v) ([_ac nx] (reduced nx))) nil x*)))

(e/defn RenderAssociativeEntry [object edge]
  (dom/td (dom/props {:title (e/server (str (hfql1/resolved-form edge)))})
          (dom/text (e/server (labelize edge))))
  (Render object edge (run-with-bindings e/*bindings* hfql1/nav object edge)))

(e/declare AutoNavigate)

(defn- find-edge
  ([form pull-edge] (find-edge form nil pull-edge))
  ([form not-found pull-edge]
   (or (first (filter (comp #{form} hfql1/edge-name hfql1/form) (hfql1/value pull-edge)))
     not-found)))

(defn- resolve-selected-object-BIG-O-N [saved-selection bindings collection]
  #?(:clj
     (debug-exceptions `resolve-selected-object
       (with-bindings bindings
         (some
           #(if (map-entry? %)
              (let [navd (clojure.datafy/nav collection (key %) (val %))] ; FIXME
                ;; (prn+ 'resolve-selected-object-BIG-O-N saved-selection navd)
                (when (= saved-selection (or (hfql1/identify navd) (key %)))
                  navd))
              (let [navd (clojure.datafy/nav collection nil %)] ; FIXME
                ;; (prn+ 'resolve-selected-object-BIG-O-N saved-selection navd)
                (when (= saved-selection (or (hfql1/identify navd) navd))
                  navd)))
           collection)))))

(e/defn RenderAssociative [traversal-edge pull-edge % effect-handlers Search route-state]
  (e/server (prn+ 'RenderAssociative traversal-edge pull-edge % route-state))
  #_(e/client
      (dom/pre (dom/text (e/server (dustingetz.str/pprint-str o)))))
  (e/client
    (let [o                             (e/server (identity %)) ; TODO remove
          {saved-selection ::selection} route-state
          !selected-object              (e/server (atom nil))
          selected-object               (e/server (e/watch !selected-object))
          symbolic-select-edge          (e/server (::hfql1/select (meta pull-edge)))

          edge+suggestions (e/server (let [pull-edge       (e/server (hfql1/value (hfql1/filter-pull (complement hfql1/star?) pull-edge)))
                                           keys            (hfql1/value pull-edge)
                                           resolved-keys   (into #{} (map hfql1/resolved-form keys))
                                           suggested-edges (e/Offload (fn [] (hfql1/suggest %)))
                                           new-edges       (hfql1/filter-pull (fn [edge] (not (resolved-keys (hfql1/resolved-form edge)))) suggested-edges)]
                                       (e/server (def _suggested-edges suggested-edges) nil)
                                       (e/server (def _new-edges new-edges) nil)
                                       (prn "suggested edges" suggested-edges)
                                       (e/Reconcile (if keys
                                                      (hfql1/concat pull-edge new-edges)
                                                      new-edges))
                                       #_(hfql1/build-pull (into keys new-edges))))
          ;; default-select                (e/server (::hfql1/select (meta pull-edge)))
          navigated        (e/server (run-with-bindings e/*bindings* hfql1/nav % edge+suggestions)) #_#_#_hfql1/nav (doto % (->> (prn+ "%"))) pull-edge
          filtered         (e/server (kvs-filter-by-needle navigated #_pulled *search))
          data             (e/server (kvs-sort-by-ordered-keyset edge+suggestions filtered))
          row-count        (e/server (count data))
          row-height       24

          selected-object  (e/server (#(or selected-object (reset! !selected-object (resolve-selected-object-BIG-O-N saved-selection e/*bindings* data)))))
          ]
      (e/server (def _edge+suggestions edge+suggestions) nil)
      ;; (e/server (prn 'edge+suggestions edge+suggestions))
      ;; (e/server (prn 'data data))
      ;; (e/server (prn 'navigated navigated))
      ;; (e/server (prn 'filtered filtered))
      ;; (e/server (prn '% %))
      (comment
        (hfql1/nav {:foo 1} (hfql1/concat (hfql1/hfql []) (hfql1/hfql []))))
      ;; (e/server (prn+ 'symbolic-selecte-edge symbolic-select-edge))
      (binding [*block-opts (e/server (meta pull-edge))]
        (dom/fieldset
          (dom/props {:class "entity hyperfiddle-navigator5__block"})
          (let [search-cmd (dom/legend
                             (dom/span (dom/props {:class "title" :data-tooltip (e/server (dustingetz.str/blank->nil (find-documentation traversal-edge)))})
                                       (dom/text (e/server (hfql1/form traversal-edge)) " "))
                             (Search false))]
            (dom/props {:style {:--column-count 2 :--row-height row-height}})
            #_(dom/pre (dom/text (e/server (dustingetz.str/pprint-str data))))
            (forms/Interpreter effect-handlers
              (e/amb
                (e/When search-cmd search-cmd)
                (let [!selected-index (e/server (atom nil))]
                  (e/server (reset! !selected-index (find-index (fn [[k]] (= k saved-selection)) data)))
                  (->>
                    (->> (forms/TablePicker! ::selection (e/server (e/watch !selected-index)) row-count
                           (e/fn [index] (e/server
                                           (let [[k v] (nth data index nil)]
                                             ;; (prn+ "render row" index k)
                                             (e/Reconcile
                                               (when k
                                                 (RenderAssociativeEntry % (find-edge k k edge+suggestions)))))))
                           :row-height row-height
                           :column-count 2)
                      (forms/Parse (e/fn ToCommand [{selected-index ::selection}]
                                     (e/server
                                       (reset! !selected-index selected-index)
                                       (let [[key object] (nth data selected-index nil)
                                             edge (find-edge key key edge+suggestions)
                                             selected-object (Nav % nil object)] ; FIXME
                                         ;; (prn+ 'selected-object selected-index key object selected-object)
                                         ;; (prn+ 'selected-object-data data #_#_#_#_selected-index key object selected-object)
                                         (def _state (list 'or (hfql1/identify selected-object) (::hfql1/select (meta edge)) key) )
                                         (e/Reconcile
                                           (if (should-navigation-happen? object)
                                             [`Select! (#(or (hfql1/identify selected-object) (::hfql1/select (meta edge)) key)) (e/server (e/fn [] selected-object))]
                                             [`Noop]))))
                                     #_(e/server
                                         (prn "selected index" index (nth data index nil) (map key data))

                                         #_(let [[k object :as x]          (nth data index nil)
                                                 edge       (find-edge k edge+suggestions)
                                                 select     (::hfql1/select (meta edge) (::hfql1/select (meta pull-edge)))
                                                 ;; select     (or row-select default-select)
                                                 ]
                                             #_(e/When (e/Reconcile (and x select))
                                                 (reset! !selected-index index)
                                                 [`Select! (first x)])))))
                      (forms/Interpreter {`Noop (e/fn [] [::forms/ok])})
                      #_(forms/Parse (e/fn ToCommand [saved] [`Select! saved])))
                    (forms/Parse (e/fn [[command & args]]
                                   (e/Reconcile
                                     (case command
                                       `Select! (e/server
                                                  (let [[symbolic-selected-object Get-actual-selected-object] args
                                                        selected-object                                       (Get-actual-selected-object)]
                                                    (reset! !selected-object selected-object)
                                                    [`Select! symbolic-selected-object]))
                                       command))))))))))
        (e/server (AutoNavigate pull-edge selected-object))
        #_(e/server
          (let [next-x (e/server (when-some [nx (find-if (fn [[k _v]] (= k saved-selection)) navigated)] ; when-some because glitch
                                   #_(rebooting nx)
                                   (Nav o (key nx) (val nx))))]
            (AutoNavigate pull-edge next-x)))
        #_(when saved-selection
            (let [next-x     (e/server (when-some [nx (find-if (fn [[k _v]] (= k saved-selection)) pulled)] ; when-some because glitch
                                         (rebooting nx
                                           (Nav o (key nx) (val nx)))))
                  row-select (e/server (-> (find-spec-prop raw-spec saved-selection) ::hfql1/select))
                  select     (or row-select default-select)]
              (rebooting select
                (if select
                  (Navigate select next-x o)
                  (e/server
                    (let [query-template (hfql1/resolve next-x)]
                      (rebooting query-template
                        (e/client
                          (if query-template
                            (Navigate query-template next-x o)
                            (AnonymousBlock saved-selection next-x))))))))))))))

(e/defn AutoNavigate [edge %]
  (e/server
    (let [{saved-selection ::selection} (state-stored-in-route router/route *depth)
          symbolic-select-edge (e/server (::hfql1/select (meta edge)))]
      (prn+ 'AutoNavigate symbolic-select-edge saved-selection)
      (e/Reconcile
        (when (#(boolean (and saved-selection %)) %)
          ;; (rebooting saved-selection)
          (e/Reconcile
            (if symbolic-select-edge
              ;; In ObjectBlock we pass the selected object and the root object.
              ;; Here the root object is a query fn, or the filtered collection.
              ;; Do we want/need to pass it? Should we bind it to `%`?
              (Navigate symbolic-select-edge %)
              (let [symbolic-edge (hfql1/resolve %)]
                #_(rebooting symbolic-edge)
                (e/Reconcile
                  (if symbolic-edge
                    (Navigate symbolic-edge %)
                    (when (should-navigation-happen? %)
                      (Navigate [saved-selection] %))))))))))))

(defn toggle-column-sort [sort-spec for-col]
  (when-some [[col asc?] (first sort-spec)]
    [[for-col (if (= col for-col) (not asc?) true)]]))

(def COLLECTION-LIMIT 10000)

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

(e/defn RenderSeqableRow [object edges]
  (e/for [edge edges] ; shouldn't this go into Render2? not sure due to `get` below
    (Render object edge (run-with-bindings e/*bindings* hfql1/pull edge object))
    ))

#?(:clj
   (defn- nav-row [bindings data row-index]
     (when-some [row-object (nth data row-index nil)]
       [row-index row-object (run-with-bindings bindings clojure.datafy/nav data nil row-object)])))

#?(:clj
   (defn- select-row-if-match-url! [!index row-index row-object selected-object]
     {:pre [(pred/atom-like? !index) (number? row-index)]
      :post [(nil? %)]}
     (if (= row-object selected-object) ; both can be nil
       (reset! !index row-index) ; current row matches. Set selection.
       (compare-and-set! !index row-index nil)) ; current row used to match and now doesn't. Clear selection.
     nil))

(e/defn RenderSeqableBody [row-count row-height edges % pull-expr selected-object]
  (let [!selected-index (e/server (atom nil))
        selected-index (e/server (e/watch !selected-index))]
    (->> (forms/TablePicker! ::selection
           selected-index
           row-count
           (e/fn [row-index]
             (e/server
               ;; (prn+ 'will-navd row-index (meta %))
               (let [navd (nav-row e/*bindings* % row-index)] ; FIXME glitch guard, ensures index travels with row
                 (e/Reconcile
                   (when (some? navd)
                     (let [[row-index row-object row-hydrated-object] navd]
                       (select-row-if-match-url! !selected-index row-index row-hydrated-object selected-object)
                       (RenderSeqableRow row-hydrated-object edges)))))))
           :row-height row-height
           :column-count (e/server (count (hfql1/form pull-expr)))
           :as :tbody)
      (forms/Parse (e/fn ToCommand [{selected-index ::selection}]
                     (e/server
                       (reset! !selected-index selected-index)
                       (let [selected-object (Nav % nil (nth % selected-index nil))] ; FIXME
                         (prn+ 'selection selected-index selected-object (hfql1/identify selected-object))
                         (if-let [symbolic-representation (hfql1/identify selected-object)]
                           [`Select! symbolic-representation (e/server (e/fn [] selected-object))]
                           [`Noop])))))
      (forms/Interpreter {`Noop (e/fn [] [::forms/ok])}))))

(e/defn RenderSeqableHeader [edge !sort-spec]
  (e/server
    (dom/thead
      (dom/tr
        (e/for [edge (e/server (e/diff-by {} (hfql1/value edge)))]
          (let [form (e/server (hfql1/form edge))
                label (e/server (labelize edge))]
            (dom/th
              (dom/props {:title (str (e/server (::hfql1/resolved-form (meta edge))))})
              #_(e/server (RenderTooltip ::hfql1/ColumnHeaderTooltip edge))
              (dom/On "click" #(swap! !sort-spec toggle-column-sort form) nil)
              (dom/text label))))))))

(defn- format-query-meta [query-meta]
  (some-> query-meta not-empty dustingetz.str/pprint-str))

#?(:clj (defn- table-title-tooltip-content [query query-meta]
          {:post [(or (string? %) (nil? %))]}
          (-> (find-documentation query)
            (str (format-query-meta query-meta))
            (str/trim)
            (dustingetz.str/blank->nil))))

(e/defn RenderSeqableTitle [traversal-edge pull-edge row-count Search metadata Suggest*]
  (dom/legend
    (dom/span
      (dom/span
        (dom/props {:class "title" :data-tooltip (e/server (table-title-tooltip-content (hfql1/form traversal-edge) metadata))})
        (dom/text (e/server (hfql1/short-form traversal-edge))))
      (e/as-vec ; dirty trick to circumvent dom/text in between Search and columns
        (e/amb
          (dom/span ; dirty trick to circumvent sited destructuring out of RenderSeqableTitle
            (e/client (let [node dom/node] (e/fn [] (binding [dom/node node] (Search false #_(>= row-count COLLECTION-LIMIT)))))))
          (dom/text (format-collection-count row-count COLLECTION-LIMIT))
          (let [keys (e/server (hfql1/value (hfql1/filter-pull (complement hfql1/star?) pull-edge)))
                resolved-keys (e/server (into #{} (map hfql1/resolved-form keys)))
                pre-checked (#(or (hfql1/pull-has-star? pull-edge) (nil? pull-edge)))
                new-suggest* (hfql1/filter-pull (fn [edge] (not (resolved-keys (hfql1/resolved-form edge))))
                               (Suggest*))]
            ;; (e/server (prn+ {'keys keys, 'pull-edge pull-edge 'new-suggests new-suggest*}))
            (dom/span
              (e/server
                (let [from-spec (e/as-vec (e/for [edge (e/diff-by {} keys)]
                                            (e/When (forms/Checkbox* true :label (e/server (labelize edge)) :title (e/server (hfql1/resolved-form edge)))
                                              edge)))
                      selected (e/as-vec (e/for [edge (e/diff-by {} (hfql1/value new-suggest*))]
                                           (e/When (forms/Checkbox* pre-checked :label (e/server (labelize edge)) :title (e/server (hfql1/resolved-form edge)))
                                             edge)))]
                  (hfql1/build-pull (into from-spec selected)))))))))))

#?(:clj (defn- eager-pull-search-sort-BIG-O-N-SQUARE [collection edge hfql-bindings search sort-spec]
          #_(prn `eager-pull-search-sort-BIG-O-N-SQUARE edge hfql-bindings search sort-spec)
          (try
            (debug-exceptions `eager-pull-search-sort-BIG-O-N-SQUARE
              #_(Thread/sleep 3000)
              (let [metadata (meta collection)
                    collection (with-meta (vec collection) metadata)         ; fix if collection is e.g. a set
                    ;; navd (with-bindings hfql-bindings (into [] (map #(clojure.datafy/nav collection nil %)) collection))
                    pulled (run-with-bindings hfql-bindings (fn [] (mapv #(hfql1/nav (hfql1/nav collection nil %) edge) collection))) ; TODO turn into a transducer
                    filtered (eduction (map-indexed vector)
                               (if (not-empty search)
                                 (filter #(strx/any-matches? (vals (second %)) search))
                                 (map identity))
                               pulled)
                    sorted (vec (if-some [sorter (and (not-empty sort-spec) (->sort-comparator sort-spec))]
                                  (try (sort-by second sorter filtered) (catch Throwable _ filtered))
                                  filtered))]
                (with-meta (into [] (comp (map first) (map #(nth collection %))) sorted)
                  metadata)))
            (catch Throwable t (prn (ex-message t)) []))))

(e/defn RenderSeqable [traversal-edge pull-edge % effect-handlers Search route-state]
  (e/server (prn+ 'RenderSeqable pull-edge (meta %)))
  (e/server
    (let [% (#(if (seqable? %) % ()) %)] ; when guard on switch from sequable to associative
      (e/client
        (let [{saved-selection ::selection} route-state
              !selected-object              (e/server (atom nil))
              selected-object               (e/server (e/watch !selected-object))
              !row-count                    (atom 0), row-count (e/watch !row-count)]
          (dom/fieldset
            (dom/props {:class "entity-children hyperfiddle-navigator5__block"})
            (let [free-args        (e/server
                                     (RenderSeqableTitle traversal-edge pull-edge row-count Search (dissoc (meta %) `clojure.core.protocols/nav)
                                       (e/fn []
                                         (let [first-item (e/Snapshot (first %))] ; snapshot so we don't re-infer if query shrinks to 0. Also prevents nasty crash
                                           (Timing 'infer-columns (partial #(hfql1/suggest (hfql1/nav % nil first-item)) %))))))
                  search-cmd       (let [Search (e/server (first free-args))]
                                     (e/Reconcile (when Search
                                                    (e/call Search)))) ; fighting against sited destructuring
                  edge+suggestions (e/server (second free-args))
                  !sort-spec       (atom [[(e/server (hfql1/form (first (hfql1/value edge+suggestions)))) true]]),
                  sort-spec        (filterv first (e/watch !sort-spec))
                  row-height       24
                  cols             (e/server (e/diff-by {} (hfql1/value edge+suggestions)))
                  column-count     (e/server (hfql1/count edge+suggestions))
                  data-count       (e/server (bounded-count COLLECTION-LIMIT %)) ; O(n)
                  data             (e/server (e/Reconcile (if (< data-count COLLECTION-LIMIT)
                                                            (eager-pull-search-sort-BIG-O-N-SQUARE % edge+suggestions e/*bindings* *search sort-spec)
                                                            (with-meta (into [] (take COLLECTION-LIMIT) %) (meta %))))) ; O(n)
                  selected-object  (e/server (#(or selected-object (reset! !selected-object (resolve-selected-object-BIG-O-N saved-selection e/*bindings* data)))))
                  data-count       (e/server (bounded-count COLLECTION-LIMIT data)) ; O(n)
                  ]
              ;; (e/server (prn+ 'free-args free-args))
              (reset! !row-count data-count)
              search-cmd                    ; force order
              (dom/table
                (dom/props {:style {:--row-height (str row-height "px"), :--column-count column-count}})
                (RenderSeqableHeader edge+suggestions !sort-spec)
                (forms/Interpreter effect-handlers
                  (e/amb
                    (e/When search-cmd search-cmd)
                    (binding [*block-opts (e/server (meta pull-edge) )] ; TODO remove
                      (->> (RenderSeqableBody row-count row-height cols data edge+suggestions selected-object)
                        (forms/Parse (e/fn [[command & args]]
                                       (e/Reconcile
                                         (case command
                                           `Select! (e/server
                                                      (let [[symbolic-selected-object Get-actual-selected-object] args
                                                            selected-object                                       (Get-actual-selected-object)]
                                                        (reset! !selected-object selected-object)
                                                        [`Select! symbolic-selected-object]))
                                           command)))))))))))
          (e/server (AutoNavigate pull-edge selected-object)))))))

(e/defn RoundTrip [v] (identity (e/server (identity v)))) ; Awesome debug tool! Forces a roundtrip to diagnose how many roundtrips it takes to ack a value!
;; (RoundTrip (RoundTrip :foo)) ; two roundtrips

;;;;;;;;;;;;;;;;
;; Entrypoint ;;
;;;;;;;;;;;;;;;;

(defn- normalize-router-query [router-query]
  (cond
    (vector? router-query) (seq router-query)
    :else router-query))

(e/defn HfqlRoot [sitemap entrypoints]
  (e/server
    (binding [e/Offload (e/fn Offload ([f!] (f!)) ([f! _executor] (f!)))]
      (e/client
        (dom/style (dom/text css tooltip/css))
        (Index sitemap entrypoints) ; FIXME this index generation is generic and may conflict with user-custom index (e.g. ObjectBrowser3 fiddle demo renders two navs)
        (tooltip/TooltipArea
          (e/fn []
            (tooltip/Tooltip)
            (binding [*server-pretty (e/server (#(or *server-pretty {})))
                      *sitemap sitemap]
              (binding [*depth 0]
                (let [[query] router/route
                      query (normalize-router-query query)]
                  (dom/div
                    (dom/props {:class "Browser"})
                    #_(QueryMonitor)
                    (if-not query
                      (router/ReplaceState! ['. [(first entrypoints)]])
                      (let [nom (hfql1/edge-name query)]
                        (set! (.-title js/document) (str (some-> nom name (str " – ")) "Hyperfiddle"))
                        (dom/props {:class (cssx/css-slugify nom)})
                        (rebooting query
                          (Navigate query))))))))))))))

;;;;;;;;;;;;
;; Extras ;;
;;;;;;;;;;;;

(def table-block-css
"
.hyperfiddle-navigator5__block table { display: grid; grid-template-columns: repeat(var(--column-count), 1fr);  grid-template-rows: var(--row-height);}

.hyperfiddle-navigator5__block table thead { display: contents; }
.hyperfiddle-navigator5__block table thead tr { display: grid; grid-row: 1; grid-column: 1 / -1; grid-template-columns: subgrid;}
.hyperfiddle-navigator5__block table thead tr th { white-space: nowrap; text-overflow: ellipsis; overflow: hidden; }

.hyperfiddle-navigator5__block .hyperfiddle-electric-forms5__table-picker { grid-row: 2; grid-column: 1 / -1; grid-template-columns: subgrid; }

"
  )

(def css
  (str forms/css
    loader/css
    table-block-css

    "
/* cosmetic defaults */
.hyperfiddle-navigator5__block legend .title {font-weight:600;}
.hyperfiddle-navigator5__block { padding: 0; background-color: white; }
.hyperfiddle-navigator5__block table { border: 0px solid #f2f2f2; border-top-left-radius: 0rem; border-top-right-radius: 0rem; }
.hyperfiddle-navigator5__block table thead tr { background-color: #f2f2f2; border-bottom: 1px lightgray solid; }
.hyperfiddle-navigator5__block table thead tr th { white-space: nowrap; text-overflow: ellipsis; overflow: hidden; }
.hyperfiddle-navigator5__block table thead tr th { font-weight: 500; }
.hyperfiddle-navigator5__block table thead tr th:not(:first-child) { border-left: 1px lightgray solid; }
.hyperfiddle-navigator5__block table :is(th, td) { padding: 0 0.25em; }
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

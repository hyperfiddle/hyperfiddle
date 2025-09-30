(ns hyperfiddle.navigator6
  {:clj-kondo/config {:lint-as '{hyperfiddle.electric3/defn clojure.core/defn}}}
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-forms5 :as forms]
            [hyperfiddle.hfql2 :as hfql]
            [hyperfiddle.entrypoint2 :refer [Index]]
            [hyperfiddle.router5 :as router]
            [hyperfiddle.electric-dom3 :as dom]

            [hyperfiddle.ui.tooltip :as tooltip]
            [contrib.css :as cssx]
            [contrib.data :refer [#?(:clj array?)]]
            [dustingetz.str :as strx]
            [clojure.string :as str]
            [clojure.pprint]
            [dustingetz.loader :as loader]
            [geoffreygaillard.predicates :as pred]
            #?(:clj [clojure.tools.logging :as log])
            [geoffreygaillard.debug-tools :refer [debug-exceptions]]
            [hyperfiddle.hfql1.functor :refer [fmap]]
            [hyperfiddle.hfql1.bifunctor :refer [pair fst snd swap]]
            [hyperfiddle.hfql1.monoid :refer [mappend]]
            [hyperfiddle.hfql1.monad :refer [join]]
            [hyperfiddle.hfql1.comonad :refer [extract]]
            [hyperfiddle.hfql1.cofree :refer [unwrap]]
            [hyperfiddle.hfql1.coalgebra :as coalgebra :refer [projection-type pull hfqlT seed #_pull-key]]
            [hyperfiddle.hfql1.analyzer :as ana]

            )
  #?(:cljs (:require-macros hyperfiddle.navigator6)))


(defmacro rebooting [sym & body] `(e/for [~sym (e/diff-by identity (e/as-vec ~sym))] ~@body))
(defmacro in [dom-node & body] `(binding [dom/node ~dom-node] ~@body))

(defmacro idef [sym val] `(do (def ~sym ~val) ~val))

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
    (or (::hfql/label (meta edge))
      #_(hfql/short-form edge) ; accounts for form
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
          #_(when-let [identifier (hfql/edge-name edge)]
            (str/triml
              (str (documentation
                     (cond (qualified-symbol? identifier) (resolve identifier) ; FIXME violates HFQL's rule ② – don't resolve
                           ;; TODO extend ; FIXME no don't extend, instead have HFQL provide all info ahead of time (e.g. doc, javadoc)
                           ;; (pred/java-method-symbol? identifier) ...
                           :else nil))
                "\n\n")))))

(defn as-coll [v] (if (or (array? v) (coll? v)) v [v])) ; TODO removeme, glitch guard

(e/defn RenderInlineColl [object edge value]
  (let [pretty-v (str-inline-coll (as-coll value) *server-pretty)
        symbolic-link-href (::hfql/link (meta edge))]
    (e/Reconcile
      (if (some? symbolic-link-href)
        (router/link ['. [symbolic-link-href]] (dom/text pretty-v))
        (dom/text pretty-v))) ))

(e/defn Resolve [fq-sym fallback] (get e/*exports* fq-sym fallback))

(e/defn RenderTooltip [tooltip-ident object edgeT value]
  (let [tooltip-ident (get (hfql/metas edgeT) tooltip-ident (get *block-opts tooltip-ident))
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
                denv {} #_{'% (hfql/identify object), (hfql/form edgeT) value, '%v (#(or (hfql/identify value) value))}
                symbolic-link-href (ana/unquote* (::hfql/link (hfql/metas edgeT)))]
            (e/Reconcile
              (if (some? symbolic-link-href)
                (router/link ['. [(replace denv symbolic-link-href)]] (dom/text pretty-v))
                (dom/text pretty-v))) )))))

(e/declare Render)
(e/defn RenderCell [object edgeT value]
  (dom/td                            ; custom renderer runs in context of a cell
    (dom/props {:data-cell (e/fn [] edgeT)})
    (binding [Render DefaultCellRenderer]
      (e/Reconcile (e/call (Resolve (ana/unquote* (::hfql/Render (hfql/metas edgeT))) ; FIXME syntax wart
                             Render) object edgeT #_(vary-meta edge dissoc ::hfql/Render) value)))))

(defn glitch-guard
  ([f] (glitch-guard some? f))
  ([pred f]
   (fn [a & as]
     (when (pred a)
       (apply f a as)))))

(defn- get-leafT [stepT] ; TODO questionable logic
  ;; Idea here is to display "leaf values" in the following case:
  ;; [{fs/jfile-kind name}] => "dir" instead of `::fs/dir`.
  (case (::ana/multiplicity (hfql/metas stepT))
    ::ana/many stepT
    (or (first (unwrap stepT)) stepT)))

(e/defn Render [object edge value] (RenderCell object edge value))
(e/defn Render* [stepT]
  (Render ((glitch-guard hfql/scope) stepT)
    stepT
    ((glitch-guard hfql/nav) ((glitch-guard get-leafT) stepT))))

;;;;;;;;;;;;;;;;;;;
;; Edit Handlers ;;
;;;;;;;;;;;;;;;;;;;

(e/defn HandleSearch! [s]
  ;; `update` doesn't work, route is a lazyseq (?)
  (router/ReplaceState! ['. (into (conj (into [] (take *depth) router/route)
                                    (assoc (nth router/route *depth {}) ::search s))
                              (drop (inc *depth) router/route))])
  [::forms/ok])

(e/defn HandleSelect! [Getter]
  ;; Want the selected row to transition atomically? : (RoundTrip (RoundTrip [::forms/ok]))
  ;; Two roundtrids because:
  ;;  - one roundtrip because TablePicker! receives the selected index from server (latency)
  ;;  - second roundtrip because of an Electric bug leo is fixing
  ;;     - Electric produces an extra roundtrip on e/fn call.
  (case (router/Navigate! ['. (conj (into [] (take *depth) router/route)
                                (assoc (nth router/route *depth {}) ::selection
                                  (e/server
                                    (let [stepT (Getter)]
                                      ;; (def _stepT_navigate stepT)
                                      ;; (hfql/symbolic-edge _stepT_navigate)
                                      (list (hfql/identify (hfql/value-at-point stepT)) (hfql/symbolic-edge stepT))))))])
    [::forms/ok]))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependency Injection ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(e/defn Searcher [saved disabled?]
  (e/client
    (->> (forms/Input! ::search saved :type :search :maxlength 100 :disabled disabled? :style {:grid-area "search"})
      (forms/Parse (e/fn [{search ::search}] [`Search! search])))))

(e/declare Search)

(defn- state-stored-in-route [router-route *depth] (nth router-route *depth {}))

(e/defn InjectDependencies [Continuation]
  (e/client
    (binding [*depth (inc *depth)]
      (let [route-state (state-stored-in-route router/route *depth)
            {saved-search ::search} route-state]
        (binding [*search saved-search
                  Search (e/Partial Searcher saved-search)
                  e/*bindings* (e/server (assoc e/*bindings* (find-var `*search) *search))]
          (forms/Interpreter {`Search! HandleSearch!
                              `Select! HandleSelect!}
            (Continuation route-state)))))))

(e/defn Constantly [x] (e/fn [& _] x))

(e/defn RenderMeta [stepT]
  (e/server
    (when-some [m (hfql/metas stepT)]
      (dom/details
        (dom/props {:open true})
        (dom/summary (dom/text "Meta"))
        (dom/pre (dom/text (strx/pprint-str m)))))))

(e/defn RenderValue [hfql & _]

  (e/server
    (RenderMeta hfql)
    (dom/pre (dom/text (strx/pprint-str (snd (extract hfql)))))))

(e/defn RenderFunction [hfql & _]
  (e/server
    (def _hfql hfql)
    (RenderMeta hfql)
    (dom/details
      (dom/props {:open true})
      (dom/summary (dom/text (pr-str (list (::coalgebra/key (meta hfql) '...)))))
      (dom/pre (dom/text (strx/pprint-str (extract (pull hfql))))))))

(comment
  (def _param (::coalgebra/param (meta _hfql)))
  (deref (first (hyperfiddle.hfql1.monad-reader/ask (:step _param))))
  )

(defn render-type [hfql]
  (let [metas (hfql/metas hfql)]
    (if (= ::ana/many (::ana/multiplicity metas))
      ::star
      (case (::ana/type metas) #_(projection-type hfql)
        ::ana/star ::star
        ::ana/pull ::pull
        ::ana/traversal (render-type (first (unwrap hfql)))
        ::ana/ifn ::ifn
        ::unknown))))


(e/declare RenderStepVertical RenderStepHorizontal)

(e/defn ResolveRenderer [hfql]
  (e/server
    (get {::pull RenderStepHorizontal
          ::star RenderStepVertical
          ::ifn RenderFunction}
      (render-type hfql)
      RenderValue)


    #_(if (hfql/star-traversal? hfql)
      [RenderStepVertical (hfql/nav object edge)]
      (list
        (get {:object RenderOne
              :collection RenderStepVertical
              :set RenderStepVertical}
          (infer-block-type object)
          (Constantly nil))
        object)) ; HACK glitch guard: Ensures object and its corresponding Renderer update at the same time
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Components ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

#?(:clj
   (defn- sort-children-by-ordered-keyset
     "Sort a map structure by keys. Static selected keys (from stepT) have
  priority and are laid out in order, as in an ordered map. Extra keys comes later and are sorted by
  `comparator`. Return a seq of map entries."
     [stepT childrenT]
     {:pre [(= ::ana/pull (::ana/type (hfql/metas stepT))) (every? hfql/symbolic-edge childrenT)]
      :post [(every? hfql/symbolic-edge childrenT) #_(= (count %) (count childrenT))]}
     (let [static-keys (distinct (map hfql/symbolic-edge childrenT))
           static-keyset (set static-keys)
           [static-children dynamic-children] (contrib.data/group-by-pred (comp boolean static-keyset hfql/symbolic-edge) childrenT)
           static-map (into {} (map (juxt hfql/symbolic-edge identity)) static-children)]
       #_(prn {'static-keys static-keys
             'static-kvs static-children
             'dynamic-kvs dynamic-children})
       (mappend (keep (fn [pullspec-key] (get static-map pullspec-key)) static-keys)
         (?sort-by hfql/symbolic-edge (remove (comp some? static-keyset hfql/symbolic-edge) dynamic-children))))))

(defn- select-pull-step [stepT]
  (case (::ana/type (hfql/metas stepT))
    ::ana/pull stepT
    ::ana/traversal (some-> (unwrap stepT) first select-pull-step)
    ;; (some-> (unwrap stepT) first select-pull-step)
    (hfql/wrap-in-pull stepT)
    #_(throw (ex-info "No pull child" {:type (::ana/type (hfql/metas stepT))}))))

(defn- resolve-selected-object-BIG-O-N [[row column] bindings childrenT]
  #?(:clj
     (debug-exceptions `resolve-selected-object
       (with-bindings bindings
         (if (= ::ana/many (::ana/multiplicity (hfql/metas childrenT)))
           (hfql/find-child-by #(= row (hfql/identify (hfql/value-at-point %))) childrenT)
           (hfql/find-child column (select-pull-step childrenT)))))))

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

#?(:clj
   (defn- select-row-if-match-url! [!index row-index row-object selected-object]
     {:pre [(pred/atom-like? !index) (number? row-index)]
      :post [(nil? %)]}
     (if (= row-object selected-object) ; both can be nil
       (reset! !index row-index) ; current row matches. Set selection.
       (compare-and-set! !index row-index nil)) ; current row used to match and now doesn't. Clear selection.
     nil))

(declare caching-nth)

(defn- format-query-meta [query-meta] (some-> query-meta not-empty dustingetz.str/pprint-str))

#?(:clj (defn- table-title-tooltip-content [query query-meta]
          {:post [(or (string? %) (nil? %))]}
          (-> (find-documentation query)
            (str (format-query-meta query-meta))
            (str/trim)
            (dustingetz.str/blank->nil))))

(e/defn ColumnPicker [stepT]
  (e/server
    (def _column_stepT stepT)
    (comment
      (unwrap _column_stepT))
    (when-let [pull-step (select-pull-step stepT)]
      (let [stepT+ (hfql/augment-with-suggestions! pull-step)
            childrenT (remove (comp ::ana/recursive-child hfql/metas) (unwrap stepT+))
            static-columns (remove (comp ::hfql/suggested hfql/metas) childrenT)
            suggested-columns (filter (comp ::hfql/suggested hfql/metas) childrenT)
            pre-checked false]
        (let [from-spec (e/as-vec (e/for [edgeT (e/diff-by {} static-columns)]
                                    (e/When
                                        (dom/span
                                          (forms/Checkbox* true :label (e/server (str (hfql/symbolic-edge edgeT)))
                                            :title (e/server (str (hfql/symbolic-edge edgeT)))))
                                      edgeT)))
              selected (e/as-vec (e/for [edgeT (e/diff-by {} suggested-columns)]
                                   (e/When
                                       (dom/span
                                         (forms/Checkbox* pre-checked :label (e/server (str (hfql/symbolic-edge edgeT)))
                                           :title (e/server (str (hfql/symbolic-edge edgeT)))))
                                     edgeT)))]
          (mappend from-spec selected))))))

(defmulti search-matches (fn [object _needle] (type object)))
(defmethod search-matches :default [object needle] (strx/any-matches? [object] needle))
(defmethod search-matches ::hfql/alist [object needle]
  (let [search-on (or (seq (map val (::hfql/alist (meta object))))
                    (vals object))]
    (strx/any-matches? search-on needle)))

(defn- prefix-filterer
  "Take a filtering predicate pred :: String -> Any -> Bool and return a function f :: String -> [Any] -> [Any].

  The returned function `f` will filter the given collection like regular
  clojure.core/filter. However, given we know the filter criteria is a String,
  we leverage the fact that if the previous search happened on a prefix of the
  current search (e.g. \"hell\" ⊏ \"hello\"), then we only filter the last result
  instead of the entire original resultset.
  E.g.:
   - (f \"\" xs)   : nothing to search on, return xs – 𝓞(1)
   - (f \"h\" xs)  : filter xs looking for \"h\" – 𝓞(n)
   - (f \"he\" xs) : \"h\"  ⊏ \"he\" ⇒ filter last result already filtered by \"h\" – 𝓞(∆n).
   - (f \"he\" xs) : \"he\" ⊑ \"he\" ⇒ return already filtered result – 𝓞(1).
   - (f \"h\" xs)  : \"h\"  ⋢ \"he\" ⇒ discard last result and filter xs looking for \"h\" – 𝓞(n).

  Therefore:
  - \"h\" ⟶ \"he\" ⟶ \"hel\" is 𝓞(n) ⟶ 𝓞(∆n) ⟶ 𝓞(∆∆n)
  - \"hel\" ⟶ \"he\" ⟶ \"h\" is 𝓞(n) ⟶ 𝓞(n) ⟶ 𝓞(n)

  "
  [pred]
  (let [!last-needle (volatile! "")
        !last-filtered (volatile! ())]
    (fn [needle pulled]
      (let [last-needle @!last-needle
            do-filter (partial pred needle)]
        (vreset! !last-needle needle)
        (cond
          (empty? needle) (vreset! !last-filtered pulled)
          (and (not (str/blank? last-needle))
            (str/starts-with? needle last-needle)) (vreset! !last-filtered (filter do-filter @!last-filtered))
          (and (not (str/blank? last-needle))
            (= needle last-needle)) @!last-filtered
          :else (vreset! !last-filtered (filter do-filter pulled)))))))

#?(:clj
   (defn- eager-pull-search-sort-BIG-O-N-SQUARE []
     ;; (prn `eager-pull-search-sort-BIG-O-N-SQUARE edge hfql-bindings search sort-spec)
     (let [do-filter (prefix-filterer (fn [needle pair] (search-matches (snd pair) needle)))]
       (fn [columnsT rowsT hfql-bindings search sort-spec]
         [columnsT
          ;; (def _args [rowsT columnsT hfql-bindings search sort-spec])
          (try
            (debug-exceptions `eager-pull-search-sort-BIG-O-N-SQUARE
              ;; (Thread/sleep 3000)
              (if (not-empty search)
                (dedupe
                  (for [rowT rowsT
                        columnT columnsT
                        :let [cell-value (hfql/nav (hfql/seed (hfql/scope rowT) columnT))]
                        :when (search-matches cell-value search)]
                    rowT))
                rowsT)
              #_(let [pulled (sequence
                               (eduction
                                 (map (join pair)) ; duplicate arg and apply both to ->Pair
                                 (map (fmap pull)) ; pull right side of pair
                                 (map (fmap extract)) ; extract right side of pair
                                 rowsT))
                      filtered (if (not-empty search)
                                 (do-filter search pulled)
                                 pulled)
                      sorted (if-some [sorter (and (not-empty sort-spec) (->sort-comparator sort-spec))]
                               (try (sort-by snd sorter filtered) (catch Throwable _ filtered))
                               filtered)]
                  (with-meta (map fst sorted) (meta (extract rowsT)))))
            (catch Throwable t (prn 'eager-pull-search-sort (ex-message t) #_t) []))]))))

(defn caching-nth
  "Returns f where (f i) ≈ (nth coll i) but caches prefix in a vector.
   After calling (f i):
   - any call to (f j) where j ≤ i will be 𝓞(1).
   - any call to (f j) where j > i will be O(∆(i, j)), which is still 𝓞(n),
     but on streamy collections the base case is j = i + 1, which will be 𝓞(1).
   Keeps only realized prefix + current tail. Same memory behavior as nth (i.e. will OOM accessing large N)."
  ([coll] (caching-nth coll ::not-found))
  ([coll not-found]
   (let [!v (volatile! [])            ; cached prefix as a vector
         !s (volatile! (seq coll))]   ; current tail
     (fn f
       ([i] (let [x (f i not-found)]
              (if (identical? x ::not-found)
                (throw #?(:clj (IndexOutOfBoundsException. (str i)), :cljs (js/Error. "Index out of bounds")))
                x)))
       ([i nf]
        (when (neg? i) (throw #?(:clj (IndexOutOfBoundsException. (str i)), :cljs (js/Error. "Index out of bounds"))))
        ;; ensure cache has (inc i) items
        (let [need (- (inc i) (count @!v))]
          (when (pos? need)
            (let [s0 @!s]
              (loop [n need, s s0, tv (transient @!v)]
                (if (zero? n)
                  (do (vreset! !v (persistent! tv))
                      (vreset! !s s))
                  (if (nil? s)
                    (do (vreset! !v (persistent! tv))
                        (vreset! !s nil))
                    (recur (dec n) (next s) (conj! tv (first s))))))))
        (let [v @!v]
          (if (< i (count v)) (nth v i) nf))))))))

(defn- seed-cell [rowT columnT]
  (if (= ::ana/pull (::ana/type (hfql/metas columnT)))
    rowT
    (hfql/seed (hfql/scope rowT) columnT)))

(e/defn RenderVerticalMatrixBody [row-height columnsT rowsT selected-rowT]
  (e/client
    (let [|columnsT> (e/server (e/diff-by {} columnsT))
          fast-nth (e/server (caching-nth rowsT))]
      (let [!selected-index (e/server (atom nil))
            selected-index (e/server (e/watch !selected-index))]
        (->> (forms/TablePicker! ::selection selected-index (e/server (count rowsT))
               (e/fn [row-index]
                 (e/server
                   (let [rowT (fast-nth row-index nil)]
                     (e/Reconcile
                       (when (some? rowT)
                         (select-row-if-match-url! !selected-index row-index rowT selected-rowT) ; TODO rename / clarify
                         (e/for [columnT |columnsT>] ; row × column cartesian product
                           (Render* ((glitch-guard seed-cell) rowT columnT))))))))
               :row-height row-height
               :column-count (e/server (max 1 (count columnsT)))
               :as :tbody)
          (forms/Parse (e/fn ToCommand [{selected-index ::selection}]
                         (e/server
                           (reset! !selected-index selected-index)
                           (e/Reconcile
                             (if-let [selected-rowT (fast-nth selected-index nil)]
                               [`Select! (e/server (e/fn [] (def _selected-rowT selected-rowT)
                                                     (comment
                                                       (unwrap _selected-rowT))
                                                     selected-rowT))]
                               [`Noop])))))
          (forms/Interpreter {`Noop (e/fn [] [::forms/ok])}))))))

(e/defn RenderVerticalMatrix
  ;; Even though a matrix is transposable, the DOM makes it hard to transpose a table. So we split vertical from horizontal rendering for our own sanity. G didn't find a robust CSS-only solution.
  [row-height columnsT rowsT selected-rowT]
  (e/client
    (dom/table
      (dom/props {:style {:--row-height (str row-height "px"),
                          :--column-count (e/server (max 1 (count columnsT)))
                          :--row-count (e/server (max 1 (count rowsT)))}})
      (e/server
        (dom/thead
          (dom/tr
            (e/server (def _columnsT columnsT) nil)
            (e/for [columnT (e/diff-by {} columnsT)]
              (let [form (str (hfql/symbolic-edge columnT))]
                (dom/th
                  (dom/props {:scope :col :title (str form)})
                  (RenderTooltip ::hfql/ColumnHeaderTooltip nil columnT nil)
                  ;; (e/client (dom/On "click" #(swap! !sort-spec toggle-column-sort form) nil))
                  (dom/text (str form))))))))
      (RenderVerticalMatrixBody row-height columnsT rowsT selected-rowT))))

(e/defn With-Search [pred rows columns Body]
  (e/client
    (let [search        *search
          rows-count    (e/server (bounded-count COLLECTION-LIMIT rows)) ; 𝓞(n)
          filtered (e/server (e/Reconcile (if (< rows-count COLLECTION-LIMIT)
                                                  (pred columns rows e/*bindings* search nil #_sort-spec)
                                                  [columns (with-meta (take COLLECTION-LIMIT rows) (meta rows))])))]
      (Body (e/server (first filtered)) (e/server (second filtered))))))

(e/defn Step-Title [stepT]
  (dom/span
    (dom/props {:class "title"
                :style {:order -3}
                :data-tooltip (e/server (table-title-tooltip-content ((glitch-guard hfql/symbolic-edge-computed) stepT)
                                          ((glitch-guard hfql/metas) stepT)))})
    (dom/text (e/server (pr-str ((glitch-guard hfql/symbolic-edge-computed) stepT))))))

(defn get-rows-container [stepT]
  (case (::ana/type (hfql/metas stepT))
    ::ana/traversal (if (= ::ana/many (::ana/multiplicity (hfql/metas stepT)))
                      stepT
                      (get-rows-container (first (unwrap stepT))))
    stepT))

(e/declare *selection)

(e/defn RenderStepVertical [stepT route-state]
  (dom/fieldset
    (dom/props {:class "entity-children hyperfiddle-navigator6__block"})
    (let [legend (dom/legend (dom/props {:style {:display :flex, :flex-wrap :wrap}})
                   (Step-Title stepT)
                   dom/node)
          columnsT (e/server (in legend (ColumnPicker stepT)))
          stepT (e/server (get-rows-container stepT))]
      (With-Search (e/server (eager-pull-search-sort-BIG-O-N-SQUARE)) (e/server (unwrap stepT)) ; rowsT
        columnsT
        (e/fn [_columnsT-filtered rowsT-filtered]
          (in legend
            (dom/span (dom/props {:style {:order -1}})
                      (dom/text (format-collection-count (e/server (bounded-count COLLECTION-LIMIT rowsT-filtered)) ; 𝓞(n)
                                  COLLECTION-LIMIT))))
          (e/amb
            (in legend (dom/span (dom/props {:style {:order -2}}) (Search false)))
            (RenderVerticalMatrix 24 columnsT rowsT-filtered *selection)))))))


(e/defn RenderHorizontalMatrixBody [row-height columnsT rowsT selected-columnT RenderHeaderCell]
  (e/client
    (let [|rowsT> (e/server (e/diff-by {} rowsT))
          fast-nth (e/server (caching-nth columnsT))]
      (let [!selected-index (e/server (atom nil))
            selected-index (e/server (e/watch !selected-index))]
        (dom/style (dom/text
                     "

.hyperfiddle-electric-forms5__table-picker.horizontal{
  overflow-x: auto;
  grid-template-columns: fit-content(25ch) repeat(var(--column-count), auto) !important;

  tr :is(th,td) {
    grid-row: auto;
    contain: layout;
    width: auto;
  }
  tr th{
    position: sticky;
    left: 0;
    z-index: 1;
    text-align: left;
  }
  tr:nth-child(even):not([aria-selected="true"], [aria-checked="true"]) th {
    background-color: #f2f2f2;
  }
  tr:nth-child(odd):not([aria-selected="true"], [aria-checked="true"]) th {
    background-color: white;
  }

}

:where(.hyperfiddle-electric-forms5__table-picker.horizontal tr:hover:has(*) :is(td, th)) { background-color: inherit; }
.hyperfiddle-electric-forms5__table-picker.horizontal tr:has(td:hover) :is(td:hover, th) { background-color: #ddd; }


"
                     ))
        (->> (forms/TablePicker! ::selection selected-index (e/server (count columnsT))
               (e/fn [column-index]
                 (e/server
                   (let [columnT (fast-nth column-index nil)]
                     (e/Reconcile
                       (when (some? columnT)
                         (dom/th (dom/props {:scope :row :style {:grid-column 1}}) (RenderHeaderCell columnT))
                         #_(select-row-if-match-url! !selected-index column-index columnT selected-columnT) ; TODO rename / clarify
                         (e/for [rowT |rowsT>] ; row × column cartesian product
                           (Render* ((glitch-guard seed-cell) columnT rowT))))))))
               :row-height row-height
               :column-count (e/server (max 1 (count rowsT)))
               #_#_:as :tbody
               :class "horizontal")
          (forms/Parse (e/fn ToCommand [{selected-index ::selection}]
                         (e/server
                           (reset! !selected-index selected-index)
                           (e/Reconcile
                             (if-let [selected-columnT (fast-nth selected-index nil)]
                               [`Select! (e/server (e/fn [] selected-columnT))]
                               [`Noop])))))
          (forms/Interpreter {`Noop (e/fn [] [::forms/ok])}))))))

(e/defn RenderHorizontalMatrix
  ;; Even though a matrix is transposable, the DOM makes it hard to transpose a table. So we split vertical from horizontal rendering for our own sanity. G didn't find a robust CSS-only solution.
  [row-height columnsT rowsT selected-rowT]
  (e/client
    (RenderHorizontalMatrixBody row-height columnsT rowsT selected-rowT
      (e/fn HeaderCell [columnT]
        (e/server
          (let [form (hfql/symbolic-edge columnT)]
            (dom/props {:title (str form)})
            (RenderTooltip ::hfql/ColumnHeaderTooltip nil columnT nil)
            (dom/text (str form))))))))

#?(:clj
   (defn- eager-search-kvs-BIG-O-N-SQUARE []
     (let [do-filter (prefix-filterer (fn [needle pair] (search-matches (snd pair) needle)))]
       (fn [columnsT rowsT hfql-bindings search sort-spec]
         (try
           (debug-exceptions `eager-search-kvs-BIG-O-N-SQUARE
             (if (not-empty search)
               (let [kvs (for [rowT rowsT
                               columnT columnsT
                               :let [cell-value (delay (hfql/nav (hfql/seed (hfql/scope rowT) columnT)))]
                               :when (or (search-matches (hfql/symbolic-edge columnT) search)
                                       (search-matches (force cell-value) search))]
                           [rowT columnT])]
                 [(distinct (map second kvs))
                  (dedupe (map first kvs))])
               [columnsT rowsT]))
           (catch Throwable t (prn 'eager-pull-search-sort (ex-message t) #_t)
                  [columnsT rowsT]))))))

(defn- select-horizontal-children [stepT] ; FIXME smell
  (case (::ana/type (hfql/metas stepT))
    ::ana/pull [stepT]
    ::ana/traversal (unwrap stepT)
    (throw (ex-info "Can't extract rows out of this hfql step" {:type (::ana/type (hfql/metas stepT))}))))

(e/defn RenderStepHorizontal [stepT route-state]
  (dom/fieldset
    (dom/props {:class "entity hyperfiddle-navigator6__block"})
    (let [legend (dom/legend (dom/props {:style {:display :grid, :grid-template-areas "'title search count'"}})
                             (Step-Title stepT)
                             dom/node)
          columnsT (e/server ((glitch-guard unwrap) (hfql/augment-with-suggestions! (select-pull-step stepT))))]
      (e/server (prn "numrows" (count (unwrap stepT))))
      (With-Search
        (e/server  (eager-search-kvs-BIG-O-N-SQUARE))
        (e/server ((glitch-guard select-horizontal-children) stepT)) ; rowsT – TODO review, this is a smell
        columnsT
        (e/fn [columnsT-filtered rowsT-filtered]
          (in legend
            (dom/span (dom/props {:style {:grid-area "count"}})
                      (dom/text (format-collection-count (e/server (bounded-count COLLECTION-LIMIT columnsT-filtered)) ; 𝓞(n)
                                  COLLECTION-LIMIT))))
          (e/amb
            (in legend (Search false))
            (RenderHorizontalMatrix 24 columnsT-filtered rowsT-filtered *selection)))))))


#_(e/defn RoundTrip [v] (identity (e/server (identity v)))) ; Awesome debug tool! Forces a roundtrip to diagnose how many roundtrips it takes to ack a value!
;; (RoundTrip (RoundTrip :foo)) ; two roundtrips

;;;;;;;;;;;;;;;;
;; Navigation ;;
;;;;;;;;;;;;;;;;

;; (defn- should-navigation-happen? [stepT]
;;   (let [object (hfql/nav stepT)] ; supposedly already cached given we click on it to navigate
;;     (or (sequential? object)
;;       (set? object)
;;       (array? object)
;;       (hfql/identify object)
;;       (some? (hfql/suggest object)))))

(defn- normalize-router-query [router-query]
  (cond
    (vector? router-query) (seq router-query)
    (ident? router-query) (list router-query)
    :else router-query))


(defn- select-eventual-recursion-point [stepT]
  (when stepT
    (if (::ana/recursion-point (hfql/metas stepT))
      (if-let [recursive-child (first (filter (comp ::ana/recursive-child hfql/metas) (unwrap stepT)))]
        (if (empty? (unwrap recursive-child))
          nil
          recursive-child)
        stepT)
      stepT)))

(declare resolve-route)
(defn- select-next-child [sitemap stepT]
  (when stepT
    (prn "hf/select: " (::hfql/select (hfql/metas stepT)))
    (or (some->> (resolve-route (::hfql/select (hfql/metas stepT)) sitemap)
          (hfql/seed (hfql/scope stepT)))
      (select-eventual-recursion-point stepT))))

(e/defn WithSelection [route-state resolve-fn Body]
  (e/client
    (let [{selection ::selection} route-state
          !selected (e/server (atom nil))
          selected  (e/server (e/watch !selected))
          selected  (e/server (#(or selected (reset! !selected (resolve-fn selection)))))]
      (->> (Body selected)
        (forms/Parse (e/fn [[op & args :as command]]
                       (e/Reconcile
                         (case op
                           `Select! (e/server (let [[Get-selected] args]
                                                (reset! !selected (Get-selected))
                                                (e/amb)))
                           (e/amb)))
                       command))))))

(e/defn Navigate [stepT]
  (e/server
    (rebooting stepT
      (e/Reconcile
        (when stepT
          (e/client
            (InjectDependencies
              (e/fn [route-state]
                (WithSelection route-state
                  (e/server #(resolve-selected-object-BIG-O-N % e/*bindings* (get-rows-container stepT)))
                  (e/fn [selected-stepT]
                    (e/amb
                      (let [Renderer (ResolveRenderer stepT)]
                        (e/server (prn {:type (render-type stepT)}))
                        (binding [*selection selected-stepT]
                          (Renderer stepT route-state)))
                      (e/server
                        (e/Reconcile
                          (when-let [next-child (select-next-child *sitemap selected-stepT)]
                            ;; Navigation prioritizes recursive children over regular children at point
                            (Navigate next-child)))
                        (e/amb)))))))))))))

;;;;;;;;;;;;;;;;
;; Entrypoint ;;
;;;;;;;;;;;;;;;;

#?(:clj
   (defn- resolve-route [route sitemap]
     (let [[f & args :as query] (normalize-router-query route)]
       (prn "route is " query)
       (when-some [stepT (get sitemap f)]
         (hfql/seed {'% (hfql/resolve (first args))} ; TODO move to ednish
           stepT)))))

(e/defn HfqlRoot [sitemap entrypoints]
  (e/client
    (dom/style (dom/text css tooltip/css))
    (Index sitemap entrypoints)
    (tooltip/TooltipArea
      (e/fn []
        (tooltip/Tooltip)
        (binding [*server-pretty (e/server (#(or *server-pretty {})))
                  *sitemap sitemap]
          (binding [*depth 0]
            (dom/div
              (dom/props {:class "Browser"})
              (let [[query] router/route]
                (if-not query
                  (router/ReplaceState! ['. [(first entrypoints)]])
                  (do
                    (let [[f & _args] (normalize-router-query query)]
                      (set! (.-title js/document)  (str (some-> f name (str " – ")) "Hyperfiddle"))
                      (dom/props {:class (cssx/css-slugify f)}))
                    (rebooting query
                      (Navigate (e/server (idef stepT (resolve-route query sitemap)))))))))))))))

;;;;;;;;;;;;
;; Extras ;;
;;;;;;;;;;;;

(def table-block-css
"
.hyperfiddle-navigator6__block table { display: grid; grid-template-columns: repeat(var(--column-count), 1fr);  grid-template-rows: var(--row-height);}

.hyperfiddle-navigator6__block table thead { display: contents; }
.hyperfiddle-navigator6__block table thead tr { display: grid; grid-row: 1; grid-column: 1 / -1; grid-template-columns: subgrid;}
.hyperfiddle-navigator6__block table thead tr th { white-space: nowrap; text-overflow: ellipsis; overflow: hidden; }

.hyperfiddle-navigator6__block .hyperfiddle-electric-forms5__table-picker { grid-column: 1 / -1; grid-template-columns: subgrid; }
.hyperfiddle-navigator6__block thead + .hyperfiddle-electric-forms5__table-picker { grid-row: 2; grid-column: 1 / -1; grid-template-columns: subgrid; }

"
  )

(def css
  (str forms/css
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

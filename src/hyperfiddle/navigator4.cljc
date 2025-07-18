(ns hyperfiddle.navigator4
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-forms5 :as forms]
            [hyperfiddle.hfql0 #?(:clj :as :cljs :as-alias) hfql]
            [hyperfiddle.entrypoint :refer [Index]]
            [hyperfiddle.router4 :as router]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.incseq :as i]

            [hyperfiddle.ui.tooltip :as tooltip]
            [hyperfiddle.token-zoo0 :as tok] ; replace with e/Token + (always t)
            [contrib.css :as cssx]
            [contrib.assert :as ca]
            [contrib.data :as datax]
            [contrib.debug :as dbg]
            [dustingetz.str :as strx]
            [missionary.core :as m]
            [clojure.datafy :refer [nav]]
            [clojure.string :as str]
            [clojure.pprint]
            [clojure.walk :as walk]
            [dustingetz.loader :as loader]
            [geoffreygaillard.predicates :as pred]
            #?(:clj [clojure.tools.logging :as log])
            [geoffreygaillard.debug-tools :refer [debug-exceptions]])
  #?(:cljs (:require-macros hyperfiddle.navigator4)))

(defmacro rebooting [sym & body] `(e/for [~sym (e/diff-by identity (e/as-vec ~sym))] ~@body))

(e/declare *mode !mode *update *sitemap-writer *sitemap *page-defaults *block-opts *depth *server-pretty)
(e/declare ^:dynamic *search)
(declare css)

(defmulti comparable type)
(defmethod comparable :default [x] x)
#?(:clj (defmethod comparable java.lang.Class [^Class x] (.getName x)))
(defmethod comparable #?(:clj clojure.lang.Symbol :cljs cljs.core.Symbol) [sym] (str sym))
(defmethod comparable #?(:clj clojure.lang.Keyword :cljs cljs.core.Keyword) [kw] (str (namespace kw) "/" (name kw))) ; drop ":"

#?(:clj
   (defn ->sort-comparator [sort-spec]
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

(e/defn IDE-mode? [] (= *mode :ide))
(e/defn Browse-mode? [] (= *mode :browse))

#?(:clj (defn is-array? [x]
          {:post [(boolean? %)]}
          (boolean (some-> x class .isArray))))

#?(:clj
   (defn infer-block-type [x]
     (debug-exceptions `infer-block-type
       (cond
         (set? x)                                             :set
         (or (sequential? x) (is-array? x))                   :collection
         (or (number? x) (string? x) (boolean? x) (ident? x)) :scalar
         (fn? x)                                              :query
         :else                                                :object))))

(defn unqualify [sym]
  (debug-exceptions `unqualify
    (symbol (name sym))))
(defn de-clojure-core [x]
  (debug-exceptions `de-clojure-core
    (->> x (walk/postwalk #(cond-> % (and (symbol? %) (= (namespace %) "clojure.core")) unqualify)))))
(defn pretty-name [x]
  (debug-exceptions `pretty-name
    (let [x (de-clojure-core x)]
      (cond
        (seq? x) (let [[qs & args] x] (list* (datax/unqualify qs) args))
        () (str x)))))

#?(:clj
   (defn pretty-value [x server-pretty]
     (debug-exceptions `pretty-value
       (if (= ::not-found x)
         ""
         ((get server-pretty (class x) pr-str) x)))))

#?(:clj (defn pretty-title [query]
          (debug-exceptions `pretty-title
            (let [?f$ (first query)]
              (cons (cond-> ?f$ (qualified-symbol? ?f$) datax/unqualify)
                (mapv #(or (hfql/identify %) %) (next query)))))))

(e/defn Resolve [fq-sym fallback] (get e/*exports* fq-sym fallback))

;; `and` is glitch guard, TODO remove
#?(:clj (defn remove-opt [spec k] (and spec (hfql/props-update-opts spec #(dissoc % k)))))

(defn column-appender [s] (fn [v]
                            (debug-exceptions `column-appender
                              (str (subs v 0 (- (count v) 2)) "\n " s "]\n"))))

#?(:clj (defn collect-suggestions [o bindings]
          (debug-exceptions `collect-suggestions
            (with-bindings bindings
              (let [innate* (or (hfql/suggest o)
                              (when (or (coll? o) (sequential? o))
                                (hfql/suggest (nth o 0 nil))))
                    jvm* (hfql/suggest-java-class-members o)]
                (into innate* jvm*))))))

(defn now-ms []
  #?(:clj (System/currentTimeMillis)
     :cljs (.getTime (new js/Date))))

#?(:clj (def timed* (i/spine)))

(e/defn Draggable []
  (e/client
    (let [[mouse-x mouse-y] (dom/On "mousedown" (fn [e] (debug-exceptions `Draggable-mousedown) (when (zero? (.-button e)) [(.-x e) (.-y e)])) nil)
          t (tok/TokenNofail mouse-x)
          stl (.-style dom/node)]
      ;; (dom/props {:style {:cursor (if t "grabbing" "grab")}})
      (when t
        (let [[left top] (let [rect (.getBoundingClientRect dom/node)] [(.-left rect) (.-top rect)])
              [mouse-x2 mouse-y2] (dom/On "mousemove" (fn [e] [(.-x e) (.-y e)]) [mouse-x mouse-y])]
          (set! (.-left stl) (str (+ left (- mouse-x2 mouse-x)) "px"))
          (set! (.-top stl) (str (+ top (- mouse-y2 mouse-y)) "px"))
          (dom/On "mouseup" t nil))))))

(e/defn QueryMonitor []
  (dom/div
    (dom/props {:class "hyperfiddle-query-monitor"})
    (Draggable)
    (e/server
      (dom/div
        (dom/props {:style {:display "flex", :flex-direction "column-reverse"}})
        (e/for [{:keys [name start end state kill-fn]} (e/join timed*)]
          (dom/div
            (dom/props {:style {:display "flex", :justify-content "space-between"}})
            (dom/span
              (e/client
                (dom/button
                  (dom/text "X")
                  (dom/props {:disabled (e/server (nil? kill-fn))})
                  (let [t (tok/TokenNofail (dom/On "click" identity nil))]
                    (when t (t (e/server (when kill-fn (kill-fn true))))))))
              (dom/text name))
            (dom/span
              (when (= :killed state) (dom/strong (dom/props {:style {:color "red"}}) (dom/text "killed ")))
              (dom/text
                (- (or end (e/System-time-ms)) start)
                "ms"))))))))

(letfn [(?keep [!x f v] (when-some [x (f v)] (reset! !x [x])))]
  (e/defn Keep [f v]
    (let [!x (atom [])]
      (?keep !x f v)
      (e/diff-by {} (e/watch !x)))))

#_(binding [e/Timing (e/fn [label f] (dustingetz.loader/Offload f {:label label}))]) ; can override from userland
(e/defn Timing [label f]
  #_(dustingetz.loader/Offload f {:label label}) ; query monitoring, currently unstable
  (e/Offload f))

(e/defn Suggestions [o]
  (e/client
    #_(e/When (IDE-mode?)
      (dom/div
        (dom/text "suggestions:")
        (let [suggestions (e/server (Timing 'suggestions #(collect-suggestions o e/*bindings*))
                                    #_(or (with-bindings e/*bindings* (hfql/suggest o))
                                        (when (or (coll? o) (sequential? o))
                                          (with-bindings e/*bindings* (hfql/suggest (nth o 0 nil))))))]
          (e/When suggestions
            (dom/div
              (e/for [{:keys [label entry]} (e/diff-by {} suggestions)]
                (dom/button
                  (dom/text label)
                  (let [[t] (e/Token (dom/On "click" (fn [_] true) nil))]
                    (e/When t [t entry])))))))))))

(e/declare Render)

#?(:clj (defn map-keep-coll [f coll]
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
   (defn str-inline-coll [coll server-pretty]
     (debug-exceptions `str-inline-coll
       (let [coll-count (count coll)
             base (binding [*print-length* 1, *print-level* 2]
                    ;; `symbol` removes double quotes from the strings inside the collection
                    (-> (map-keep-coll #(symbol ((get server-pretty (class %) strx/pprint-str) %)) coll)
                      str
                      (str/replace (str \newline) (str \space))
                      (str/trim)))]
         (cond-> base (> coll-count 1) (str " " coll-count " elements"))))))

;; TODO removeme, glitch guard
(defn as-coll [v] (if (coll? v) v [v]))

(e/defn RenderInlineColl [v o spec]
  (let [pretty-v (str-inline-coll (as-coll v) *server-pretty)
        opts (hfql/opts spec)]
    (if-some [query (::hfql/link opts)]
      (router/link ['. [query]] (dom/text pretty-v))
      (dom/text pretty-v))))

(e/defn RenderTooltip [k opts v o spec]
  (when-some [Tooltip (Resolve (get opts k) (Resolve (get *block-opts k) nil))]
    (let [show? (dom/Mouse-over?)]
      (dom/props {:data-tooltip (when show? (Tooltip v o spec))}))))

(e/defn RenderCell [v o spec]
  (let [opts (hfql/opts spec)]
    (dom/td                          ; custom renderer runs in context of a cell
      (let [K (e/fn [v o spec]
                (RenderTooltip ::hfql/Tooltip opts v o spec)
                (if (or (set? v) (sequential? v))
                  (RenderInlineColl v o spec)
                  (if (= ::not-found v)
                    (dom/props {:data-empty true})
                    (let [pretty-v (pretty-value v *server-pretty)
                          denv {'% o, (hfql/unwrap spec) v, '%v v}]
                      (if-some [query (::hfql/link opts)]
                        (router/link ['. [(replace denv query)]] (dom/text pretty-v))
                        (dom/text pretty-v))))))]
        (binding [Render K]
          (e/call (Resolve (::hfql/Render opts) K) v o (remove-opt spec ::hfql/Render)))))))

(e/defn Render [v o spec] (RenderCell v o spec))

(e/defn Search! [saved-search]
  (->> (forms/Input! ::search saved-search :type :search :maxlength 100)
    (forms/Parse (e/fn [{search ::search}] [`Search! search]))))

(defn find-index [pred x*]
  (debug-exceptions `find-index
    (transduce (keep-indexed (fn [idx x] (when (pred x) idx))) (fn ([v] v) ([_ac nx] (reduced nx))) nil x*)))

(defn find-if [pred x*]
  (debug-exceptions `find-if
    (transduce (keep (fn [x] (when (pred x) x))) (fn ([v] v) ([_ac nx] (reduced nx))) nil x*)))


(defn- find-by [accessor-fn looked-up-key map]
  {:pre [(map? map) (ifn? accessor-fn)]
   :post [(or (nil? %) (map-entry? %))]}
  (reduce (fn [_ [key _value :as map-entry]]
            (when (= looked-up-key (accessor-fn key))
              (reduced map-entry)))
    nil map))

(defn- lookup-by
  ([accessor-fn looked-up-key map] (lookup-by accessor-fn looked-up-key nil map))
  ([accessor-fn looked-up-key default-not-found map]
   (if-let [map-entry (find-by accessor-fn looked-up-key map)]
     (val map-entry)
     default-not-found)))

#?(:clj
   (defn find-sitemap-spec [sitemap f$]
  (debug-exceptions `find-sitemap-spec
       (let [sitemap-spec (lookup-by first f$ ::not-found sitemap)]
         (if (= ::not-found sitemap-spec)
           (log/warn "Couldn't find sitemap definition for " (pr-str f$))
           sitemap-spec)))))

#?(:clj (defn find-spec-prop [raw-spec raw-k]
          (debug-exceptions `find-spec-prop
            (transduce (keep #(when (= raw-k (hfql/unwrap %)) %)) (fn ([v] v) ([_ac nx] (reduced nx))) nil raw-spec))))

#?(:clj (defn query->object [hfql-bindings query] ; run-query!
          (try (let [[f$ & args] query
                     f (hfql/resolve! f$)]
                 (with-bindings hfql-bindings (apply f args)))
               (catch Throwable e ;; swallowed exception possible here
                 (log/error e "Failed to run query" {:query (seq query)})
                 #_(throw e) nil ; FIXME glitch guard
                 ))))

#?(:clj (defn add-suggestions [spec pull-spec]
          (debug-exceptions `add-suggestions
            (hfql/props-update-k spec
              (fn [raw-spec]
                (reduce (fn [raw-spec pull]
                          (if (find-if #(= pull (hfql/unwrap %)) raw-spec)
                            raw-spec
                            (conj raw-spec pull)))
                  raw-spec pull-spec))))))

#?(:clj
   (defn- form-identifier [spec] ; TODO move to hfql
     (when-let [spec (hfql/unwrap spec)]
       (cond
         (ident? spec) spec
         (sequential? spec) (hfql/unwrap (first spec))
         (map? spec) (form-identifier (key (first spec)))
         :else spec))))

#?(:clj (defn labelize [?spec]
          (debug-exceptions `labelize
            (and ?spec (or (-> ?spec hfql/opts ::hfql/label) (form-identifier ?spec))))))

(e/defn ObjectRow [[k v] o spec shorten]
  (dom/td (dom/text (e/server (pretty-name (shorten (labelize spec))))))
  (Render v o spec))

(e/declare Block)

(e/defn Searcher [saved]
  (e/client
    (let [!s (atom (e/Snapshot saved))] ; currently we're the only writer so snapshotting is OK
      [(e/fn [disabled?]
         (->> (forms/Input! ::search saved :type :search :maxlength 100 :disabled disabled?)
           (forms/Parse (e/fn [{search ::search}] (reset! !s search) [`Search! search]))))
       (e/watch !s)])))

(e/defn NextBlock [query-template next-x o]
  (e/server
    (rebooting o
      (e/client
        (binding [*depth (inc *depth)]
          (let [{saved-search ::search} (nth router/route *depth {})
                [Search search] (Searcher saved-search)]
            (binding [*search search
                      e/*bindings* (e/server (assoc e/*bindings* (find-var `*search) search))]
              (let [query (e/server (replace {'% (hfql/identify o), '%v (hfql/identify next-x)} query-template))
                    next-o (e/server (loader/Initialized (Timing (pretty-title query) #(query->object e/*bindings* query)) nil))]
                (when (e/server (some? next-o))
                  (Block query next-o (e/server (find-sitemap-spec *sitemap (first query-template))) Search))))))))))

#?(:clj (defn not-entity-like? [x]
          (debug-exceptions `not-entity-like?
            (or (nil? x) (boolean? x) (string? x) (number? x) (ident? x) (vector? x) (is-array? x)))))

(e/defn AnonymousBlock [selection next-x]
  (e/server
    (rebooting next-x
      ;; similarity with `infer-block-type`
      ;; maybe blocks should decide if they handle this object?
      (when (Timing 'should-next-block-mount #(debug-exceptions `should-next-block-mount
                                                (or (sequential? next-x) (set? next-x) (is-array? next-x) (seq (hfql/suggest next-x)))))
        (e/client
          (binding [*depth (inc *depth)]
            (let [{saved-search ::search} (nth router/route *depth {})
                  [Search search] (Searcher saved-search)]
              (binding [*search search
                        e/*bindings* (e/server (assoc e/*bindings* (find-var `*search) search))]
                (Block [selection] next-x (e/server []) Search)))))))))

;; #?(:clj (defn find-default-page [page-defaults o] (some #(% o) page-defaults)))
#?(:clj (defn find-default-page [_page-defaults o]
          (debug-exceptions `find-default-page
            (hfql/resolve o)) #_(some #(% o) page-defaults)))

(defn ->short-map [cols-available! filterer]
  (debug-exceptions `->sort-map
    (let [k* (filterv filterer cols-available!)
          freq (frequencies (mapv datax/unqualify k*))]
      (into {} (map #(let [unq (datax/unqualify %)] [% (if (= 1 (freq unq)) unq %)]))
        k*))))

(defn column-shortener [symbolic-columns filterer]
  (let [short-map (->short-map symbolic-columns filterer)]
    (fn [symbolic-column]
      (debug-exceptions `column-shortener
        (let [symbolic-column #?(:clj (hfql/unwrap symbolic-column) :cljs symbolic-column)]
          (cond
            (filterer symbolic-column) (or (short-map symbolic-column)
                                         (and (ident? symbolic-column) (datax/unqualify symbolic-column)))
            (seq? symbolic-column) (let [[qs & args] symbolic-column] (list* (datax/unqualify qs) args))
            () symbolic-column))))))

#?(:clj
   (defn nav* [bindings coll k v]
     (try (debug-exceptions `nav* (with-bindings bindings (nav coll k v)))
          (catch Throwable e nil))))

(e/defn Nav [coll k v] (e/server (Timing 'Nav #(nav* e/*bindings* coll k v))))

#?(:clj (defn find-key-spec [spec k] (find-if #(= k (some-> % hfql/unwrap)) spec))) ; TODO remove some->, guards glitched if
#?(:clj (defn realize ; TODO useful enough and hard-to-get-right enough to promote to HFQL
          "Realize a lazy-seq to a plain list. Values that are not a lazy-seq just passes
  through. Differs from `doall` in that `(str (doall seq))` will print an opaque
  object address, whereas `(str (realize seq))` will print as a plain list. Will
  hang forever on infinite seqs."
          [supposedly-lazy-seq]
          (if (seq? supposedly-lazy-seq) (list* supposedly-lazy-seq) supposedly-lazy-seq)))

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

(defmulti documentation-dispatch type)
(defmethod documentation-dispatch :default [x] (type x))
(defmethod documentation-dispatch (type :kw) [kw] kw)
(defmethod documentation-dispatch (type 'sym) [sym] sym)
(defmethod documentation-dispatch (type "str") [str] str)

(defmulti documentation documentation-dispatch)
(defmethod documentation :default [_] nil)
#?(:clj (defmethod documentation (type #'type) [var] (:doc (meta var))))

#?(:clj (defn- find-documentation [query]
          (when-let [identifier (form-identifier query)]
            (str/triml
              (str (documentation
                     (cond (qualified-symbol? identifier) (resolve identifier)
                           ;; TODO extend
                           ;; (pred/java-method-symbol? identifier) ...
                           :else nil))
                "\n\n")))))

#?(:clj (defn- kvs-sort-by-ordered-keyset
          "Sort a map structure by keys. Static selected keys (from pullspec) have
  priority and are laid out in order, as in an ordered map. Extra keys comes later and are sorted by
  `comparator`. Return a seq of map entries."
          [pullspec kvs]
          {:pre [(sequential? pullspec) (every? map-entry? kvs)]
           :post [(every? map-entry? %) (= (count %) (count kvs))]}
          (let [static-keys (map hfql/unwrap pullspec)
                static-keyset (set static-keys)
                [static-kvs dynamic-kvs] (contrib.data/group-by-pred (comp boolean static-keyset key) kvs)
                static-map (into {} static-kvs)]
            (concat (keep (fn [pullspec-key] (find static-map (hfql/unwrap pullspec-key))) static-keys)
              (?sort-by key (remove (comp some? static-keyset key) dynamic-kvs))))))

#?(:clj (defn- kvs-filter-by-needle [kvs needle]
          {:pre [(every? map-entry? kvs) (or (string? needle) (nil? needle))]
           :post [(every? map-entry? %) (<= (count %) (count kvs))]}
          (if (empty? needle)
            kvs
            (filter (fn [[k v]] (or (strx/includes-str? (realize k) needle) (strx/includes-str? v needle)))
              kvs))))

(e/defn ObjectBlock [query o spec effect-handlers Search args]
  (e/client
    (let [{saved-selection ::selection} args
          opts (e/server (hfql/opts spec))
          browse? true #_(Browse-mode?)
          ;; TODO remove Reconcile eventually? Guards mount-point bug in forms4/Picker! - is the bug present in forms5?
          spec2 (e/server (e/Reconcile (cond-> spec browse? (add-suggestions (Timing 'add-suggestions #(hfql/suggest o))))))
          raw-spec (e/server (hfql/unwrap spec2))
          shorten (e/server (column-shortener (mapv labelize raw-spec) symbol?))
          default-select (e/server (::hfql/select opts))
          pulled (e/server (Timing (cons 'pull (pretty-title query)) #(hfql/pull e/*bindings* raw-spec o)))
          filtered (e/server (kvs-filter-by-needle pulled *search))
          data (e/server (kvs-sort-by-ordered-keyset (hfql/unwrap spec) filtered))
          row-count (e/server (count data)), row-height 24]
      (binding [*block-opts (e/server (hfql/opts spec))]
        (dom/fieldset
          (dom/props {:class "entity hyperfiddle-navigator4__block"})
          (let [search-cmd (dom/legend
                             (dom/span (dom/props {:class "title" :data-tooltip (e/server (dustingetz.str/blank->nil (find-documentation query)))})
                                       (dom/text (e/server (pretty-title query)) " "))
                             (Search false))]
            (dom/props {:style {:--column-count 2 :--row-height row-height}})
            (forms/Interpreter effect-handlers
              (e/amb
                (e/When search-cmd search-cmd)
                (let [!index (e/server (atom nil))]
                  (e/server (reset! !index (find-index (fn [[k]] (= k saved-selection)) data)))
                  (->> (forms/TablePicker! ::selection (e/server (e/watch !index)) row-count
                         (e/fn [index] (e/server
                                         (let [kv (nth data index nil)]
                                           ;; rebooting kv would solve glitches but degrades performance too severely
                                           (when-some [k (#(some-> kv key))] ; TODO simplify, guards glitched if
                                             (ObjectRow kv o (find-key-spec raw-spec k) shorten)))))
                         :row-height row-height
                         :column-count 2)
                    (forms/Parse (e/fn ToSaved [{index ::selection}]
                                   (e/server
                                     (let [x (nth data index nil)
                                           row-select (-> (nth raw-spec index nil) hfql/opts ::hfql/select)
                                           select (or row-select default-select)]
                                       (e/When (or browse? (and x select))
                                         (reset! !index index)
                                         (first x))))))
                    (forms/Parse (e/fn ToCommand [saved] [`Select! saved]))))))))
        (when saved-selection
          (let [next-x (e/server (when-some [nx (find-if (fn [[k _v]] (= k saved-selection)) pulled)] ; when-some because glitch
                                   (rebooting nx
                                     (Nav o (key nx) (val nx)))))
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
                            (AnonymousBlock saved-selection next-x)))))))))))))))

(e/defn TableRow [cols col->spec o m]
  (e/for [col cols]
    (Render (get m col ::not-found) o (e/server (col->spec col)))))

(defn toggle-column-sort [sort-spec for-col]
  (when-some [[col asc?] (first sort-spec)]
    [[for-col (if (= col for-col) (not asc?) true)]]))

(e/defn TableHeader [raw-spec !sort-spec]
  (e/server
    (dom/thead
      (dom/tr
        (let [shorten (column-shortener (mapv labelize raw-spec) ident?)]
          (e/for [spec (e/diff-by {} raw-spec)]
            (let [k (hfql/unwrap spec)
                  label (shorten (labelize spec))]
                (dom/th
                  (dom/props {:title (str (labelize spec))})
                  (e/server (RenderTooltip ::hfql/ColumnHeaderTooltip (hfql/opts spec) k nil k))
                  (dom/On "click" #(swap! !sort-spec toggle-column-sort k) nil)
                  (dom/text label)))))))))

(defn format-query-meta [query-meta]
  (some-> query-meta not-empty dustingetz.str/pprint-str))

(def collection-limit 10000)

(defn format-collection-count
  ;; FIXME Today we can't differentiate collections strictly equal to
  ;; `collection-limit` from collections larger than `collection-limit`.
  [coll-count collection-limit]
  {:pre [(number? coll-count) (number? collection-limit)]
   :post [(string? %)]}
  (debug-exceptions `format-collection-count
    (let [reached-max-in-memory-lenght? (>= coll-count collection-limit)]
      (str " ("
        (when reached-max-in-memory-lenght?
          ">=") ; FIXME we'd like to display ">" instead
        coll-count
        " items"
        (when reached-max-in-memory-lenght?
          " · truncated")
        ")"))))

#?(:clj (defn- table-title-tooltip-content [query query-meta]
          {:post [(or (string? %) (nil? %))]}
          (-> (find-documentation query)
            (str (format-query-meta query-meta))
            (str/trim)
            (dustingetz.str/blank->nil))))

(e/defn TableTitle [query Search row-count spec query-meta Suggest*]
  (dom/legend
    (dom/span
      (dom/span
        (dom/props {:class "title" :data-tooltip (e/server (table-title-tooltip-content query query-meta))})
        (dom/text (pretty-title query)))
      (e/as-vec ; dirty trick to circumvent dom/text in between Search and columns
        (e/amb
          (dom/span ; dirty trick to circumvent sited destructuring out of TableTitle
            (e/client (let [node dom/node] (e/fn [] (binding [dom/node node] (Search false #_(>= row-count collection-limit)))))))
          (dom/text (format-collection-count row-count collection-limit))
          (let [k* (e/server (into #{} (map hfql/unwrap) (hfql/unwrap spec)))
                label* (e/server (into #{} (map labelize) (hfql/unwrap spec)))
                pre-checked (empty? k*)
                new-suggest* (into [] (remove k*) (Suggest*))]
            (dom/span
              (let [shorten (column-shortener (into label* new-suggest*) ident?)
                    selected (e/as-vec (e/for [entry (e/diff-by {} new-suggest*)]
                                         (e/When (forms/Checkbox* pre-checked :label (shorten (labelize entry)) :title (labelize entry))
                                           entry)))
                    from-spec (e/as-vec (e/for [spec (e/diff-by {} (hfql/unwrap spec))]
                                          (e/When (forms/Checkbox* true :label (shorten (labelize spec)) :title (labelize spec))
                                            spec)))]
                (hfql/props-update-k spec (fn [_] (into from-spec selected)))))))))))

#?(:clj
   (defn- select-row-if-match-url! [!index row-index row-object url-symbolic-representation]
     {:pre [(pred/atom-like? !index) (number? row-index)]
      :post [(nil? %)]}
     (if (and (hfql/identifiable? row-object)
             (= (hfql/identify row-object) url-symbolic-representation)) ; both can be nil
       (reset! !index row-index) ; current row matches. Set selection.
       (compare-and-set! !index row-index nil)) ; current row used to match and now doesn't. Clear selection.
     nil))

#?(:clj
   (defn- nav-row [bindings data row-index]
     (when-some [row-object (nth data row-index nil)]
       [row-index row-object (nav* bindings data nil row-object)])))

(e/defn CollectionTableBody [row-count row-height cols data raw-spec saved-selection select]
  (let [!selected-index (e/server (atom nil))
        selected-index (e/server (e/watch !selected-index))
        col->spec (e/server (into {} (map (fn [x] [(hfql/unwrap x) x])) raw-spec))
        ]
    (->> (forms/TablePicker! ::selection
           selected-index
           row-count
           (e/fn [row-index]
             (e/server
               (when-some [[row-index row-object row-hydrated-object] (nav-row e/*bindings* data row-index)] ; FIXME glitch guard, ensures index travels with row
                 (select-row-if-match-url! !selected-index row-index row-hydrated-object saved-selection)
                 (let [pulled (#(when row-hydrated-object (hfql/pull e/*bindings* raw-spec row-hydrated-object)))] ; FIXME conditional glitch guard
                   (TableRow cols col->spec row-object pulled)))))
           :row-height row-height
           :column-count (e/server (count raw-spec))
           :as :tbody)
      (forms/Parse (e/fn ToCommand [{selected-index ::selection}]
                     (e/server
                       (reset! !selected-index selected-index)
                       (let [selected-object (Nav data nil (nth data selected-index nil))]
                         (if-let [symbolic-representation (hfql/identify selected-object)]
                           [`Select! symbolic-representation]
                           [`Noop])))))
      (forms/Interpreter {`Noop (e/fn [] [::forms/ok])}))))

(defn timef [label f]
  (pr label)
  (time (f)))

#?(:clj (defn eager-pull-search-sort [data spec hfql-bindings search sort-spec]
          (debug-exceptions `eager-pull-search-sort
            #_(Thread/sleep 3000)
            (let [metadata (meta data)
                  data (with-meta (vec data) metadata)         ; fix if data is e.g. a set
                  navd (with-bindings hfql-bindings (into [] (map #(nav data nil %)) data))
                  pulled (hfql/pull hfql-bindings spec navd)
                  filtered (eduction (map-indexed vector)
                             (if (not-empty search)
                               (filter #(strx/any-matches? (vals (second %)) search))
                               (map identity))
                             pulled)
                  sorted (vec (if-some [sorter (and (not-empty sort-spec) (->sort-comparator sort-spec))]
                                (try (sort-by second sorter filtered) (catch Throwable _ filtered))
                                filtered))]
              (with-meta (into [] (comp (map first) (map #(nth data %))) sorted)
                metadata)))))

(e/defn CollectionBlock [query data spec effect-handlers Search args]
  (e/client
    (let [{saved-selection ::selection} args
          select (e/server (::hfql/select (hfql/opts spec)))
          !sort-spec (atom [[(e/server (form-identifier spec)) true]]),
          sort-spec (filterv first (e/watch !sort-spec)) #_(loader/Latch (e/watch ) (e/Filter ffirst (e/watch !sort-spec)))
          !row-count (atom 0), row-count (e/watch !row-count)]
      (dom/fieldset
        (dom/props {:class "entity-children hyperfiddle-navigator4__block"})
        (let [free-args (e/server
                          (TableTitle query Search row-count spec (dissoc (meta data) `clojure.core.protocols/nav)
                            (e/fn []
                              (when true #_(Browse-mode?)
                                (let [navd (Nav data nil (e/Snapshot (first data)))] ; snapshot so we don't re-infer if query shrinks to 0. Also prevents nasty crash
                                  (Timing 'infer-columns #(hfql/suggest navd)))))))
              search-cmd (e/call (e/server (first free-args))) ; fighting against sited destructuring
              spec2      (e/server (second free-args))
              raw-spec2 (e/server (hfql/unwrap spec2))
              row-height 24
              cols (e/server (e/diff-by {} (mapv hfql/unwrap raw-spec2)))
              column-count (e/server (count raw-spec2))
              data-count (e/server (bounded-count collection-limit data))
              data (e/server (if (< data-count collection-limit)
                               (eager-pull-search-sort data raw-spec2 e/*bindings* *search sort-spec)
                               (with-meta (into [] (take collection-limit) data) (meta data))))
              data-count (e/server (bounded-count collection-limit data))]
          (reset! !row-count data-count)
          ;; cycle back first column as sort in browse mode
          (when (and #_(Browse-mode?) (e/server (nil? (some-> (hfql/unwrap spec) first))))
            (reset! !sort-spec [[(e/server (some-> (hfql/unwrap spec2) first hfql/unwrap)) true]]))
          search-cmd                    ; force order
          (dom/table
            (dom/props {:style {:--row-height (str row-height "px"), :--column-count column-count}})
            (TableHeader raw-spec2 !sort-spec)
            (forms/Interpreter effect-handlers
              (e/amb
                (e/When search-cmd search-cmd)
                (binding [*block-opts (e/server (hfql/opts spec))]
                  (CollectionTableBody row-count row-height cols data raw-spec2 saved-selection select)))))))
      (when saved-selection
        (let [next-x (e/server
                       (loader/Initialized (Timing 'next-object
                                          (fn []
                                            (debug-exceptions `CollectionBlock-next-object
                                              (with-bindings e/*bindings*
                                                (some #(let [navd (nav data nil %)]
                                                         (when (= saved-selection (or (hfql/identify %) %))
                                                           navd))
                                                  data)))))
                         nil))]
          (rebooting select
            (if select
              ;; In ObjectBlock we pass the selected object and the root object.
              ;; Here the root object is a query fn, or the filtered collection.
              ;; Do we want/need to pass it? Should we bind it to `%`?
              (NextBlock select next-x next-x)
              (when true #_(Browse-mode?)
                (e/server
                  (let [query-template (find-default-page *page-defaults next-x)]
                    (rebooting query-template
                      (e/client
                        (if query-template
                          (NextBlock query-template next-x next-x)
                          (AnonymousBlock saved-selection next-x))))))))))))))

#?(:clj (defn sitemapify [spec]
          (debug-exceptions `sitemapify
            (walk/postwalk
              (fn [x] (if (hfql/opts x)
                        (list 'hfql/props (hfql/unwrap x) (hfql/opts x))
                        x))
              spec))))

(defn append-to-query [sitemap query added-spec]
  (debug-exceptions `append-to-query
    (reduce-kv (fn [ac k v] (assoc ac k (cond-> v (= (first k) (first query)) (conj added-spec))))
      {} sitemap)))

(def default-mode :browse)

(e/defn RoundTrip [v] (identity (e/server (identity v)))) ; Awesome debug tool! Forces a roundtrip to diagnose how many roundtrips it takes to ack a value!
;; (RoundTrip (RoundTrip :foo)) ; two roundtrips

(e/defn Block [query o spec Search]
  (when-some [F (e/server (e/Reconcile (case (infer-block-type o)
                            :object ObjectBlock
                            :collection CollectionBlock
                            :set CollectionBlock
                                         #_else nil)))]
    #_(when (IDE-mode?)
      (let [update-text (dom/textarea
                          (dom/props {:rows 10, :cols 80})
                          (dom/text (e/server (strx/pprint-str (sitemapify spec))))
                          (fn [f] (debug-exceptions `Block-update-text
                                    (set! (.-value dom/node) (f (.-value dom/node))))))]
        update-text
        (e/for [[t v] (Suggestions o)]
          (update-text (column-appender v))
          (case (e/server
                  (*sitemap-writer
                    (sitemapify
                      (append-to-query (e/Snapshot *sitemap) query v)))
                  #_(*sitemap-writer (sitemapify (update *sitemap (list* query) conj v))))
            (t)))))
    (let [effect-handlers
          {`Search! (e/fn [s]
                      ;; `update` doesn't work, route is a lazyseq (?)
                      (router/ReplaceState! ['. (into (conj (into [] (take *depth) router/route)
                                                        (assoc (nth router/route *depth {}) ::search s))
                                                  (drop (inc *depth) router/route))])
                      [::forms/ok])
           `Select! (e/fn [symbolic-x]
                      (router/Navigate! ['. (conj (into [] (take *depth) router/route)
                                              (assoc (nth router/route *depth {}) ::selection symbolic-x))])
                      ;; Want the selected row to transition atomically? : (RoundTrip (RoundTrip [::forms/ok]))
                      ;; Two roundtrids because:
                      ;;  - one roundtrip because TablePicker! receives the selected index from server (latency)
                      ;;  - second roundtrip because of an Electric bug leo is fixing
                      ;;     - Electric produces an extra roundtrip on e/fn call.
                      [::forms/ok])}]
      #_(reset! !mode (or (e/server (-> spec hfql/opts ::hfql/mode)) default-mode))
      (F query o spec effect-handlers Search (nth router/route *depth {})))))

(e/defn ModePicker [mode]
  (let [edit (forms/RadioPicker! nil (name mode) :Options (e/fn [] (e/amb "crud" "ide" "browse")))]
    (if (e/Some? edit)
      (keyword (::forms/value edit))
      mode)))

(e/defn HfqlRoot [sitemap entrypoints]
  (e/client
    (dom/style (dom/text css tooltip/css))
    (Index sitemap entrypoints) ; FIXME this index generation is generic and may conflict with user-custom index (e.g. ObjectBrowser3 fiddle demo renders two navs)
    (tooltip/TooltipArea
      (e/fn []
        (tooltip/Tooltip)
        (binding [!mode (atom default-mode)
                  *server-pretty (e/server (#(or *server-pretty {})))
                  *sitemap sitemap]
          (binding [*mode (e/watch !mode) #_(ModePicker mode)
                    *depth 0]
            (let [[query] router/route]
              (rebooting query
                (dom/div
                  (dom/props {:class "Browser"})
                  #_(QueryMonitor)
                  (if-not query
                    (router/ReplaceState! ['. [(first entrypoints)]])
                    (let [f$ (first query)]
                      (set! (.-title js/document) (str (some-> f$ name (str " – ")) "Hyperfiddle"))
                      (dom/props {:class (cssx/css-slugify f$)})
                      (NextBlock query nil nil))))))))))))

(def table-block-css
"
.hyperfiddle-navigator4__block table { display: grid; grid-template-columns: repeat(var(--column-count), 1fr);  grid-template-rows: var(--row-height);}

.hyperfiddle-navigator4__block table thead { display: contents; }
.hyperfiddle-navigator4__block table thead tr { display: grid; grid-row: 1; grid-column: 1 / -1; grid-template-columns: subgrid;}
.hyperfiddle-navigator4__block table thead tr th { white-space: nowrap; text-overflow: ellipsis; overflow: hidden; }

.hyperfiddle-navigator4__block .hyperfiddle-electric-forms5__table-picker { grid-row: 2; grid-column: 1 / -1; grid-template-columns: subgrid; }

"
  )

(def css
  (str forms/css
    loader/css
    table-block-css

    "
/* cosmetic defaults */
.hyperfiddle-navigator4__block legend .title {font-weight:600;}
.hyperfiddle-navigator4__block { padding: 0; background-color: white; }
.hyperfiddle-navigator4__block table { border: 0px solid #f2f2f2; border-top-left-radius: 0rem; border-top-right-radius: 0rem; }
.hyperfiddle-navigator4__block table thead tr { background-color: #f2f2f2; border-bottom: 1px lightgray solid; }
.hyperfiddle-navigator4__block table thead tr th { white-space: nowrap; text-overflow: ellipsis; overflow: hidden; }
.hyperfiddle-navigator4__block table thead tr th { font-weight: 500; }
.hyperfiddle-navigator4__block table thead tr th:not(:first-child) { border-left: 1px lightgray solid; }
.hyperfiddle-navigator4__block table :is(th, td) { padding: 0 0.25em; }
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

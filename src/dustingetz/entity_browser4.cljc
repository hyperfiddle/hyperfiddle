(ns dustingetz.entity-browser4
  (:require [contrib.css :as cssx]
            [contrib.assert :as ca]
            [contrib.data :as datax]
            [contrib.debug :as dbg]
            [dustingetz.str :as strx]
            [peternagy.hfql #?(:clj :as :cljs :as-alias) hfql]
            #?(:clj [peternagy.file-watcher :as fw])
            #?(:clj [clojure.java.io :as io])
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-forms5 :as forms]
            [hyperfiddle.incseq :as i]
            [hyperfiddle.ui.tooltip :as tooltip]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.router4 :as router]
            [hyperfiddle.nav0 :as hfp]
            [hyperfiddle.token-zoo0 :as tok]
            [missionary.core :as m]
            [clojure.datafy :as datafy]
            [clojure.string :as str]
            [clojure.pprint]
            [edamame.core :as eda]
            [clojure.walk :as walk])
  #?(:clj (:import [java.io File]))
  #?(:cljs (:require-macros dustingetz.entity-browser4)))

(defmacro rebooting [sym & body] `(e/for [~sym (e/diff-by identity (e/as-vec ~sym))] ~@body))

(e/declare *hfql-bindings *mode !mode *update *sitemap-writer *sitemap *page-defaults *block-opts *depth)
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

;; `and` is glitch guard, TODO remove
#?(:clj (defn remove-opt [spec k] (and spec (hfql/props-update-opts spec #(dissoc % k)))))

(defn column-appender [s] (fn [v] (str (subs v 0 (- (count v) 2)) "\n " s "]\n")))

#?(:clj (defn collect-suggestions [o bindings]
          (with-bindings bindings
            (let [innate* (or (hfql/suggest o)
                            (when (or (coll? o) (sequential? o))
                              (hfql/suggest (nth o 0 nil))))
                  jvm* (hfql/suggest-jvm o)]
              (into innate* jvm*)))))

(defn now-ms []
  #?(:clj (System/currentTimeMillis)
     :cljs (.getTime (new js/Date))))

#?(:clj (def timed* (i/spine)))

(e/defn QueryMonitor []
  (dom/div
    (dom/props {:class "hyperfiddle-query-monitor"})
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


(defn offload-latch-timeout [<f executor timeout-ms timeout-v]
  (i/diff-by {}
    (m/reductions {} []
      (m/ap
        (try [(m/? (m/timeout (m/via-call executor (m/?< <f)) timeout-ms timeout-v))]
             (catch #?(:clj Throwable :cljs :default) e
               ;; Swallow all exceptions if current Thread is already interrupted - rethrow otherwise.
               ;; Catching InterruptedException is not enough, because Datomic (at least) will throw a
               ;; domain-specific exception on thread interruption.
               (try (m/!) (catch missionary.Cancelled _ (m/amb)))
               (throw e)))))))

;; like e/Offload but can time out
(e/defn Offload [f! executor timeout-ms timeout-v]
  (e/join (offload-latch-timeout (e/join (i/items (e/pure f!))) executor timeout-ms timeout-v)))

(letfn [(?keep [!x f v] (when-some [x (f v)] (reset! !x [x])))]
  (e/defn Keep [f v]
    (let [!x (atom [])]
      (?keep !x f v)
      (e/diff-by {} (e/watch !x)))))

(let [!cache (atom {::idx 0})
      ->idx (fn [nm]
              (-> (swap! !cache (fn [{i ::idx :as c}]
                                  (if (contains? c nm)
                                    c
                                    (-> c (update ::idx inc) (assoc nm (inc i))))))
                (get nm)))]
  (letfn [(timed-started [nm _ dfv]
            #?(:clj
               (timed* (->idx nm) (fn [ac _]
                                    (case (:state ac)
                                      (:re-ended) (assoc ac :start (:end ac), :state :ended)
                                      (:re-killed) (assoc ac :start (:end ac), :state :killed)
                                      #_else (-> ac (dissoc :end) (assoc :name nm, :start (now-ms), :state :started, :kill-fn dfv)))) nil)))
          (timed-finished [nm t] ((if (= :ok t) timed-ended timed-killed) nm))
          (timed-killed [nm] #?(:clj (timed* (->idx nm) (fn [ac _] (-> ac (dissoc :kill-fn) (assoc :end (now-ms), :state (case (:state ac) (:killed :ended) :re-killed #_else :killed)))) nil)))
          (timed-ended [nm] #?(:clj (timed* (->idx nm) (fn [ac _] (-> ac (dissoc :kill-fn) (assoc :end (now-ms), :state (case (:state ac) (:killed :ended) :re-ended #_else :ended)))) nil)))
          (run-fn [f dfv]
            (m/race
              (m/sp [:killed (m/? dfv)])
              (m/sp [:ok (m/? (m/via-call m/blk f))])))
          (keep-ok [[t v]] (when (= t :ok) v))]
    (e/defn Timing [nm f]
      (let [dfv ((fn [_] (m/dfv)) f)
            [t _v :as tv] (e/Task (run-fn f dfv))]
        (timed-started nm f dfv)
        (timed-finished nm t)
        (Keep keep-ok tv)))))

(e/defn Suggestions [o]
  (e/client
    (e/When (IDE-mode?)
      (dom/div
        (dom/text "suggestions:")
        (let [suggestions (e/server (Timing 'suggestions #(collect-suggestions o *hfql-bindings))
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

;; TODO removeme, glitch guard
(defn as-coll [v] (if (coll? v) v [v]))

(e/defn RenderInlineColl [v o spec]
  (let [pretty-v (str-inline-coll (as-coll v))
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
                  (let [pretty-v (pretty-value v)
                        denv {'% o, (hfql/unwrap spec) v, '%v v}]
                    (if-some [query (::hfql/link opts)]
                      (router/link ['. [(replace denv query)]] (dom/text pretty-v))
                      (dom/text pretty-v)))))]
        (binding [Render K]
          (e/call (Resolve (::hfql/Render opts) K) v o (remove-opt spec ::hfql/Render)))))))

(e/defn Render [v o spec] (RenderCell v o spec))

(e/defn Search! [saved-search]
  (->> (forms/Input! ::search saved-search :type :search)
    (forms/Parse (e/fn [{search ::search}] [`Search! search]))))

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

#?(:clj (defn query->object [hfql-bindings query] ; run-query!
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
              next-o (e/server (Timing (pretty-title query) #(query->object *hfql-bindings query)))]
          (when (e/server (some? next-o))
            (binding [*depth (inc *depth)]
              (Block query next-o (e/server (find-sitemap-spec *sitemap (first query-template)))))))))))

#?(:clj (defn not-entity-like? [x] (or (nil? x) (boolean? x) (string? x) (number? x) (ident? x) (vector? x) (.isArray (class x)))))

(e/defn AnonymousBlock [selection next-x]
  (e/server
    (rebooting next-x
      ;; similarity with `infer-block-type`
      ;; maybe blocks should decide if they handle this object?
      (when (Timing 'should-next-block-mount #(or (sequential? next-x) (set? next-x) (seq (hfql/suggest next-x))))
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

(e/defn Nav [coll k v] (e/server (Timing 'Nav #(with-bindings *hfql-bindings (datafy/nav coll k v)))))

#?(:clj (defn find-key-spec [spec k] (find-if #(= k (some-> % hfql/unwrap)) spec))) ; TODO remove some->, guards glitched if
#?(:clj (defn ?unlazy [o] (cond-> o (seq? o) list*)))

(defn ?sort-by [keyfn v*]
  (try (sort-by keyfn v*)
       (catch #?(:clj Throwable :cljs :default) e
         (prn 'failed-to-sort (type e) (ex-message e))
         v*)))

(e/defn ObjectBlock [query o spec effect-handlers args]
  (e/client
    (let [{saved-search ::search, saved-selection ::selection} args
          opts (e/server (hfql/opts spec))
          browse? (Browse-mode?)
          ;; TODO remove Reconcile eventually? Guards mount-point bug in forms4/Picker! - is the bug present in forms5?
          spec2 (e/server (e/Reconcile (cond-> spec browse? (add-suggestions (Timing 'add-suggestions #(hfql/suggest o))))))
          raw-spec (e/server (hfql/unwrap spec2))
          shorten (e/server (column-shortener (mapv hfql/unwrap raw-spec) symbol?))
          default-select (e/server (::hfql/select opts))
          !search (atom nil), search (e/watch !search)
          pulled (e/server (Timing (cons 'pull (pretty-title query)) #(hfql/pull *hfql-bindings raw-spec o)))
          data (e/server
                 (?sort-by key
                   (into [] (keep (fn [kspec]
                                    (let [k (hfql/unwrap kspec)
                                          v (get pulled k)]
                                      (when (or (strx/includes-str? (?unlazy k) saved-search)
                                              (strx/includes-str? v saved-search))
                                        (datax/map-entry k v)))))
                     raw-spec)))
          row-count (e/server (count data)), row-height 24]
      (binding [*block-opts (e/server (hfql/opts spec))]
        (dom/fieldset
          (dom/props {:class "entity dustingetz-entity-browser4__block"})
          (dom/legend
            (dom/span (dom/props {:class "title"}) (dom/text (e/server (pretty-title query)) " "))
            ;; TODO remove ugly workaround, solves bug where search travels across navigation
            (let [[t search] (Search! saved-search)]
              (reset! !search (e/When t [(forms/after-ack t #(reset! !search nil)) search]))))
          (dom/props {:style {:--column-count 2 :--row-height row-height}})
          (forms/Interpreter effect-handlers
            (e/amb
              (e/When search search)
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

(e/defn TableHeader [cols !sort-spec spec]
  (dom/thead
    (dom/tr
      (let [shorten (column-shortener (e/as-vec cols) ident?)]
        (e/for [col cols]
          (dom/th
            (dom/props {:title (str col)})
            (e/server (RenderTooltip ::hfql/TitleTooltip (hfql/opts spec) col nil col))
            (dom/On "click" #(swap! !sort-spec toggle-column-sort col) nil)
            (dom/text (pretty-name (shorten col)))))))))

(e/defn TableTitle [query !search saved-search row-count spec query-meta suggest*]
  (dom/legend
    (dom/span
      (dom/props {:class "title" :title (e/server (pr-str query-meta))})
      (dom/text (pretty-title query))
      (dom/text " ")
      ;; TODO remove ugly workaround, solves bug where search travels across navigation
      (e/client (let [[t search] (Search! saved-search)]
                  (reset! !search (e/When t [(forms/after-ack t #(reset! !search nil)) search])))
                nil)
      (dom/text " (" row-count " items) ")
      (let [k* (into #{} (map hfql/unwrap) (hfql/unwrap spec))
            pre-checked (empty? k*)
            new-suggest* (into [] (comp (map :entry) (remove k*)) suggest*)
            shorten (column-shortener (into k* new-suggest*) ident?)
            selected (e/as-vec
                       (e/for [entry (e/diff-by {} new-suggest*)]
                         (e/When (forms/Checkbox* pre-checked :label (shorten entry))
                           entry)))]
        (e/for [k (e/diff-by {} (mapv hfql/unwrap (hfql/unwrap spec)))]
          (forms/Checkbox* true :disabled true :label (shorten k)))
        (hfql/props-update-k spec (fn [raw-spec] (into raw-spec selected)))))))

#_
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
          ;; (Thread/sleep 1500)
          (let [enriched (hfql/pull hfql-bindings spec data)
                filtered (eduction (filter #(strx/any-matches? (vals %) search)) enriched)]
            (vec (if-some [sorter (->sort-comparator sort-spec)]
                   (try (sort sorter filtered) (catch Throwable _ filtered))
                   filtered)))))

(e/defn CollectionTableBody [row-count row-height cols data raw-spec saved-selection select]
  (let [!index (e/server (atom nil))
        col->spec (e/server (into {} (map (fn [x] [(hfql/unwrap x) x])) raw-spec))]
    (e/server (reset! !index (find-index #{saved-selection} (eduction (map #(let [o (-> % meta ::hfql/origin)] (or (hfp/identify o) o))) data))))
    (->> (forms/TablePicker! ::selection
           (e/server (e/watch !index))
           row-count
           (e/fn [index] (e/server (some->> (nth data index nil)
                                     (TableRow cols col->spec data))))
           :row-height row-height
           :column-count (e/server (count raw-spec))
           :as :tbody)
      (forms/Parse (e/fn ToSavable [{index ::selection}]
                     (e/When (or select (Browse-mode?))
                       (e/server
                         (let [x (-> (nth data index nil) meta ::hfql/origin)
                               symbolic-x (hfp/identify x)]
                           (e/When symbolic-x
                             (reset! !index index)
                             symbolic-x))))))
      (forms/Parse (e/fn ToCommand [symbolic-x] [`Select! symbolic-x])))))

;; CollectionBlock is naive, doing N+1 queries and doing work in memory
(e/defn CollectionBlock [query unpulled spec effect-handlers args]
  (e/client
    (let [{saved-search ::search, saved-selection ::selection} args
          select (e/server (::hfql/select (hfql/opts spec)))
          !sort-spec (atom [[(e/server (some-> (hfql/unwrap spec) first hfql/unwrap)) true]]), sort-spec (e/watch !sort-spec)
          !search (atom nil), search (e/watch !search)
          !row-count (atom 0), row-count (e/watch !row-count)]
      (dom/fieldset
        (dom/props {:class "entity-children dustingetz-entity-browser4__block"})
        (let [spec2 (e/server
                      (TableTitle query !search saved-search row-count spec (dissoc (meta unpulled) `clojure.core.protocols/nav)
                        (when (Browse-mode?)
                          (let [navd (Nav unpulled nil (first unpulled))]
                            (Timing 'infer-columns #(hfql/suggest navd))))))
              raw-spec2 (e/server (hfql/unwrap spec2))
              data (e/server (Timing (cons 'pull (pretty-title query))
                               (fn [] (eager-pull-search-sort
                                        ((fn [] (with-bindings *hfql-bindings (mapv #(datafy/nav unpulled nil %) unpulled))))
                                        raw-spec2 *hfql-bindings saved-search sort-spec))))
              row-height 24
              cols (e/server (e/diff-by {} (mapv hfql/unwrap raw-spec2)))
              column-count (e/server (count raw-spec2))]
          (reset! !row-count (e/server (count data)))
          ;; cycle back first column as sort in browse mode
          (when (and (Browse-mode?) (e/server (nil? (some-> (hfql/unwrap spec) first))))
            (reset! !sort-spec [[(e/server (some-> (hfql/unwrap spec2) first hfql/unwrap)) true]]))
          (dom/table
            (dom/props {:style {:--row-height (str row-height "px"), :--column-count column-count}})
            (TableHeader cols !sort-spec spec)
            (forms/Interpreter effect-handlers
              (e/amb
                (e/When search search)
                (binding [*block-opts (e/server (hfql/opts spec))]
                  (CollectionTableBody row-count row-height cols data raw-spec2 saved-selection select)))))))
      (when saved-selection
        (let [next-x (e/server
                       (Timing 'next-object
                         (fn [] (with-bindings *hfql-bindings
                                  (some #(let [navd (datafy/nav unpulled nil %)]
                                           (when (= saved-selection (or (hfp/identify %) %))
                                             navd))
                                    unpulled)))))]
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

(e/defn RoundTrip [v] (identity (e/server (identity v)))) ; Awesome debug tool! Forces a roundtrip to diagnose how many roundtrips it takes to ack a value!
;; (RoundTrip (RoundTrip :foo)) ; two roundtrips

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
                      (router/ReplaceState! ['. (conj (into [] (take *depth) router/route)
                                                  (assoc (nth router/route *depth {}) ::selection symbolic-x))])
                      ;; Want the selected row to transition atomically? : (RoundTrip (RoundTrip [::forms/ok]))
                      ;; Two roundtrids because:
                      ;;  - one roundtrip because TablePicker! receives the selected index from server (latency)
                      ;;  - second roundtrip because of an Electric bug leo is fixing
                      ;;     - Electric produces an extra roundtrip on e/fn call.
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
          (QueryMonitor)
          (binding [!mode (atom default-mode)]
            (let [mode (e/watch !mode)]
              (binding [*mode mode #_(ModePicker mode)
                        *depth 1]
                (let [[query] router/route]
                  (rebooting query
                    (if-not query
                      (router/ReplaceState! ['. default])
                      (let [f$ (first query)
                            o (e/server (Timing (pretty-title query) #(query->object *hfql-bindings query)))]
                        (set! (.-title js/document) (str (some-> f$ name (str " – ")) "Hyperfiddle"))
                        (dom/props {:class (cssx/css-slugify f$)})
                        (Block query o (e/server (find-sitemap-spec sitemap f$)))))))))))))))

(def table-block-css
"
.dustingetz-entity-browser4__block table { display: grid; grid-template-columns: repeat(var(--column-count), 1fr);  grid-template-rows: var(--row-height);}

.dustingetz-entity-browser4__block table thead { display: contents; }
.dustingetz-entity-browser4__block table thead tr { display: grid; grid-row: 1; grid-column: 1 / -1; grid-template-columns: subgrid;}
.dustingetz-entity-browser4__block table thead tr th { white-space: nowrap; text-overflow: ellipsis; overflow: hidden; }

.dustingetz-entity-browser4__block .hyperfiddle-electric-forms5__table-picker { grid-row: 2; grid-column: 1 / -1; grid-template-columns: subgrid; }

"
  )

(def css
  (str forms/css
    table-block-css

    "
/* cosmetic defaults */
.dustingetz-entity-browser4__block legend .title {font-weight:600;}
.dustingetz-entity-browser4__block { padding: 0; background-color: white; }
.dustingetz-entity-browser4__block table { border: 0px solid #f2f2f2; border-top-left-radius: 0rem; border-top-right-radius: 0rem; }
.dustingetz-entity-browser4__block table thead tr { background-color: #f2f2f2; border-bottom: 1px lightgray solid; }
.dustingetz-entity-browser4__block table thead tr th { white-space: nowrap; text-overflow: ellipsis; overflow: hidden; }
.dustingetz-entity-browser4__block table thead tr th { font-weight: 500; }
.dustingetz-entity-browser4__block table thead tr th:not(:first-child) { border-left: 1px lightgray solid; }
.dustingetz-entity-browser4__block table :is(th, td) { padding: 0 0.25em; }
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

#?(:clj
   (defn normalize-sitemap [ns sitemap]
     (let [qualify #(symbol (hfql/resolve! % ns))]
       (update-keys sitemap
         (fn [k]
           (if (symbol? k)
             (seq (list (qualify k)))
             (cons (qualify (first k)) (next k))))))))

#?(:clj (defn qualify-sitemap-symbol [ns s]
          (if (qualified-symbol? s)
            (symbol (hfql/resolve! s ns))
            (cond (hfql/field-access? s)  s
                  (hfql/method-access? s) s
                  (#{'% '%v} s)           s
                  :else                   (symbol (hfql/resolve! s ns))))))
#?(:clj
   (defn auto-resolves [ns]             ; to resolve ::keywords based on the caller ns
     (as-> (ns-aliases ns) $
       (assoc $ :current (ns-name ns), 'hfql 'peternagy.hfql)
       (zipmap (keys $)
         (map ns-name (vals $))))))

#?(:clj (defn read-sitemap [resource-path ns]
          (binding [*ns* ns]
            (->> (eda/parse-string (slurp (io/resource resource-path)) {:auto-resolve (auto-resolves ns)})
              (walk/postwalk (fn [x] (cond
                                       (symbol? x)                              (qualify-sitemap-symbol ns x)
                                       (and (seq? x) (= `hfql/props (first x))) (apply hfql/props (next x))
                                       :else                                    x)))
              (normalize-sitemap ns)))))

;; #?(:clj (defn sitemap-incseq [resource-path ns]
;;           (let [f (io/file (io/resource resource-path))]
;;             (->> (m/ap
;;                    (let [f (m/?> (fw/watch-file f))]
;;                      (m/? (m/via m/blk (read-sitemap f ns)))))
;;               (e/flow->incseq)))))

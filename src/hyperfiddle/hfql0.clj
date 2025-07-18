(ns hyperfiddle.hfql0
  (:refer-clojure :exclude [resolve])
  (:import [java.io Writer])
  (:require [clojure.core.protocols :as ccp]
            [clojure.datafy :refer [nav]]
            [geoffreygaillard.predicates :as pred]
            [clojure.string :as str]
            [hyperfiddle.rcf :refer [tests]]
            [clojure.tools.logging :as log]
            [clojure.walk :as walk]
            [clojure.java.io :as io]
            [edamame.core :as edn]
            [clojure.spec.alpha :as s]))

(defprotocol Identifiable :extend-via-metadata true
  (-identify [o])) ; local-symbolize

(extend-protocol Identifiable
  Object (-identify [_]) ;#?(:clj Object :cljs default)
  nil (-identify [_]))

(defn identifiable [identify-fn object]
  (vary-meta object assoc `-identify (bound-fn* identify-fn))) ; #?(:clj bound-fn* :cljs identity)

(defn identify [obj] "
Produces a symbolic identity for `obj`, or nil if `obj` does not explicitly
implement `Identifiable`. Serializable and uniquely resolvable symbolic
identities should be favored.
All values being self-identical, `(or (identify x) x)` always yields a valid
identifier, though it might not be serializable."
  (-identify obj))

(comment
  (identify (Object.)) := nil
  (let [obj (Object.)] (pr-str (or (identify obj) obj))) := "#object[java.lang.Object 0x75bf525d \"java.lang.Object@75bf525d\"]"
  (identify nil) := nil
  (identify (identifiable (constantly `hash-map) {})) := `hash-map)

(defn identifiable? [object]
  (extends? Identifiable (type object))
  ;; #?(:clj (extends? Identifiable object)
  ;;    :cljs (and (not (nil? x))
  ;;            (not= js/Object (type object))
  ;;            (satisfies? Identifiable object)))
  )

;(def ^:dynamic *hierarchy* #?(:clj @#'clojure.core/global-hierarchy :cljs (cljs.core/get-global-hierarchy)))
(def ^:dynamic *hierarchy* @#'clojure.core/global-hierarchy)

(defmulti -resolve (fn ([object] (type object)) ([object opts] (type object))) :hierarchy #'*hierarchy*)
(defmethod -resolve :default [_object & _opts] nil)

(defn resolve ; todo resolve_, i.e. don't shadow core. Is this private?
  ([object] (-resolve object))
  ([object {::keys [hierarchy] :as opts}]
   (binding [*hierarchy* (or hierarchy *hierarchy*)]
     (-resolve object (dissoc opts ::hierarchy)))))

(defprotocol NavContext
  (-nav-context [o]))

(extend-protocol NavContext
  nil (-nav-context [_])
  ; #?(:clj Object, :cljs default)
  Object (-nav-context [_] {})) ; https://github.com/clojure/clojurescript/blob/26dea31110502ae718399868ad7385ffb8efe6f6/src/main/cljs/clojure/datafy.cljs#L25-L27

(defn nav-context "
Provide an opportunity to compute (meta (datafy x)) without touching all attributes. User
implementation should respect (= (nav-context x) (meta (datafy x))). Use case: hf-pull produces
navigable pulled maps, without touching all attributes."
  [x]
  (some-> (-nav-context x)
    (assoc :clojure.datafy/obj x)
    (assoc :clojure.datafy/class (-> x class .getName symbol)))) ; #?(:clj)

(defn navigable-indexed [nav-fn collection]
  (vary-meta collection assoc `ccp/nav (bound-fn [_coll key value] (nav-fn key value)))) ; #?(:clj bound-fn :cljs fn)

(defn navigable [nav-fn collection] (navigable-indexed (fn [_index value] (nav-fn value)) collection))

(comment
  (nav-context nil) := nil
  (nav-context java.lang.String)
  := {:clojure.datafy/obj java.lang.String, :clojure.datafy/class java.lang.Class})

(comment "insight: nav on dehydrated collection with nil key can be used to hydrate an object in context"
  (require '[clojure.datafy :refer [nav]] '[dustingetz.mbrainz :refer [test-db lennon]])
  #_(clojure.repl/doc nav) ; big idea: k is optional actually!
  ; nav can use k if it helps you enrich the object but you don't have to!
  ; G: I think it was a mistake for Rich to make nav look like get and get-in
  (def xs (with-meta [123 124 lennon] ; attach polymorphic context to the resultset not the element
            {`clojure.core.protocols/nav ; polymorphic not by type but by meta
             (fn [xs k v] (d/entity @test-db v))}))
  "nav can resolve a hydrated object from a dehydrated resultset"
  (nav xs nil lennon) := (d/entity @test-db lennon))

(defprotocol Suggestable :extend-via-metadata true
  (-suggest [o]))

(defn suggest [o] (-suggest o))

(defn simplify-call-pattern [call-pattern]
  {:pre [(seq? call-pattern)]}
  ; (foo %) -> foo
  (if (and (= 2 (count call-pattern))
        (= '% (second call-pattern)))
    (first call-pattern)
    call-pattern))

(defn simplify-pull-pattern [pull-pattern]
  (cond (seq? pull-pattern) (simplify-call-pattern pull-pattern)
        :else pull-pattern))

(defn suggest-fields [^Class clazz]
  (into []
    (eduction
      (keep
        (fn [^java.lang.reflect.Field fld]
          (when-not (java.lang.reflect.Modifier/isStatic (.getModifiers fld))
            (let [nm (.getName fld)]
              (list (symbol (str ".-" nm)) (symbol "%"))))))
      (map simplify-pull-pattern)
      (.getFields clazz))))

(defn suggest-methods [^Class clazz]
  (into []
    (eduction (map
                (fn [^java.lang.reflect.Method meth]
                  (let [nm (.getName meth), arg-count (.getParameterCount meth)
                        stub-arg* (mapv #(symbol (str "arg" (inc %))) (range arg-count))]
                    (list* (symbol (str "." nm)) (symbol "%") stub-arg*))))
      (map simplify-pull-pattern)
      (.getMethods clazz))))

(defn suggest-java-class-members [java-object]
  (let [clazz (class java-object)]
    (into (suggest-fields clazz) (suggest-methods clazz))))

(comment
  (suggest-fields (class (props :k {})))
  (suggest-methods (class (props :k {})))
  (def xs (suggest-java-class-members (clojure.java.io/file "./")))
  (->> xs (filter symbol?) (sort-by name) (take 3))
  := ['.canExecute '.canRead '.canWrite])

(extend-protocol Suggestable
  clojure.lang.IPersistentMap
  (-suggest [m] (keys m))
  Object (-suggest [o] (suggest-fields (class o)))
  nil (-suggest [_]))

(defprotocol Viewer
  (-view [_ _o])
  (-unwrap [_])
  (-opts [_]))

(defn view [k o] (-view k o))
(defn unwrap [k] (-unwrap k))
(defn opts [k] (-opts k))

(extend-protocol Viewer
  Object
  (-view [k o] (get o k))
  (-unwrap [k] k)
  (-opts [_])
  nil
  (-view [_ o] (throw (ex-info "Cannot view on nil" {:o o})))
  (-unwrap [_] nil #_(throw (ex-info "Cannot unwrap nil" {})))
  (-opts [_]))

(deftype Props [k opts]
  Viewer
  (-view [_ o] (-view k o))
  (-unwrap [_] k)
  (-opts [_] opts))

(defmethod print-method Props [^Props props ^Writer w]
  (.write w "#Props[")
  (print-method (.-k props) w)
  (.write w " ")
  (print-method (.-opts props) w)
  (.write w "]"))

(defn props [k opts] (->Props k opts))
(defn props-update-k [props_ f]
  (let [raw-props (unwrap props_)]
    (cond-> (f raw-props) (not (identical? props_ raw-props)) (props (opts props_)))))
(defn props-update-opts [props_ f]
  (let [raw-props (unwrap props_)]
    (if (identical? raw-props props_)
      props_
      (props raw-props (f (or (opts props_) {}))))))

(defn resolve!
  ([f$] (or (clojure.core/resolve f$) (throw (ex-info (str "Failed to resolve " f$) {}))))
  ([ns f$] (if (qualified-symbol? f$)
             (or (ns-resolve ns f$) (requiring-resolve f$))
             (or (ns-resolve ns f$) (throw (ex-info (str "Failed to resolve " f$) {}))))))

(defn invoke-reflective [method$ o & args]
  (clojure.lang.Reflector/invokeInstanceMethod o (subs (str method$) 1) (into-array args)))

(defn read-reflective [field$ o]
  (let [fld (.getField (class o) (subs (str field$) 2))]
    (.get fld o)))

(defn field-access? [f$] (str/starts-with? (str f$) ".-"))
(defn method-access? [f$] (str/starts-with? (str f$) "."))

(defn pull-view [scope viewer]
  (let [k (unwrap viewer), o (get scope '%)]
    (cond
      (symbol? k) (let [f$ k]
                    (cond (= '. f$) (get o f$)
                          (= '.. f$) (get o f$)
                          (field-access? f$) (read-reflective f$ o)
                          (method-access? f$) (invoke-reflective f$ o)
                          :else (or (let [v (get o f$ ::not-found)]
                                      (when (not= v ::not-found) v))
                                  (let [resolved (resolve! f$)]
                                    (when (var? resolved) (resolved o))))))
      (seq? k) (let [[f$ & args] (replace scope k)]
                 (cond (field-access? f$) (read-reflective f$ (first args))
                       (method-access? f$) (apply invoke-reflective f$ args)
                       :else (apply (resolve! f$) args)))
      (map? k) (let [[k k2] (first k), v (pull-view scope k)]
                 (pull-view (assoc scope '% (nav o k v)) k2))
      :else (view viewer o))))

(defn pull-object [scope spec]
  (with-meta
    (reduce (fn self [ac viewer] (assoc ac (unwrap viewer) (pull-view scope viewer))) {} spec)
    {::origin (get scope '%)}))

(defn pull [bindings spec o]
  {:pre [(or (nil? bindings) (map? bindings))]}
  (try
    (with-bindings bindings
      (if (sequential? o)
        (mapv (fn [x] (pull-object {'% x} spec)) o)
        (pull-object {'% o} spec)))
    (catch Throwable t
      (log/error t "Failed to pull " {:spec spec, :object o}))))

(def ^:dynamic *test* nil)
(defn test-times [n] (* *test* n))
(tests
  (pull {} [:a :b] {:a 1 :b 2}) := {:a 1, :b 2}
  (pull {} [:a :b] [{:a 1 :b 2}]) := [{:a 1, :b 2}]
  (pull {} [(props :a {}) :b] {:a 1 :b 2}) := {:a 1, :b 2}
  (pull {} `[(inc ~'%)] 1) := {`(inc ~'%) 2}
  (pull {#'*test* 10} `[(test-times ~'%)] 2) := {`(test-times ~'%) 20}
  (pull {} `[(inc ~'%)] [1 2]) := `[{(inc ~'%) 2} {(inc ~'%) 3}]
  (pull {} `[(.get ~'% :a)] {:a 1}) := `{(.get ~'% :a) 1}
  (pull {} `[(.get ~'% :a)] [{:a 1} {:b 2}]) := `[{(.get ~'% :a) 1} {(.get ~'% :a) nil}]
  (pull {} `[(.-width ~'%)] (new java.awt.Rectangle 10 20 30 40)) := `{(.-width ~'%) 30}
  (pull {} `[inc] 41) := `{inc 42}
  (pull {} `[{:foo inc}] {:foo 41, :bar 0}) := `{{:foo inc} 42}
  (pull {} '[{.-x inc}] (new java.awt.Point 41 2)) := '{{.-x inc} 42}
  (let [data-with-nav (with-meta {:a 1, :b 2} {`ccp/nav (fn [_this k v] (if (= k :a) {:db/ident 42} v))})]
    (pull {} [{:a :db/ident} :b] data-with-nav)) := {{:a :db/ident} 42, :b 2}
  (pull {} '[Number] {'Number Number}) := {'Number Number}
  (pull {} '[+] {'+ #'+}) := {'+ #'+}
  (pull {} '[..] {'.. :foo}) := {'.. :foo}
  )


(comment
  (.get {:a 1} :a)
  (clojure.lang.Reflector/invokeInstanceMethodOfClass {:a 1} (class {:a 1}) "get" (into-array [:a]))
  (invoke-reflective {:a 1} '.get :a)
  )

;;;;;;;;;;;;;
;; SITEMAP ;;
;;;;;;;;;;;;;

(s/def ::sitemap-key (s/or :symbol symbol? :call seq?))
;; in principle it should be `vector?` as in `[:db/id :db/ident]`. But
;; `'(:db/id :db/ident)` is fine too because it's not ambiguous. Also makes
;; Suggestable api nicer: users won't have to remember to cast to vector when
;; dynamically generating pullspecs. (e.g. `(concat colsA colsB)`)
(s/def ::pull-spec #(sequential? (unwrap %)))
(s/def ::sitemap (s/map-of ::sitemap-key ::pull-spec))

(defn normalize-sitemap-key [ns sitemap-key]
  {:pre [(simple-symbol? ns) (s/assert ::sitemap-key sitemap-key)]
   :post [(s/assert ::sitemap-key %)]}
  (let [qualify #(symbol (resolve! ns %))]
    (cond
      (= '. sitemap-key) '.
      (symbol? sitemap-key) (seq (list (qualify sitemap-key)))
      :else (cons (qualify (first sitemap-key)) (next sitemap-key)))))

;; (normalize-sitemap-key 'dustingetz.file-explorer '.getName)

;; (parse-sitemap* 'dustingetz.file-explorer '[.getName file-kind])

(defn normalize-sitemap [ns site-map]
  {:pre [(simple-symbol? ns) (s/assert ::sitemap site-map)]
   :post [(s/assert ::sitemap %)]}
  (update-keys site-map (partial normalize-sitemap-key ns)))

(defn qualify-sitemap-symbol [ns sym]
  {:pre [(simple-symbol? ns) (symbol? sym)]
   :post [(symbol? %)]}
  (let [resolve! (partial resolve! ns)]
    (cond
      (qualified-symbol? sym)   (symbol (resolve! sym))
      (field-access? sym)  sym
      (method-access? sym) sym
      (#{'% '%v '.} sym)        sym
      :else                     (symbol (resolve! sym)))))

(s/def ::edamame-ns-alias (s/or :alias simple-symbol? :current-ns #{:current}))
(s/def ::edamame-sitemap-ns-aliases (s/and (s/map-of ::edamame-ns-alias simple-symbol?)
                              #(contains? % :current)  ; intentional – see `edamame.core/parse-string`
                              #(contains? % 'hfql) ; sitemap sugar
                              ))

(defn edamame-auto-resolves
  "Produces an alias -> ns-name map to pass to edamame/parse-string :auto-resolves option"
  [ns] ; to resolve ::keywords based on the caller ns
  {:pre [(pred/namespace? ns)]
   :post [(s/assert ::edamame-sitemap-ns-aliases %)]}
  (-> {'hfql 'hyperfiddle.hfql0} ; sugar
    (merge (ns-aliases ns)) ; {'alias #ns[aliased]}
    (update-vals ns-name) ; {'alias 'aliased}
    (assoc :current (ns-name ns)) ; edamame interprets :current to auto-resolve ::foo and `foo
    ))

(defn- parse-sitemap* [ns-sym form]
  (walk/postwalk (fn [x] (cond
                           (symbol? x) (qualify-sitemap-symbol ns-sym x)
                           (and (seq? x) (= `props (first x))) (apply props (next x))
                           :else x))
    form))

(defn parse-sitemap
  [ns-sym quoted-site-map]
  {:pre [(simple-symbol? ns-sym) (s/assert ::sitemap quoted-site-map)]
   :post [(s/assert ::sitemap %)]}
  (->> (parse-sitemap* ns-sym quoted-site-map)
    (normalize-sitemap ns-sym)))

(defn- current-ns [&env]
  (or (some-> &env :ns :name) ; cljs
    (ns-name *ns*)) ; clj
  )

(defn pull-spec* [ns-sym pull-spec] (parse-sitemap* ns-sym pull-spec))

(defmacro pull-spec
  ([pull-spec] `(pull-spec ~(current-ns &env) ~pull-spec))
  ([ns-sym pull-spec]
   {:pre [(simple-symbol? ns-sym) (s/assert ::pull-spec pull-spec)]}
   `'~(pull-spec* ns-sym pull-spec)))

(defmacro sitemap
  ([site-map] `(sitemap ~(current-ns &env) ~site-map))
  ([ns-sym site-map]
   {:pre [(simple-symbol? ns-sym) (s/assert ::sitemap site-map)]}
   [ns-sym site-map]
   `'~(parse-sitemap ns-sym site-map)))

(defn read-sitemap [ns-sym resource-path]
  {:pre [(simple-symbol? ns-sym)]
   :post [(s/assert ::sitemap %)]}
  (parse-sitemap ns-sym
    (edn/parse-string (slurp (io/resource resource-path)) {:auto-resolve (edamame-auto-resolves (find-ns ns-sym))})))

;; #?(:clj (defn sitemap-incseq [resource-path ns]
;;           (let [f (io/file (io/resource resource-path))]
;;             (->> (m/ap
;;                    (let [f (m/?> (fw/watch-file f))]
;;                      (m/? (m/via m/blk (read-sitemap ns f)))))
;;               (e/flow->incseq)))))



(ns hyperfiddle.sitemap
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.hfql0 #?(:clj :as :cljs :as-alias) hfql]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.router4 :as r]
   [contrib.data :refer [namespace?]]
   [clojure.walk :as walk]
   #?(:clj [clojure.java.io :as io])
   [edamame.core :as edn]
   [clojure.spec.alpha :as s]
   )
  #?(:cljs (:require-macros [hyperfiddle.sitemap])))


(s/def ::sitemap-key (s/or :symbol symbol? :call seq?))
;; in principle it should be `vector?` as in `[:db/id :db/ident]`. But
;; `'(:db/id :db/ident)` is fine too because it's not ambiguous. Also makes
;; Suggestable api nicer: users won't have to remember to cast to vector when
;; dynamically generating pullspecs. (e.g. `(concat colsA colsB)`)
(s/def ::pull-spec sequential?)
(s/def ::sitemap (s/map-of ::sitemap-key ::pull-spec))

#?(:clj
   (defn normalize-sitemap-key [ns sitemap-key]
     {:pre [(simple-symbol? ns) (s/assert ::sitemap-key sitemap-key)]
      :post [(s/assert ::sitemap-key %)]}
     (let [qualify #(symbol (hfql/resolve! ns %))]
       (cond
         (= '. sitemap-key) '.
         (symbol? sitemap-key) (seq (list (qualify sitemap-key)))
         :else (cons (qualify (first sitemap-key)) (next sitemap-key))))))

;; (normalize-sitemap-key 'dustingetz.file-explorer '.getName)

;; (parse-sitemap* 'dustingetz.file-explorer '[.getName file-kind])

#?(:clj
   (defn normalize-sitemap [ns site-map]
     {:pre [(simple-symbol? ns) (s/assert ::sitemap site-map)]
      :post [(s/assert ::sitemap %)]}
     (update-keys site-map (partial normalize-sitemap-key ns))))

#?(:clj (defn qualify-sitemap-symbol [ns sym]
          {:pre [(simple-symbol? ns) (symbol? sym)]
           :post [(symbol? %)]}
          (let [resolve! (partial hfql/resolve! ns)]
            (cond
              (qualified-symbol? sym)   (symbol (resolve! sym))
              (hfql/field-access? sym)  sym
              (hfql/method-access? sym) sym
              (#{'% '%v '.} sym)        sym
              :else                     (symbol (resolve! sym))))))

(s/def ::edamame-ns-alias (s/or :alias simple-symbol? :current-ns #{:current}))
(s/def ::edamame-sitemap-ns-aliases (s/and (s/map-of ::edamame-ns-alias simple-symbol?)
                              #(contains? % :current)  ; intentional – see `edamame.core/parse-string`
                              #(contains? % 'hfql) ; sitemap sugar
                              ))

#?(:clj
   (defn edamame-auto-resolves
     "Produces an alias -> ns-name map to pass to edamame/parse-string :auto-resolves option"
     [ns] ; to resolve ::keywords based on the caller ns
     {:pre [(namespace? ns)]
      :post [(s/assert ::edamame-sitemap-ns-aliases %)]}
     (-> {'hfql 'hyperfiddle.hfql0} ; sugar
       (merge (ns-aliases ns)) ; {'alias #ns[aliased]}
       (update-vals ns-name) ; {'alias 'aliased}
       (assoc :current (ns-name ns)) ; edamame interprets :current to auto-resolve ::foo and `foo
       )))

#?(:clj
   (defn- parse-sitemap* [ns-sym form]
     (walk/postwalk (fn [x] (cond
                              (symbol? x)                              (qualify-sitemap-symbol ns-sym x)
                              (and (seq? x) (= `hfql/props (first x))) (apply hfql/props (next x))
                              :else                                    x))
       form)))

#?(:clj
   (defn parse-sitemap
     [ns-sym quoted-site-map]
     {:pre [(simple-symbol? ns-sym) (s/assert ::sitemap quoted-site-map)]
      :post [(s/assert ::sitemap %)]}
     (->> (parse-sitemap* ns-sym quoted-site-map)
       (normalize-sitemap ns-sym))))

(defn- current-ns [&env]
  (or (some-> &env :ns :name) ; cljs
    (ns-name *ns*)) ; clj
  )

#?(:clj (defn pull-spec* [ns-sym pull-spec] (parse-sitemap* ns-sym pull-spec)))

(defmacro pull-spec
  ([pull-spec] `(hyperfiddle.sitemap/pull-spec ~(current-ns &env) ~pull-spec))
  ([ns-sym pull-spec]
   {:pre [(simple-symbol? ns-sym) (s/assert ::pull-spec pull-spec)]}
   `'~(pull-spec* ns-sym pull-spec)))

(defmacro sitemap
  ([site-map] `(sitemap ~(current-ns &env) ~site-map))
  ([ns-sym site-map]
   {:pre [(simple-symbol? ns-sym) (s/assert ::sitemap site-map)]}
   [ns-sym site-map]
   `'~(parse-sitemap ns-sym site-map)))

#?(:clj (defn read-sitemap [ns-sym resource-path]
          {:pre [(simple-symbol? ns-sym)]
           :post [(s/assert ::sitemap %)]}
          (parse-sitemap ns-sym
            (edn/parse-string (slurp (io/resource resource-path)) {:auto-resolve (edamame-auto-resolves (find-ns ns-sym))}))))

;; #?(:clj (defn sitemap-incseq [resource-path ns]
;;           (let [f (io/file (io/resource resource-path))]
;;             (->> (m/ap
;;                    (let [f (m/?> (fw/watch-file f))]
;;                      (m/? (m/via m/blk (read-sitemap ns f)))))
;;               (e/flow->incseq)))))


;;; UI

(defn find-context-free-pages [sitemap]
  (sort-by first (filterv #(not (next %)) (keys sitemap))))

(e/defn Index [sitemap]
  (dom/nav
    (dom/props {:class "Index"})
    (dom/text "Nav:")
    (e/for [view (e/diff-by {} (e/server (find-context-free-pages sitemap)))]
      (dom/text " ") (r/link ['. [view]] (dom/text (name (first view)))))))
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
   ))

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
       (assoc $ :current (ns-name ns), 'hfql 'hyperfiddle.hfql0)
       (zipmap (keys $)
         (map ns-name (vals $))))))

#?(:clj
   (defn parse-sitemap
     ([quoted-site-map] (parse-sitemap *ns* quoted-site-map))
     ([ns-or-ns-sym quoted-site-map]
      {:pre [(or (simple-symbol? ns-or-ns-sym) (namespace? ns-or-ns-sym))
             (map? quoted-site-map)]
       :post [(map? %)]}
      (binding [*ns* (find-ns (ns-name ns-or-ns-sym))] ; resolve sym to ns (throws if not found), ns object passes through, noop if ns = *ns*
        (->> quoted-site-map
          (walk/postwalk (fn [x] (cond
                                   (symbol? x)                              (qualify-sitemap-symbol *ns* x)
                                   (and (seq? x) (= `hfql/props (first x))) (apply hfql/props (next x))
                                   :else                                    x)))
          (normalize-sitemap *ns*))))))

#?(:clj (defn read-sitemap [ns-or-ns-sym resource-path]
          (binding [*ns* (find-ns (ns-name ns-or-ns-sym))] ; resolve sym to ns (throws if not found), ns object passes through
            (parse-sitemap *ns*
              (edn/parse-string (slurp (io/resource resource-path)) {:auto-resolve (auto-resolves *ns*)})))))



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
      (dom/text " ") (r/link ['. [view]] (dom/text (name (first view)))))
    (dom/text " — Datomic Browser")))
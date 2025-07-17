(ns hyperfiddle.entrypoint
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.router4 :as r]
            #?(:clj [hyperfiddle.hfql0 :as hfql])))

(e/declare apps)

#?(:clj (defn find-context-free-entries [sitemap] ; TODO move to hfql
          (->> sitemap
            keys
            (filter (fn [[fsym & _]] ; assumes a normalized sitemap (parsed by hfql/sitemap)
                      (and (qualified-symbol? fsym)
                        (when-let [?var (try (hfql/resolve! fsym) (catch Throwable t (prn t) nil))]
                          (some #{[]} (:arglists (meta ?var))))))))))

#?(:clj (defn build-nav-links [sitemap defaults]
          (->> (find-context-free-entries sitemap)
            (sort-by first)
            (concat defaults)
            (distinct))))

(e/defn Index [sitemap defaults]
  (dom/nav
    (dom/props {:class "Index hyperfiddle-entrypoint-Index"})
    (dom/text "Nav:")
    (e/for [view (e/diff-by {} (e/server (build-nav-links sitemap defaults)))]
      (dom/text " ") (r/link ['. [view]] (dom/text (name (first view)))))))

(e/defn IndexPage []
  (dom/h1 (dom/text "Hyperfiddle"))
  (dom/menu
    (e/for [[app-name App] (e/diff-by key (dissoc apps `Index))]
      (dom/li (r/link ['/ [app-name]] (dom/text (name app-name)))))))

(e/defn NotFoundPage [& args]
  (e/client
    (dom/h1 (dom/text "Page not found"))
    (r/link ['/ []] (dom/text "index"))))

(e/defn DefaultApps []
  {`Index IndexPage})

(defmacro rebooting [sym & body] `(e/for [~sym (e/diff-by identity (e/as-vec ~sym))] ~@body)) ; TODO remove

(e/defn Hyperfiddle [user-apps & {:keys [default] :or {default `(Index)}}]
  (r/router (r/HTML5-History)
    (let [[app-name & _] r/route]
      (if-not app-name
        (r/ReplaceState! ['. default])
        (let [apps (merge (DefaultApps) user-apps)
              App (get apps app-name NotFoundPage)]
          (set! (.-title js/document) (str (some-> app-name name (str " – ")) "Hyperfiddle"))
          (binding [hyperfiddle.entrypoint/apps apps]
            (rebooting App ; FIXME navigation can crash
              (r/pop (App)))))))))


(ns hyperfiddle.entrypoint2
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.router4 :as r]
            [hyperfiddle.hfql1 :as hfql]))

(e/declare apps)

#?(:clj (defn build-nav-links [sitemap defaults]
          (->> (hfql/context-free-edges sitemap)
            (map hfql/form)
            ;; (map hfql/edge-name)
            (sort-by hfql/edge-name)
            (concat defaults)
            (distinct)
            )))

(e/defn Index [sitemap defaults]
  (dom/nav
    (dom/props {:class "Index hyperfiddle-entrypoint-Index"})
    (dom/text "Nav:")
    (e/for [symbolic-edge  (e/diff-by {} (e/server (build-nav-links sitemap defaults)))]
      (dom/text " ") (r/link ['. [symbolic-edge]] (dom/text symbolic-edge)))))

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

; this is ElectricFiddle not Hyperfiddle
(e/defn Hyperfiddle [user-apps & {:keys [default] :or {default `(Index)}}]
  (r/router (r/HTML5-History)
    (let [[app-name & _] r/route]
      (if-not app-name
        (r/ReplaceState! ['. default])
        (let [apps (merge (DefaultApps) user-apps)
              App (get apps app-name NotFoundPage)]
          (set! (.-title js/document) (str (some-> app-name name (str " – ")) "Hyperfiddle"))
          (binding [hyperfiddle.entrypoint2/apps apps]
            (rebooting App ; FIXME navigation can crash
              (r/pop (App)))))))))


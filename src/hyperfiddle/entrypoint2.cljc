(ns hyperfiddle.entrypoint2
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.router5 :as r]
            [hyperfiddle.hfql2 :as hfql]
            [contrib.data :refer [filter-vals]]
            [hyperfiddle.navigator6.utils :refer [rebooting freezing]]))

(e/declare apps)

#?(:clj
   (defn build-nav-links [sitemap defaults]
     (->> sitemap
       (filter-vals hfql/fullfilled?)
       (keys)
       (sort)
       (concat defaults)
       (distinct))))

(e/defn Index [sitemapT defaults]
  (dom/nav
    (dom/props {:class "Index hyperfiddle-entrypoint-Index"})
    (dom/text "Nav:")
    (e/for [symbolic-edge  (e/diff-by {} (e/server (build-nav-links sitemapT defaults)))]
      (dom/text " ") (r/link ['. [symbolic-edge]]
                       (dom/props {:title (pr-str symbolic-edge)})
                       (dom/text symbolic-edge #_(name (hfql/edge-name symbolic-edge)))))))

(e/defn IndexPage []
  (dom/h1 (dom/text "Hyperfiddle"))
  (dom/menu
    (e/for [[app-identifier App] (e/diff-by key (dissoc apps `Index))]
      (dom/li (r/link ['/ [app-identifier]] (dom/text (name app-identifier)))))))

(e/defn NotFoundPage [& args]
  (e/client
    (dom/h1 (dom/text "Page not found"))
    (r/link ['/ []] (dom/text "index"))))

(e/defn DefaultApps []
  {`Index IndexPage})

(defn normalize-page-identifier [identifier]
  (if (sequential? identifier)
    identifier
    (list identifier)))

; this is ElectricFiddle not Hyperfiddle
(e/defn Hyperfiddle [user-apps & {:keys [default] :or {default `(Index)}}]
  (r/router (r/HTML5-History)
    (let [[app-identifier & _] r/route]
      (if-not app-identifier
        (r/ReplaceState! ['. default])
        (let [[app & args] (normalize-page-identifier app-identifier)
              apps (merge (DefaultApps) user-apps)
              App (get apps app NotFoundPage)]
          (set! (.-title js/document) (str (some-> app name (str " – ")) "Hyperfiddle"))
          (binding [hyperfiddle.entrypoint2/apps apps]
            (rebooting app ; FIXME navigation can crash
              (freezing [app App]
                (r/pop (e/Reconcile (e/apply App args)))))))))))


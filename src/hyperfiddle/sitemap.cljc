(ns hyperfiddle.sitemap
  (:require [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.router4 :as r]))

(defn find-context-free-pages [sitemap]
  (sort-by first (filterv #(not (next %)) (keys sitemap))))

(e/defn Index [sitemap]
  (dom/nav
    (dom/props {:class "Index"})
    (dom/text "Nav:")
    (e/for [view (e/diff-by {} (e/server (find-context-free-pages sitemap)))]
      (dom/text " ") (r/link ['. [view]] (dom/text (name (first view)))))
    (dom/text " — Datomic Browser")))
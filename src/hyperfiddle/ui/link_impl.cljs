(ns hyperfiddle.ui.link-impl
  (:require
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment]]
    [hypercrud.browser.link :as link]
    [hypercrud.ui.connection-color :refer [border-color]]))


(defn options-link? [link]
  ; don't care if its inline or not, just do the right thing.
  (= :options (:link/rel link)))

(def options-processor (partial remove options-link?))

(defn options-links [path links]
  (->> links
       (filter options-link?)
       (filter (partial link/draw-link? path))))

(defn draw-options? [path links]
  (not (empty? (options-links path links))))

(defn contextual-links [path embed links ?processor]
  (->> (reduce (fn [links f] (f links)) @links (if ?processor [?processor]))
       ((if embed filter remove) (fn [link]
                                   (and (not (link/popover-link? link))
                                        (:link/render-inline? link))))
       ; path filtering is the most expensive, do it last
       (filter (partial link/draw-link? path))
       vec))

; todo why does this not just happen inside hyperfiddle.ui/ui-from-link
(defn- wrap-with-styles [ctx child]
  (if (not= :hyperfiddle.ui.layout/table (:hyperfiddle.ui/layout ctx))
    [:div {:style {:border-color (border-color ctx)}} child]
    child))

(defn anchors [path props ctx & [?processor]]
  (->> (r/track contextual-links path false (:hypercrud.browser/links ctx) ?processor)
       (r/unsequence :db/id)
       (map (fn [[link-ref link-id]]
              ^{:key (hash link-id)}
              [wrap-with-styles ctx
               [hyperfiddle.ui/ui-from-link link-ref ctx props]]))
       (doall)
       (apply fragment)))

(defn iframes [path props ctx & [?processor]]
  (->> (r/track contextual-links path true (:hypercrud.browser/links ctx) ?processor)
       (r/unsequence :db/id)
       (map (fn [[link-ref link-id]]
              ^{:key (hash link-id)}
              [wrap-with-styles ctx
               [hyperfiddle.ui/ui-from-link link-ref ctx props]]))
       (doall)
       (apply fragment)))
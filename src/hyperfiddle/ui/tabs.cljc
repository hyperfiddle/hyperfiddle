(ns hyperfiddle.ui.tabs
  #?(:cljs (:require-macros [hyperfiddle.ui.tabs]))
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.history :as history]
            [missionary.core :as m]))

(e/def !tab-ids)
(e/def tab-ids)
(e/def !tabs-list)
(e/def !selected)
(e/def selected)

(defmacro TabGroup [{::keys [selected] :or {selected nil}} & body]
  `(binding [!tab-ids   (atom {})
             !tabs-list (atom (list))
             !selected  (atom nil)]
     (binding [tab-ids (e/watch !tab-ids)]
       (binding [selected (or (e/watch !selected) (first (get tab-ids ~selected)) (first (e/watch !tabs-list)))]
         ~@body))))

#?(:cljs
   (defn child-list* [^js node]
     (m/relieve {}
       (m/observe (fn [!]
                    (! (or (array-seq (.-children node)) ()))
                    (let [obs (new js/MutationObserver (fn [^js mutation-list _observer]
                                                         (doseq [^js mutation (array-seq mutation-list)]
                                                           (case (.-type mutation)
                                                             "childList" (! (array-seq (.-children node)))))))]
                      (.observe obs node #js{:childList true})
                      #(.disconnect obs)))))))

(defmacro child-ids [] `(map (fn [^js node#] (.-id node#))
                          (new (child-list* dom/node))))

(defmacro TabList [& body]
  `(dom/div (dom/props {::dom/role "tablist"})
     (reset! !tabs-list (child-ids))
     ~@body))

(defmacro Tab [{:keys [key route]} & body]
  (let [body `(dom/button
                (let [tab-id#    (str (gensym "tab_"))
                      panel-id#  (str (gensym "tabpanel_"))
                      key#       ~key
                      selected?# (= tab-id# selected)]
                  (swap! !tab-ids assoc key# [tab-id# panel-id#])
                  (e/on-unmount #(swap! !tab-ids dissoc key#))
                  (dom/on! "click" #(reset! !selected tab-id#))
                  (dom/on! "keydown" (fn [^js event#]
                                       (when-let [^js sibling# (case (.-key event#)
                                                                 "ArrowLeft"  (.-previousSibling dom/node)
                                                                 "ArrowRight" (.-nextSibling dom/node)
                                                                 nil)]
                                         (.focus sibling#)
                                         (reset! !selected (.-id sibling#)))))
                  (dom/props {::dom/type          "button"
                              ::dom/role          "tab"
                              ::dom/id            tab-id#
                              ::dom/aria-selected selected?#
                              ::dom/tabindex      (if selected?# "0" "-1")
                              ::dom/aria-controls panel-id#}))
                ~@body)]
    (if route
      `(history/Link. ~route
         (e/fn* [] ~body))
      body)))

(defmacro TabPanel [{:keys [key preload?]} & body]
  `(let [key#                ~key
         [tab-id# panel-id#] (get tab-ids key#)
         selected?#          (= tab-id# selected)
         preload?#           ~preload?]
     (when (or selected?# preload?#)
       (dom/div
         (dom/props {::dom/id              panel-id#
                     ::dom/role            "tabpanel"
                     ::dom/tabindex        "0"
                     ::dom/aria-labelledby tab-id#
                     ::dom/aria-hidden     (not selected?#)})
         ~@body))))

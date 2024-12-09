(ns hyperfiddle.ui.context-menu
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom])
  #?(:cljs (:require-macros [hyperfiddle.ui.context-menu])))

(e/declare open?) ; bool
(e/declare mouse-coordinates)  ; [x y]
(e/declare open!) ; (fn [context event] …)
(e/declare close!) ; (fn [] …)
(e/declare context-menu?)
(e/declare context)

(e/defn Item [Body]
  (dom/li
    (Body)))

(defmacro item [& body]
  `(Item (e/fn [] ~@body)))

(e/defn Items [Body]
  (when (e/client open?)
    ;; (dom/div (dom/props {:class "absolute z-10 bg-red-500/10 w-full h-full"}))
    (dom/ul
      (if context-menu?
        (let [[left top] mouse-coordinates]
          (dom/props {:class "absolute"
                      :style {:left (str left "px")
                              :top  (str top "px")}})))
      (Body))))

(defmacro items [& body]
  `(Items (e/fn [] ~@body)))

(e/defn Menu [{::keys [open? context-menu?]} Body]
  (e/client ;; TODO v3: don't force Body to be called on client
    (let [!open?             (atom open?)
          !context           (atom nil)
          !mouse-coordinates (atom [0 0])]
      (binding [hyperfiddle.ui.context-menu/open?         (e/watch !open?)
                hyperfiddle.ui.context-menu/context-menu? context-menu?
                context           (e/watch !context)
                mouse-coordinates (e/watch !mouse-coordinates)]
        (binding [open!  (fn [context ^js event]
                           (.preventDefault event)
                           (reset! !mouse-coordinates [(.-pageX event) (.-pageY event)])
                           (reset! !context context)
                           (reset! !open? true))
                  close! (fn [] (reset! !open? false))]
          (Body))))))

(defmacro menu [{::keys [open? context-menu?] :as props} & body]
  `(Menu ~props (e/fn [] ~@body)))

(ns hyperfiddle.ui.disclosure
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]))

(e/declare open?)
(e/declare !open?)
(e/declare panel-id)
(e/declare preload?)
(e/declare keep-loaded?)
(e/declare !loaded?)
(e/declare loaded?)
(e/declare disabled?)

(defmacro disclosure [props & body]
  `(let [props# ~props]
     (binding [!open?       (atom (::open? props# false))
               preload?     (::preload? props# false)
               keep-loaded? (::keep-loaded? props# false)
               disabled?    (::disabled? props# false)
               panel-id     (str (gensym "disclosure-panel-"))
               !loaded?     (atom false)]
       (binding [open?   (e/watch !open?)
                 loaded? (e/watch !loaded?)]
         (dom/div (dom/props {:class "disclosure"})
                  ~@body)))))

(defmacro button [& body]
  `(dom/span (dom/props {:class         "disclosure-button"
                         :aria-expanded open?
                         :aria-controls panel-id})
     (when-not disabled?
       (dom/props {:role "button"})
       (dom/On "click" #(swap! !open? not) nil))
     ~@body))

(defmacro panel [{::keys [open?]} & body]
  (let [body `(dom/div (dom/props {:class "disclosure-panel"
                                   :id panel-id})
                       (when (not open?)
                         (dom/props {:class "soft-display-none"}))
                       ~@body)]
    (if (and open? (not disabled?)) ; hardcoded
      body
      `(when (or open? preload? loaded?)
         (when keep-loaded? (reset! !loaded? true))
         ~body))))

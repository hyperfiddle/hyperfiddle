(ns hyperfiddle.ui.stepper
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [heroicons.electric3.v24.solid :as icons])
  #?(:cljs (:require-macros hyperfiddle.ui.stepper)))

;; * Missing
;;   - [ ] horizontal spacing between icons and text must me customizable
;;   - [ ] colors must be customizable
;;   - [ ] events (e.g. click) must attach to the anchor, not children.

(e/defn Stepper [Body]
  (dom/nav
    (dom/props {:aria-label "Progress" :class "flex"})
    (dom/ol (dom/props {:role "list"}) ; :class "space-y-6"
      (Body))))

(e/defn Step [{::keys [completed? current?] :or {completed? false, current? false}} Body]
  (dom/li
    (dom/a
      (dom/props {:href "#", :class ["flex items-start" (when-not current? "group")], :style {:text-decoration-line :none}})
      (dom/span (dom/props {:class "relative flex size-5 shrink-0 items-center justify-center"})
        (if completed?
          (icons/check-circle (dom/props {:class "size-full text-indigo-600 group-hover:text-indigo-800"}))
          (if current?
            (do (dom/span (dom/props {:class "absolute size-4 rounded-full bg-indigo-200"}))
                (dom/span (dom/props {:class "relative block size-2 rounded-full bg-indigo-600"})))
            (dom/span (dom/props {:class "size-2 rounded-full bg-gray-300 group-hover:text-gray-900"})))))
      (dom/p (dom/props {:class ["ml-3 text-sm font-medium first-letter:uppercase" (if current? "text-indigo-600" "text-gray-500 group-hover:text-gray-900")]})
             (Body)))))

(defmacro stepper [& body] `(Stepper (e/fn [] ~@body)))
(defmacro step [props & body] `(Step ~props (e/fn [] ~@body)))
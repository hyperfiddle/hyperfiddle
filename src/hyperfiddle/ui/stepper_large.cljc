(ns hyperfiddle.ui.stepper-large
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [heroicons.electric3.v24.solid :as icons])
  #?(:cljs (:require-macros hyperfiddle.ui.stepper-large)))

;; * Missing
;;   - [ ] horizontal spacing between icons and text must me customizable
;;   - [ ] colors must be customizable
;;   - [ ] events (e.g. click) must attach to the anchor, not children.

(e/defn Stepper [Body]
  (dom/nav
    (dom/props {:aria-label "Progress"})
    (dom/ol (dom/props {:role "list" :class "overflow-hidden"})
      (Body))))

(e/defn Step [{::keys [completed? current?] :or {completed? false, current? false}} Body]
  (dom/li (dom/props {:class "group/step relative pb-6"})
    (dom/div (dom/props {:aria-hidden "true" :class ["group-last/step:hidden absolute left-4 top-4 -ml-px mt-0.5 h-full w-0.5" (if completed? "bg-indigo-600" "bg-gray-300") "transition-colors"]}))
    (dom/a
      (dom/props {:href "#", :class ["group relative flex" (if completed? "items-center" "items-baseline")], :style {:text-decoration-line :none}})
      (when current? (dom/props {:aria-current "step"}))
      (dom/span (dom/props {:aria-hidden true, :class "flex h-9 items-center transform-gpu"})
        (dom/span (dom/props {:class ["relative z-10 flex size-8 items-center justify-center rounded-full"
                                      (cond
                                        completed? "border-2 border-indigo-600 bg-white group-hover:bg-indigo-800 "
                                        current? "border-2 border-indigo-600 bg-white"
                                        () "border-2 border-gray-300 bg-white group-hover:border-gray-400")]})
          (cond
            completed? (dom/span (dom/props {:class "step-completed-indicator w-full h-full rounded-full bg-indigo-600 flex items-center justify-center"})
                                 (icons/check (dom/props {:aria-hidden "true" :class "size-5 text-white"})))
            current?   (dom/span (dom/props {:class "step-indicator size-2.5 rounded-full bg-indigo-600"}))
            ()         (dom/span (dom/props {:class "size-2.5 rounded-full bg-transparent group-hover:bg-gray-300"})))))
      (dom/p (dom/props {:class ["ml-4 flex min-w-0 flex-col" (when current? "text-indigo-600")]})
             (Body)))))

(defmacro stepper [& body] `(Stepper (e/fn [] ~@body)))
(defmacro step [props & body] `(Step ~props (e/fn [] ~@body)))
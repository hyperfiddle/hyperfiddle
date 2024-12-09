(ns hyperfiddle.ui.spinner
  #?(:cljs (:require-macros [hyperfiddle.ui.spinner]))
  (:require
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-svg3 :as svg]
   [hyperfiddle.electric3 :as e]))

(e/defn Spinner [Body]
  (svg/svg (dom/props {:fill         "none"
                       :viewBox      "0 0 24 24"
                       :stroke-width "1.5"
                       :stroke       "currentColor"
                       :class        "icon animate-spin"
                       :aria-hidden  "true"})
    (svg/path (dom/props {:stroke-linecap  "round"
                          :stroke-linejoin "round"
                          :d               "M12,3 A9,9 0 1,1 7.5,19.8"}))
    (Body)))

(defmacro spinner [& body] `(Spinner (e/fn [] ~@body)))
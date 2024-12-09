(ns hyperfiddle.ui.inputs
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom])
  #?(:cljs (:require-macros hyperfiddle.ui.inputs)))

(e/defn Textarea [Body]
  (dom/textarea
    (dom/props {:class "lock w-full rounded-md border-0 py-1 px-2 text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 placeholder:text-gray-400 focus:ring-2 focus:ring-inset focus:ring-indigo-600 sm:text-sm/6"})
    (Body)))

(defmacro textarea [& body] `(Textarea (e/fn [] ~@body)))
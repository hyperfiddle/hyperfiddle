(ns hyperfiddle.tailwindui.combobox
  (:require [hyperfiddle.electric-css3 :as css]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.electric-svg3 :as svg]
            [hyperfiddle.electric3 :as e]))

(e/defn ComboBox-item []
  (dom/li (dom/props {:class "relative cursor-default select-none py-2 pl-8 pr-4 text-gray-900"
                      :id "option-0" :role "option" :tabindex "-1"})
    ; Selected: "font-semibold"
    (dom/span {:class "block truncate"} (dom/text "Leslie Alexander"))
    ; Checkmark, only display for selected option.
    ; Active: "text-white", Not Active: "text-indigo-600"
    (dom/span (dom/props {:class "absolute inset-y-0 left-0 flex items-center pl-1.5 text-indigo-600"})
      (svg/svg (dom/props {:class "size-5" :viewBox "0 0 20 20" :fill "currentColor" :aria-hidden "true" :data-slot "icon"})
        (svg/path (dom/props {:fill-rule "evenodd" :clip-rule "evenodd"
                              :d "M16.704 4.153a.75.75 0 0 1 .143 1.052l-8 10.5a.75.75 0 0 1-1.127.075l-4.5-4.5a.75.75 0 0 1 1.06-1.06l3.894 3.893 7.48-9.817a.75.75 0 0 1 1.05-.143Z"}))))))

(e/defn ComboBox []
  (e/client
    (let [dom-id (str (gensym "id-"))]
      (dom/div
        (dom/label (dom/text "Assigned to")
          (dom/props {:for dom-id :class "block text-sm/6 font-medium text-gray-900"}))
        (dom/div (dom/props {:class "relative mt-2"})
          (dom/input (dom/props {:id dom-id :type "text" :role "combobox" :aria-controls "options" :aria-expanded "false"
                                 :class "w-full rounded-md border-0 bg-white py-1.5 pl-3 pr-12 text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 focus:ring-2 focus:ring-inset focus:ring-indigo-600 sm:text-sm/6"}))
          (dom/button (dom/props {:type "button" :class "absolute inset-y-0 right-0 flex items-center rounded-r-md px-2 focus:outline-none"})
            (svg/svg (dom/props {:class "size-5 text-gray-400"
                                 :viewBox "0 0 20 20" :fill "currentColor" :aria-hidden "true" :data-slot "icon"})
              (svg/path (dom/props {:fill-rule "evenodd" :clip-rule "evenodd"
                                    :d "M10.53 3.47a.75.75 0 0 0-1.06 0L6.22 6.72a.75.75 0 0 0 1.06 1.06L10 5.06l2.72 2.72a.75.75 0 1 0 1.06-1.06l-3.25-3.25Zm-4.31 9.81 3.25 3.25a.75.75 0 0 0 1.06 0l3.25-3.25a.75.75 0 1 0-1.06-1.06L10 14.94l-2.72-2.72a.75.75 0 0 0-1.06 1.06Z"}))))
          (dom/ul (dom/props {:class "absolute z-10 mt-1 max-h-60 w-full overflow-auto rounded-md bg-white py-1 text-base shadow-lg ring-1 ring-black/5 focus:outline-none sm:text-sm"
                              :id "options" :role "listbox"})
            (ComboBox-item)))))))
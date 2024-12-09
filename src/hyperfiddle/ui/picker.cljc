(ns hyperfiddle.ui.picker
  (:require
   [heroicons.electric.v24.outline :as i]
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   [hyperfiddle.ui.combobox :as cb]
   [hyperfiddle.ui.spinner :as spinner]
   [hyperfiddle.ui.virtual-scroll :as vs])
  #?(:cljs(:require-macros [hyperfiddle.ui.picker]))
  (:import (hyperfiddle.electric Pending)))

(e/def !query)
(e/def query)
(e/def !selected)
(e/def selected)
(e/def open? false)

(e/defn* Picker* [{::keys [initial-state
                          open?
                          Change  ; (e/fn [selected-or-nil] …)
                          Body]}]
  (e/client
    (binding [!query (atom "")
              !selected (atom initial-state)]
      (binding [query (e/watch !query)
                selected (e/watch !selected)]
        (cb/combobox {:open? open?
                      :value    selected
                      :OnChange (e/fn* [selected]
                                  (when (nil? selected)
                                    (reset! !query ""))
                                  ;; ensure we run the Change callback before resetting selected state
                                  ;; or the Change callback might unmount midway before the potential
                                  ;; remote part is done running.
                                  (case (when Change (Change. selected))
                                    (reset! !selected selected)))}
          (dom/props {:class "picker"})
          (binding [hyperfiddle.ui.picker/open? cb/open?]
            (try
              (cb/clear-button (dom/props {:class ["picker-clear-button" (when-not cb/current-value "hidden")]})
                (i/x-circle (dom/props {:class "icon", :aria-hidden "true"})))
              (cb/button (dom/props {:class ["picker-expand-button" (when cb/current-value "hidden")]})
                (i/chevron-up-down (dom/props {:class "icon", :aria-hidden "true"})))
              (Body.)
              (catch Pending _
                (dom/div (dom/props {:class "picker-spinner"})
                         (spinner/Spinner. {}))))))
        selected))))

(defmacro picker [{::keys [selected
                           open?
                           Change  ; (e/fn [selected-or-nil] …)
                           ]}
                  & body]
  `(new Picker* {::initial-state ~selected
                 ::open?         ~open?
                 ::Change        ~Change
                 ::Body          (e/fn* [] ~@body)}))

(defmacro input [{::keys [OnChange DisplayValue]} & body]
  `(let [OnChange#     ~OnChange
         DisplayValue# ~DisplayValue]
     (cb/input {:OnChange     (e/fn* [event#] (reset! !query (.. event# -target -value))
                                (when OnChange# (new OnChange# event#)))
                :DisplayValue (e/fn* [value#] (if DisplayValue# (new DisplayValue# value#) value#))}
       (dom/props {:class "picker-input"})
       ~@body)))


(defmacro options [{::keys [rows row-height align wrapper-props]} & body]
  `(when (e/client cb/open?)
     (let [rows# ~rows
           option-height# (or ~row-height 30)
           count#         (count rows#)
           min-height#    (* (min count# 10) option-height#)]
       (vs/virtual-scroll {::vs/rows-count  count#
                           ::vs/rows-height option-height#}
         (dom/props {:class    (if (= :left ~align) "left-0" "right-0")
                     :tabIndex "0"})
         ~(when wrapper-props ; FIXME unbundle options from vscroll
            `(dom/props ~wrapper-props))
         (dom/ul
           (dom/props {:aria-labelledby cb/label-id
                       :role            "listbox"
                       :class           "picker-options"
                       :style           {:height "100%"}})
           (try ~@body
                (catch Pending _)))))))


(defmacro option [& args] `(cb/option ~@args))

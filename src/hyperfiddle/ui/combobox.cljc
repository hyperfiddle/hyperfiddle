;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Legacy code, for reference, to be removed soon ;;
;; Replaced by hyperfiddle.ui.typeahead           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns hyperfiddle.ui.combobox
  #?(:cljs (:require-macros [hyperfiddle.ui.combobox]))
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]))

(e/def label-id nil)
(e/def input-id nil)
(e/def !open?)
(e/def open? false)
(e/def OnSelect)
(e/def current-value)
(e/def current-active)
(e/def selected? false)
(e/def active? false)

(e/def !modal-open?)
(e/def modal-open? false)

(defmacro combobox [{:keys [as value OnChange open?]
                     :or   {as       `hyperfiddle.electric-dom2/div
                            value    nil
                            OnChange `(e/fn* [_#])
                            open?     false}}
                    & body]
  `(e/client
     (let [open?# ~open?]
       (binding [!open?       (atom false)
                 !modal-open? (atom false)]
         (binding [label-id      (gensym)
                   input-id      (gensym)
                   open?         (or open?# (e/watch !open?))
                   modal-open?   (e/watch !modal-open?)
                   OnSelect      ~OnChange
                   current-value ~value]
           (~as (dom/props {::dom/data-electric-state (if open? "open" "")})
            (when (and (not open?#) open? (not modal-open?))
              (dom/on! js/document.body "click" (fn [^js e#] (when-not (.contains dom/node (.-target e#))
                                                               (reset! !open? false))))
              (dom/on! "focusin" #(reset! !open? true))
              (dom/on! "focusout" (fn [^js e#]
                                    ;; Devs must make sure options or options wrapper is focusable, i.e have a tabIndex and is not `display:contents`.
                                    ;; Otherwise `relatedTarget` will be nil and the picker will close when selecting an option, BEFORE the option is selected.
                                    (when (or (nil? (.-relatedTarget e#)) (not (.contains dom/node (.-relatedTarget e#))))
                                      (reset! !open? false)))))
            ~@body))))))

(defmacro label [& body]
  `(dom/label (dom/props {::dom/id  label-id
                          ::dom/for input-id})
     ~@body))

(defmacro input [{:keys [OnChange DisplayValue]} & body]
  `(dom/input
     (dom/props {::dom/class             "relative"
                 ::dom/id                input-id
                 ::dom/aria-expanded     (pr-str open?)
                 ::dom/aria-autocomplete "list"
                 ::dom/aria-labelledby   label-id})
     (dom/on! "focus" #(reset! !open? true))
     ~@(remove nil? [(when OnChange
                       `(dom/on "input" ~OnChange))
                     (when DisplayValue
                       `(let [arg# (new ~DisplayValue current-value)]
                          (set! (.. dom/node -value) arg#)))])
     ~@body))

#?(:cljs
   (defn focus! [input-id]
     (doto (js/document.getElementById input-id)
       (.focus)
       (.select))))

(defmacro button [& body]
  `(dom/button
     (dom/props {::dom/type            "button"
                 ::dom/tabindex        "-1"
                 ::dom/aria-haspopup   "listbox"
                 ::dom/aria-expanded   (pr-str open?)
                 ::dom/aria-labelledby label-id})
     (dom/on! "click" (partial focus! input-id))
     ~@body))

(defmacro clear-button [& body]
  `(dom/button
     (dom/props {::dom/type     "button"
                 ::dom/tabindex "-1"})
     (dom/on "click" (e/fn* [_#] (new OnSelect nil)))
     ~@body))


(defmacro options [{::keys [as]
                    :or {as 'hyperfiddle.electric-dom2/ul}} & body]
  `(when open?
     (~as (dom/props {::dom/aria-labelledby label-id
                      ::dom/role "listbox"})
       ~@body)))

(defmacro option [{:keys [as value selected?] :or {as 'hyperfiddle.electric-dom2/li}} & body]
  `(let [value#     ~value
         selected?# (or ~selected? (= value# current-value))
         active?#   false #_ (= value# current-active)] ;; TODO
     (binding [selected? selected?#
               active?   active?#]
       (~as
        (dom/props {::dom/role                "option"
                    ::dom/tabindex            -1
                    ::dom/aria-selected       selected?#
                    ::dom/data-electric-state (str (when selected?# "selected")
                                                (when active?# "active"))})
        (dom/on "click" (e/fn* [_#]
                          (case (new OnSelect value#)
                            (reset! !open? false))))
        ~@body))))

(defmacro dialog-button [& body]
  `(dom/button
     (dom/props {::dom/tabindex "-1"})
     (dom/on! "click" (fn [_#] (reset! !modal-open? true)))
     ~@body))


(defmacro divider [tw-border-color-class & body] ;; like "border-gray-300"
  `(dom/div (dom/props {::dom/class "relative w-full"})
     (dom/div (dom/props {::dom/class "absolute inset-0 flex items-center"
                          ::dom/aria-hidden "true"})
       (dom/div (dom/props {::dom/class ["w-full border-t" ~tw-border-color-class]})))
     (dom/div (dom/props {::dom/class "relative flex justify-center"})
       ~@body)))

(defmacro dialog [& body]
  `(when modal-open?
     (dom/dialog (dom/props {::dom/open modal-open?})
       (dom/on! "click" (fn [^js e#]
                          (when (= dom/node (.-target e#))
                            (.stopPropagation e#)
                            (reset! !modal-open? false))))
       ~@body)))



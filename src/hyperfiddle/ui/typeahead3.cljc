(ns hyperfiddle.ui.typeahead3 ; TODO implement with TablePicker and move to electric-forms5
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.ui.spinner :as spinner]))

;; * Missing
;;   - [ ] Extract CSS from `Typeahead` and `Typeahead!`
;;   - [ ] Linkage between an external <label> and internal <input>
;;   - [ ] Integrated virtual scroll
;;     Typeahead option panes should all come with virtual scroll out of the box.
;;     Users should only care about CSS "max-height".
;;   - [ ] Pressing <ESC> on focused input:
;;     - [ ] clears the input of any search string
;;     - [ ] closes option pane
;;   - [ ] A dirty input field should have a ❌ button on the right to clear the input
;;   - [ ] A blurred input field should have a ⌄ as the "open options" affordance
;;   - [ ] Keyboard nav
;;     - [ ] pressing <TAB> on focused input focuses first entry
;;     - [ ] pressing <TAB> on an entry focuses next control in the page - not the
;;       next option!
;;     - [ ] pressing ↑ or ↓ keys resp. move focus to the prev/next option
;;       - [ ] cycling at top or bottom
;;     - [ ] Pressing <ENTER> on a focused option selects it and closes the options pane.
;;     - [ ] `Typeahead` interface should provide a way to customize Options
;;       pane without having to resort to Combobox* constructs

(e/declare !state)
(e/declare state)

(e/declare options-id)
(e/declare format-value) ; default (constantly "") ; any -> string, not an electric fn on purpose

#?(:cljs
   (defn target-inside?
     "State if an event happened under `node`. The area on which even fires must be focusable (e.g. have a tabIndex and no display:content)."
     [node event]
     (and (some? (.-relatedTarget event)) ; relatedTarget is nil if element is not focusable or under a focusable parent.
       (.contains node (.-relatedTarget event)))))

(e/defn ComboboxWrapper [initial-state format-value Body]
  (binding [!state (atom {::open false, ::search "", ::value (e/snapshot initial-state)})] ; prevent mount/unmount
    (binding [state (e/watch !state)
              options-id (str (gensym "id-"))
              hyperfiddle.ui.typeahead3/format-value format-value]
      (swap! !state assoc ::value initial-state) ; always controlled
      ;; (dom/pre (dom/text (pr-str state)))
      (dom/div
        (dom/On "focusin" #(do (swap! !state assoc ::open true) %) nil)
        (dom/On "focusout" #(do (when-not (target-inside? dom/node %) (swap! !state assoc ::open false)) %) nil)
        (Body)))))

(e/defn ComboboxInput [Body & {:keys [open?]}]
  (dom/input
    (let [open? (or (::open state) open?)]
      (dom/props {:type :search, :role "combobox", :aria-controls options-id, :aria-expanded (boolean open?)})
      (let [user-input (dom/On "input" #(-> % .-target .-value) "")]
        (prn "user-input" user-input)
        (if open?
          (do (set! (.-value dom/node) user-input) ; reset user search input on re-open, noop otherwise
              (swap! !state assoc ::search user-input))
          (set! (.-value dom/node) (format-value (::value state)))))
      (Body))))

(e/defn ComboboxOptions [Body]
  (let [{::keys [open search]} state]
    (dom/div
      (dom/props {:aria-role :listbox :tabIndex "1", :id options-id})
      (Body open search))))

(e/defn ComboboxOption [value Body]
  (let [selected? (= value (::value state))]
    (dom/div
      (dom/props {:aria-role :option
                  :aria-selected (or selected? nil) ; attr is present or not, not true|false
                  })
      (dom/On "click" (fn [^js e]
                        (swap! !state assoc ::value value)
                        (swap! !state assoc ::open false) ; swap in two times for better perceived responsiveness
                        (set! (.-hyperfiddle_ui_typeahead3_submit e) true)
                        e) nil)
      (Body (format-value value) selected?))))

(e/defn Loader [query Fallback Body]
  (let [!loaded? (atom false)
        cnt      (e/Count query)]
    (when (pos? cnt)
      (reset! !loaded? true))
    (if (or (e/watch !loaded?) (pos? cnt))
      (Body query)
      (Fallback))))

(e/defn Typeahead [selected
                   & {:keys [Options
                             option-label ; not an e/fn on purpose¹
                             open?]}]
  ;; ¹ option-label is used to render options, but also to render the selected
  ;;   option into the input. Inputs only accept strings, so option-label must
  ;;   not mount DOM items (props, nodes) and must return a string. If we allow
  ;;   an e/fn for option-label, user can query the server (good) but also alter
  ;;   the DOM (not allowed).
  (ComboboxWrapper selected option-label
    (e/fn []
      (ComboboxInput
        (e/fn [] (dom/props {:placeholder "Filter..."}))
        :open? open?)
      (ComboboxOptions
        (e/fn [user-open? search]
          (when (or user-open? open?)
            (Loader (Options search)
              (e/fn [] (spinner/spinner))
              (e/fn [options]
                (e/for [opt options]
                  (ComboboxOption opt
                    (e/fn [label selected?]
                      (dom/span (dom/text label))))))))))
      (::value state))))

(defn reset-typeahead! [!state input-node]
  (swap! !state assoc ::search "")
  (set! (.-value input-node) "")
  nil)

;; TODO add Parse/Unparse
(e/defn Typeahead! ; same as `Typeahead`, but return form edits
  [name selected
   & {:keys [Options
             option-label ; not an e/fn on purpose¹
             open?]
      :as props}]
  ;; ¹ option-label is used to render options, but also to render the selected
  ;;   option into the input. Inputs only accept strings, so option-label must
  ;;   not mount DOM items (props, nodes) and must return a string. If we allow
  ;;   an e/fn for option-label, user can query the server (good) but also alter
  ;;   the DOM (not allowed).
  (ComboboxWrapper selected option-label
    (e/fn []
      (let [{::keys [value open]} state
            input-node (ComboboxInput
                         (e/fn [] (dom/props {:placeholder "Filter..."})
                           (dom/props (dissoc props :Options :label :open?))
                           dom/node)
                         :open? open?)]
        input-node ; force let binding, forces input to show up
        (ComboboxOptions
          (e/fn [user-open? search]
            (when (or open? user-open?)
              (Loader (Options search)
                (e/fn [] (spinner/spinner ))
                (e/fn [options]
                  (e/for [opt options]
                    (ComboboxOption opt
                      (e/fn [label selected?]
                        (dom/span
                          (dom/text label))))))))))
        (let [e (e/Filter some? (dom/On "click" (fn [^js e] (when (.-hyperfiddle_ui_typeahead3_submit e) e)) nil))
              [t err] (e/Token e)
              waiting? (some? t)
              error? (some? err)]
          (when error? (dom/props input-node {:aria-invalid true}))
          (if waiting?
            (do (dom/props input-node {:aria-busy true})
                [(e/->Token `Typeahead3 t (fn ([] (reset-typeahead! !state input-node)) ; success: Typeahead's authoritative value is expected to loop back from server
                                            ([err] (reset-typeahead! !state input-node)))) ; todo use forms/after-ack
                 {name value}])
            (e/amb)))))))


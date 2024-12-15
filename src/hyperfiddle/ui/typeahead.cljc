(ns hyperfiddle.ui.typeahead
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
              hyperfiddle.ui.typeahead/format-value format-value]
      (swap! !state assoc ::value initial-state) ; always controlled
      ;; (dom/pre (dom/text (pr-str state)))
      (dom/div
        (dom/props {:class "hyperfiddle-typeahead relative"})
        (dom/On "focusin" #(do (swap! !state assoc ::open true) %) nil)
        (dom/On "focusout" #(do (when-not (target-inside? dom/node %) (swap! !state assoc ::open false)) %) nil)
        (Body)))))

(e/defn ComboboxInput [Body]
  (dom/input
    (let [open? (::open state)]
      (dom/props {:type :search, :role "combobox", :aria-controls options-id, :aria-expanded open?})
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
      (dom/props {:aria-role :listbox
                  :class [(#(when-not % "hidden") open)]
                  :tabIndex "1"})
      (.. dom/node -classList (add "absolute" "grid" "grid-flow-row" "overflow-x-hidden" "overflow-y-auto")) ; for perfs
      (Body open search))))

(e/defn ComboboxOption [value Body]
  (let [selected? (= value (::value state))]
    (dom/div
      (dom/props {:aria-role :option
                  :aria-selected (or selected? nil) ; attr is present or not, not true|false
                  })
      (.. dom/node -classList (add "grid" "grid-rows-subgrid" "grid-cols-subgrid"
                                "text-nowrap" "max-w-full" "text-ellipsis" "overflow-hidden"
                                "cursor-pointer")) ; for perfs
      (dom/On "click" (fn [^js e]
                        (swap! !state assoc ::value value)
                        (swap! !state assoc ::open false) ; swap in two times for better perceived responsiveness
                        (set! (.-hyperfiddle_ui_typeahead_submit e) true)
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
                             ]}]
  ;; ¹ option-label is used to render options, but also to render the selected
  ;;   option into the input. Inputs only accept strings, so option-label must
  ;;   not mount DOM items (props, nodes) and must return a string. If we allow
  ;;   an e/fn for option-label, user can query the server (good) but also alter
  ;;   the DOM (not allowed).
  (ComboboxWrapper selected option-label
    (e/fn []
      (ComboboxInput
        (e/fn [] (dom/props {:placeholder "Filter..."})))
      (ComboboxOptions
        (e/fn [open? search]
          (dom/props {:class "bg-white shadow-lg w-full rounded py-0.5 border border-slate-300 z-10"})
          (when open?
            (Loader (Options search)
              (e/fn [] (spinner/spinner (dom/props {:class "m-1 w-4 aspect-square justify-self-center"})))
              (e/fn [options]
                (e/for [opt options]
                  (ComboboxOption opt
                    (e/fn [label selected?]
                      (dom/props {:class ["px-2 hover:text-white hover:bg-blue-400"
                                          (when selected? "text-white bg-blue-500")]})
                      (dom/span (dom/props {:class "overflow-hidden text-ellipsis"})
                                (dom/text label))))))))))
      (::value state))))

(defn reset-typeahead! [!state input-node]
  (swap! !state assoc ::search "")
  (set! (.-value input-node) "")
  nil)

(e/defn Typeahead! ; same as `Typeahead`, but return form edits
  [name selected
   & {:keys [Options
             option-label ; not an e/fn on purpose¹
             edit-monoid
             open?]
      :or {edit-monoid hash-map}
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
                           (dom/props (dissoc props :Options :label :edit-monoid :open?))
                           dom/node))]
        input-node ; force let binding, forces input to show up
        (ComboboxOptions
          (e/fn [user-open? search]
            (dom/props {:class "bg-white shadow-lg w-full rounded py-0.5 border border-slate-300 z-10"})
            (when (or open? user-open?)
              (Loader (Options search)
                (e/fn [] (spinner/spinner (dom/props {:class "m-1 w-4 aspect-square justify-self-center"})))
                (e/fn [options]
                  (e/for [opt options]
                    (ComboboxOption opt
                      (e/fn [label selected?]
                        (.add (.-classList dom/node) "px-2" "hover:text-white" "hover:bg-blue-400")
                        (dom/props {:class [(when selected? "text-white bg-blue-500")]})
                        (dom/span (.add (.-classList dom/node) "overflow-hidden" "text-ellipsis")
                          (dom/text label))))))))))
        (let [e (e/Filter some? (dom/On "click" (fn [^js e] (when (.-hyperfiddle_ui_typeahead_submit e) e)) nil))]
          (if (= value selected)
            (e/amb)
            (let [[t err] (e/Token e)
                  waiting? (some? t)
                  error? (some? err)]
              (when error? (dom/props input-node {:aria-invalid true}))
              (if waiting?
                (do (dom/props input-node {:aria-busy true})
                    [(fn ([] (t) (reset-typeahead! !state input-node)) ; success: Typeahead's authoritative value is expected to loop back from server
                       ([err] (t err) (reset-typeahead! !state input-node))) ; failure: leave current state untouched, user will edit
                     (edit-monoid name value)])
                (e/amb)))))))))


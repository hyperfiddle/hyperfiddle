(ns hyperfiddle.navigator6.arg
  "Multi-field parameter form for partial HFQL calls.
   Shows one field per call argument: editable for free (unbound) params,
   disabled for bound (lexical/global) params. Uses forms5 for EDN validation.
   Follows the same effect pattern as search.cljc (WithLocalSearch → WithLocalArg)."
  (:require [clojure.string :as str]
            [edamame.core :as edn]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.electric-forms5 :refer [Form! Input! SubmitButton!]]
            [hyperfiddle.navigator6.state :as state]
            [hyperfiddle.navigator6.utils :as utils])
  #?(:cljs (:require-macros [hyperfiddle.navigator6.arg :refer [with-local-arg]])))

;; Committed arg values from route state — nil until user submits.
;; Map of {symbol parsed-edn-value} for multi-param, or nil.
(e/declare ^:dynamic *arg)

(e/defn WithLocalArg [state-key Body]
  (e/client
    (binding [*arg (get state/*local-state state-key)]
      (->> (Body)
        (utils/InterceptCommand `SetArg!
          ;; value is a map {sym edn-val} from the multi-field form
          (e/fn [[_ value]] [`state/UpdateState! assoc state-key value]))))))

(defmacro with-local-arg [state-key & body]
  `(WithLocalArg ~state-key (e/fn [] ~@body)))

(defn parse-edn
  "Parse an EDN string, returning the parsed value or an ex-info on failure.
   Regular defn (not e/fn) so try/catch works — Electric doesn't have try/catch."
  [s]
  (try (edn/parse-string s)
       (catch #?(:clj Exception :cljs :default) e
         (ex-info (str "Invalid EDN: " #?(:clj (ex-message e) :cljs (.-message e))) {}))))

(e/defn ArgInput!
  "Multi-field parameter form for partial HFQL calls.
   Free params: editable Input! with EDN parse + inline validation.
   Bound params: disabled <input> showing symbol name.
   Submit emits SetArg! with {sym parsed-edn, ...} map.
   Styling: relies on electric-forms.css (form dl grid, required indicators,
   submit button) — see that file for layout and validation CSS."
  [params]
  (e/client
    (dom/div (dom/props {:class "parameter-form"})
      (let [;; Restore previously submitted values from route state
            saved (or *arg {})
            ;; Initial form values — only free params participate in Form!
            ;; Keyword keys for forms5 field names (sym→kw conversion)
            initial (into {} (comp (filter :free?)
                                  (map (fn [{:keys [name]}]
                                         [(keyword name) (get saved name)])))
                         params)]
        (Form! initial
          (e/fn [form-v]
            (e/amb
              (dom/dl
                ;; e/diff-by converts the params vector into a keyed concurrent stream (table).
                ;; Without it, e/for treats the vector as a single element — not 4 separate frames.
                ;; Free params contribute Input! edits, bound params contribute (e/amb) = nothing.
                (e/for [{:keys [name free? display-value]} (e/diff-by :name params)]
                  (let [kw (keyword name)
                        input-id (random-uuid)]
                    ;; dt with label — electric-forms.css shows red star for required fields
                    (dom/dt (dom/label (dom/props {:for input-id}) (dom/text (str name))))
                    ;; dd with input — sequential after dt (NOT e/amb)
                    (dom/dd
                      (if free?
                        ;; Editable — EDN parse with per-field validation via forms5.
                        ;; :required true → electric-forms.css shows "Required" when empty.
                        ;; parse-edn is a regular defn (not e/fn) so try/catch works.
                        (Input! kw (get form-v kw)
                          :id input-id
                          :required true
                          :placeholder (str "EDN value for " name)
                          :Parse (e/fn [s]
                                   (if (str/blank? s)
                                     nil ;; empty = not yet filled
                                     (parse-edn s)))
                          :Unparse (e/fn [v] (if (some? v) (pr-str v) "")))
                        ;; Bound param — disabled input showing resolved default value.
                        ;; display-value is the pr-str'd default from the expr closure.
                        ;; Returns (e/amb) = nothing, so MergeEdits ignores this frame.
                        (do (dom/element :input
                              (dom/props {:disabled true
                                         :value (or display-value (str name))
                                         :id input-id}))
                            (e/amb)))))))
              (SubmitButton! :label "Run")))
          ;; Convert keyword form fields to symbol-keyed map for HFQL scope seeding
          :Parse (e/fn [merged-fields _tempid]
                   [`SetArg! (into {} (map (fn [{:keys [name]}]
                                              [(symbol name) (get merged-fields (keyword name))]))
                                   (filter :free? params))])
          :type :command)))))

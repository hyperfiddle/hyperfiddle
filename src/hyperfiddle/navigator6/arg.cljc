(ns hyperfiddle.navigator6.arg
  "Parameter form for partial HFQL calls — lets the user seed `%` via a textarea.
   Follows the same effect pattern as search.cljc (WithLocalSearch → WithLocalArg)."
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.navigator6.state :as state]
            [hyperfiddle.navigator6.utils :as utils])
  #?(:cljs (:require-macros [hyperfiddle.navigator6.arg :refer [with-local-arg]])))

;; Committed arg text from route state — nil until user submits.
(e/declare ^:dynamic *arg)

(e/defn WithLocalArg [state-key Body]
  (e/client
    (binding [*arg (get state/*local-state state-key)]
      (->> (Body)
        (utils/InterceptCommand `SetArg!
          (e/fn [[_ text]] [`state/UpdateState! assoc state-key text]))))))

(defmacro with-local-arg [state-key & body]
  `(WithLocalArg ~state-key (e/fn [] ~@body)))

(e/defn ArgInput!
  "Textarea + submit button for entering the `%` parameter as EDN.
   Uses e/Token for explicit submit (Ctrl+Enter or button click) — not reactive."
  []
  (e/client
    (dom/div (dom/props {:class "parameter-form"})
      (let [;; Local DOM state — textarea value lives here until submit
            !text (atom (or *arg ""))
            text (e/watch !text)
            ;; Submit via button click or Ctrl+Enter in textarea
            submit-event
            (e/amb
              (dom/div (dom/props {:class "parameter-form__input"})
                (dom/element :textarea
                  (dom/props {:class "parameter-form__textarea"
                              :placeholder "Enter EDN value for %\ne.g. {:query '[...] :args [*db*]}"
                              :rows 5
                              :spellcheck false})
                  (set! (.-value dom/node) text)
                  (dom/On "input" (fn [^js e] (reset! !text (.. e -target -value)) nil) nil {})
                  ;; Ctrl+Enter submits — return event object (unique per keypress)
                  ;; so e/Token sees a value transition on each submit.
                  (dom/On "keydown"
                    (fn [^js e]
                      (when (and (= "Enter" (.-key e)) (or (.-ctrlKey e) (.-metaKey e)))
                        (.preventDefault e)
                        e))
                    nil {})))
              (dom/button (dom/props {:class "parameter-form__submit"
                                      :title "Submit (Ctrl+Enter)"})
                (dom/text "Run")
                ;; Return event object, not boolean — e/Token needs unique values
                ;; to detect repeated clicks (true→true is not a transition).
                (dom/On "click" (fn [^js e] (.preventDefault e) e) nil {})))]
        ;; Gate submission through e/Token — one pending at a time
        (let [[t _err] (e/Token submit-event)]
          (e/When t [t [`SetArg! @!text]]))))))

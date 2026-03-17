(ns hyperfiddle.ui.codemirror-forms5
  "CodeMirror-based EDN input with forms5 transactional semantics.
   Bridges CodeMirror's updateListener model into forms5's token/dirty/validation
   contract, so it works as a drop-in for Input! inside Form!.
   Reuses contrib.electric-codemirror/codemirror (missionary observable) for CM
   lifecycle — we add the forms5 edit lifecycle on top."
  (:require
    [contrib.electric-codemirror :as cm]
    [hyperfiddle.electric3 :as e]
    [hyperfiddle.electric-dom3 :as dom]
    [hyperfiddle.electric-forms5 :as forms5 :refer [Identity tx-error?]]
    [missionary.core :as m]))

;; Validation bridge — CM has no native form element, so .setCustomValidity()
;; can't be called on CM's contenteditable. A hidden proxy <input> carries
;; the :invalid pseudo-class for the CSS error chain:
;;   dd:has(:invalid)::after { content: var(--error-message) }
;; data-errormessage on the wrapper feeds --error-message via electric-forms.css.
(e/defn CMSetValidity [wrapper-node parent-node parsed-v]
  (let [message (str (ex-message parsed-v))]
    ;; Set data-errormessage on the dd (parent-node), not the wrapper —
    ;; dd::after renders the error via --error-message CSS variable,
    ;; and CSS vars on a child don't propagate up to the parent's pseudo-element.
    (if (not-empty message)
      (dom/props parent-node {:data-errormessage message})
      (dom/props parent-node {:data-errormessage nil}))
    ;; Hidden proxy input — carries .setCustomValidity() for :invalid pseudo-class.
    ;; No `required` attr — proxy is always empty, so native required would be
    ;; permanently :invalid. Required-ness belongs in the Parse function instead.
    (dom/element :input
      (dom/props {:type "text" :tabindex -1 :aria-hidden "true"
                  :style {:position "absolute" :width "1px" :height "1px"
                          :overflow "hidden" :clip "rect(0,0,0,0)"
                          :border 0 :padding 0}})
      (.setCustomValidity dom/node message)
      (e/on-unmount #(.setCustomValidity dom/node "")))))

(e/defn EdnInput!
  "CodeMirror-based EDN input for forms5. Drop-in for Input! inside Form!.
   Returns [token {field-name parsed-value}] when editing, (e/amb) otherwise.
   CM already sets role=\"textbox\" + aria-multiline on .cm-content internally;
   wrapper carries ARIA state attributes (aria-invalid, aria-busy, aria-required)."
  [field-name v & {:keys [id Parse Unparse required placeholder extensions readonly]
                   :or {id (random-uuid)
                        Parse  Identity
                        Unparse (forms5/Lift pr-str)}}]
  (e/client
    v ; consume v — same guard as Input! line 131
    (let [parent-node dom/node]
      (dom/div
        (dom/props {:class "codemirror-forms5" :id id})
        (when-some [[!cm >cm-v] (e/input (cm/codemirror {:parent dom/node
                                                       :inline true
                                                       :readonly readonly}))]
          ;; m/reductions turns discrete >cm-v into continuous signal for e/Token.
          ;; ::no-edit sentinel = "no user edit yet" — on? predicate filters it out.
          (let [cm-v (e/input (m/relieve {} (m/reductions {} ::no-edit >cm-v)))
                [t err] (e/Token cm-v (fn [v] (not= v ::no-edit)))
                editing? (dom/Focused-in?)
                waiting? (some? t)
                error?   (tx-error? err)
                dirty?   (#(or editing? waiting? error?))
                ;; "Don't damage user input" — cm-v is the reactive string from >cm-v,
                ;; updated on each keystroke. Imperative (.. !cm -state -doc) would freeze
                ;; because Electric doesn't see CM's internal state changes.
                unparsed-v (e/Reconcile
                             (if waiting?
                               cm-v
                               (str (Unparse v))))
                parsed-v (Parse unparsed-v)
                ;; Track last v synced to CM — detects no-op commits where v didn't
                ;; change (= ignores metadata). Without this, whitespace-only edits
                ;; revert on commit because (Unparse unchanged-v) produces the old string.
                !synced-v (atom ::not-synced)]
            (CMSetValidity dom/node parent-node parsed-v)
            ;; Controlled value writeback — only when form is clean AND v actually changed.
            ;; identical? detects no-op commits: if v is the same object reference,
            ;; the commit didn't propagate (= work-skipped), so keep user's CM text.
            (when-not dirty?
              (#(when (and (some? !cm) (not (identical? v @!synced-v)))
                  (reset! !synced-v v)
                  (cm/cm-set! !cm unparsed-v))))
            ;; ARIA state on wrapper (CM's .cm-content already has role="textbox")
            (dom/props {:aria-required (#(when % true) required)
                        :aria-invalid  (#(when % true) error?)
                        :aria-busy     (#(when % true) waiting?)})
            ;; Return edit tuple — same shape as Input!, bubbles to Form!/MergeEdits
            (e/When waiting?
              [t {field-name parsed-v}])))))))

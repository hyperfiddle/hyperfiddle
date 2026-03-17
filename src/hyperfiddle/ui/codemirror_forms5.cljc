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
    [missionary.core :as m]
    #?@(:cljs [["@codemirror/lint" :refer [linter]]
               [edamame.core :refer [parse-string]]])))

;; ── EDN Linter ──────────────────────────────────────────────────────────
;; Runs inside CM6's lint infrastructure — parses the document on each change
;; via edamame and maps parse errors to inline diagnostics (wavy red underline).
;; Adapted from scratch/src/edn_view/linter.cljs.
#?(:cljs
   (defn- edn-locate
     "Convert edamame's {:row :col} error position to CM's {:from :to} byte offsets.
      Falls back to start-of-doc if position info is missing."
     [^js state err]
     (let [{:keys [row col]} (ex-data err)
           doc-len (.. state -doc -length)]
       (if (and row col)
         ;; edamame's col is 1-based; CM's doc offsets are 0-based.
         (let [pos (min (+ (.. state -doc (line row) -from) (dec col)) doc-len)
               ;; Strategy: try Lezer first, fall back to token scan.
               ;; Lezer gives accurate spans when the tree is locally well-formed,
               ;; but returns multi-line root/error nodes when it's not.
               ^js node (.. state -tree (resolveInner pos 1))
               nfrom (.-from node)
               nto   (.-to node)
               ;; Trust Lezer only if the node stays on one line — multi-line nodes
               ;; are error-recovery artifacts that span far beyond the actual token.
               tight? (and (< nfrom nto)
                        (= (.. state -doc (lineAt nfrom) -number)
                           (.. state -doc (lineAt (dec nto)) -number)))]
           (if tight?
             {:from nfrom :to nto}
             ;; Lezer can't help — scan from error pos to next EDN delimiter.
             (let [text (.. state -doc (toString))
                   delim? #{\space \tab \newline \return \, \( \) \[ \] \{ \}}
                   end (loop [i pos]
                         (if (>= i doc-len) doc-len
                           (if (and (> i pos) (delim? (.charAt text i)))
                             i (recur (inc i)))))]
               {:from pos :to (max (inc pos) end)})))
         ;; No position info — highlight first char as a fallback marker.
         {:from 0 :to (min 1 doc-len)}))))

#?(:cljs
   (defn- edn-lint
     "CM6 lint source: parse doc as EDN, return diagnostics array."
     [^js state]
     (let [doc (.. state -doc (toString))]
       (if (or (empty? doc) (re-matches #"\s*" doc))
         #js[]  ; blank input — nothing to lint
         (try (parse-string doc)
              #js[]  ; valid EDN — no errors
              (catch :default e
                (let [{:keys [from to]} (edn-locate state e)]
                  #js[#js{:from     from
                          :to       to
                          :severity "error"
                          :source   "EDN"
                          :message  (ex-message e)}])))))))

#?(:cljs
   (defonce edn-linter
     ;; defonce — stable across hot reloads so CM doesn't re-register the extension.
     (linter (fn [^js view] (edn-lint (.-state view))))))

;; Hoisted outside e/defn — reader conditionals inside Electric code
;; cause DAG misalignment between CLJ and CLJS compilation.
(def ^:private edn-extensions #?(:cljs [edn-linter] :clj []))

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
                                                       :readonly readonly
                                                       ;; Inline EDN linter — wavy red underline at parse errors.
                                                       :extensions edn-extensions}))]
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

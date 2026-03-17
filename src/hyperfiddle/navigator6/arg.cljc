(ns hyperfiddle.navigator6.arg
  "Multi-field parameter form for partial HFQL calls.
   Shows one field per call argument: editable for free (unbound) params,
   disabled for bound (lexical/global) params. Uses forms5 for EDN validation.
   Follows the same effect pattern as search.cljc (WithLocalSearch → WithLocalArg)."
  (:require [clojure.string :as str]
            [edamame.core :as edn]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.electric-forms5 :as forms :refer [Form! SubmitButton!]]
            [hyperfiddle.ednish :as ednish]
            [hyperfiddle.hfql2.utils :refer [has-meta?]]
            [hyperfiddle.router5 :as router]
            [hyperfiddle.ui.codemirror-forms5 :refer [EdnInput!]]
            [hyperfiddle.navigator6.state :as state]
            [hyperfiddle.navigator6.utils :as utils])
  #?(:cljs (:require-macros [hyperfiddle.navigator6.arg :refer [with-local-arg]])))

;; Committed arg values — stored in sessionStorage (not URL) to avoid
;; ednish encoding issues with Jetty 12. Map of {symbol parsed-edn-value}.
(e/declare ^:dynamic *arg)

;; ── SessionStorage helpers ──────────────────────────────────────────
;; Args bypass the URL path but use EDNish encoding for consistency
;; (and to benefit from ~edn metadata preservation).

#?(:cljs
   (defn- read-session-storage [k]
     (when-let [s (.getItem js/sessionStorage k)]
       (try (first (ednish/parse s)) (catch :default _ nil)))))

#?(:cljs
   (defn- write-session-storage [k v]
     (if (some? v)
       (.setItem js/sessionStorage k (ednish/serialize v))
       (.removeItem js/sessionStorage k))))

#?(:cljs
   (defn- setup-session-sync!
     "Sync atom changes to sessionStorage. add-watch on same key is idempotent."
     [!atom storage-key]
     (add-watch !atom ::session-sync
       (fn [_ _ _ v] (write-session-storage storage-key v)))))

#?(:cljs
   (defn- teardown-session-sync! [!atom]
     (remove-watch !atom ::session-sync)))

#?(:cljs
   (defn- gc-and-init!
     "Keeps the arg value stable across route changes while avoiding garbage keys.
      Route change (prev non-nil): migrate current value to new key, remove old key.
      First mount / refresh (prev nil): restore from sessionStorage."
     [!prev-key !arg storage-key]
     (let [prev @!prev-key]
       (when (not= prev storage-key)
         (if prev
           ;; Route changed within session — migrate value to new key, GC old.
           (do (.removeItem js/sessionStorage prev)
               (write-session-storage storage-key @!arg))
           ;; First mount (or refresh) — restore from sessionStorage.
           (reset! !arg (read-session-storage storage-key)))
         (reset! !prev-key storage-key)))))

;; ── WithLocalArg ────────────────────────────────────────────────────

(e/defn WithLocalArg [state-key Body]
  (e/client
    ;; Storage key scoped by depth + focused route segment — prevents
    ;; different cards from colliding in sessionStorage.
    (let [storage-key (str "hf-arg-" state/*depth "-" (hash router/route))
          !prev-key (atom nil)
          !arg (atom nil)
          arg (e/watch !arg)]
      ;; Side effects in let body — Electric lazy let only realizes bindings
      ;; that are referenced downstream; body expressions are sampled by implicit do.
      (gc-and-init! !prev-key !arg storage-key)
      (setup-session-sync! !arg storage-key)
      (e/on-unmount #(teardown-session-sync! !arg))
      (binding [*arg arg]
        ;; Interpreter handles SetArg! locally (atom reset + ack).
        ;; Unrecognized commands pass through unchanged.
        (forms/Interpreter {`SetArg! (e/fn [value] (reset! !arg value) ::forms/ok)}
          (Body))))))

(defmacro with-local-arg [state-key & body]
  `(WithLocalArg ~state-key (e/fn [] ~@body)))

(defn parse-edn
  "Parse an EDN string, returning the parsed value or an ex-info on failure.
   Regular defn (not e/fn) so try/catch works — Electric doesn't have try/catch."
  [s]
  (try (edn/parse-string s)
       (catch #?(:clj Exception :cljs :default) e
         (ex-info (str "Invalid EDN: " #?(:clj (ex-message e) :cljs (.-message e))) {}))))

(defn- parse-edn-field
  "Parse EDN string, attaching original as metadata so EDNish preserves
   user formatting across URL roundtrips (see ::ednish/edn)."
  [s]
  (when-not (str/blank? s)
    (let [parsed (parse-edn s)]
      (if (instance? #?(:clj Throwable :cljs js/Error) parsed)
        parsed
        (cond-> parsed
          (has-meta? parsed) (with-meta {::ednish/edn s}))))))

(defn- unparse-edn-field
  "Prefer user's original string (from ::ednish/edn metadata) over pr-str,
   so formatting survives URL roundtrips."
  [v]
  (if (some? v)
    (or (::ednish/edn (meta v)) (pr-str v))
    ""))

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
                    ;; dt with label + tooltip (like matrix column headers)
                    (dom/dt (dom/label (dom/props {:for input-id
                                                   :title (if free?
                                                            (str name)
                                                            (str name " = " display-value))})
                                       (dom/text (str name))))
                    ;; dd with input — sequential after dt (NOT e/amb)
                    (dom/dd
                      (if free?
                        ;; Editable — CodeMirror with EDN syntax highlighting.
                        ;; Drop-in for Input! inside Form! via forms5 token bridge.
                        (EdnInput! kw (get form-v kw)
                          :id input-id
                          :required true
                          :placeholder (str "EDN value for " name)
                          :Parse (e/fn [s] (parse-edn-field s))
                          :Unparse (e/fn [v] (unparse-edn-field v)))
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

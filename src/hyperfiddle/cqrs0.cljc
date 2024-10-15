(ns hyperfiddle.cqrs0
  #?(:cljs (:require-macros hyperfiddle.cqrs0))
  (:require [contrib.data :refer [index-by]]
            [contrib.str :refer [pprint-str]]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.input-zoo0 :refer [Button! ButtonGenesis!]]))

; commit/discard with staging area
; inputs - emit as you type - with a token
; stage - monitor edits and accumulate them
; button - batch all edits into single token, chained with upstream tokens

#?(:cljs (defn- active-form-input [form]
           (when-let [focused-input (.-activeElement js/document)]
             (when (.contains form focused-input)
               focused-input))))

#?(:cljs (defn- blur-active-form-input! [form] (some-> (active-form-input form) (.blur))))

#?(:cljs (defn- reset-active-form-input! [form]
           (when-let [input (active-form-input form)]
             (set! (.-value input) ""))))

(e/defn FormDiscard! ; dom/node must be a form
  [directive & {:keys [disabled show-button label form] :as props}]
  (e/client
    (dom/On "keyup" #(when (= "Escape" (.-key %)) (.stopPropagation %) (.reset dom/node) nil) nil)
    (e/When show-button
      (let [[t err] (e/apply Button! directive (mapcat identity (-> props (dissoc :form) ; if we don't dissoc form, both the button and FormDiscard will try to burn the token and we get an NPE - seems like the `when true` bug.
                                                                  (assoc :type :reset))))]
        (t))) ; always safe to call, Button returns [t err] or (e/amb)
    (let [[t err] (e/RetryToken (dom/On "reset" #(do #_(.log js/console %) (.preventDefault %)(.stopPropagation %)
                                                     (blur-active-form-input! (.-target %)) %) nil))]
      (if t ; TODO unify with FormSubmit! and Button!
        (let [[form-t form-v] form]
          (prn "click discard" form)
          [(fn token
             ([] (t) (when form-t (form-t))) ; reset controlled form and both buttons, cancelling any in-flight commit
             ([err] (t err) #_(form-t err))) ; redirect error to button ("retry"), leave uncommitted form dirty
           (if form-t [directive form-v] [directive])]) ; compat
        (e/amb)))))

(e/defn FormSubmit! ; dom/node must be a form
  [directive & {:keys [disabled show-button label auto-submit form] :as props}]
  (e/client
    (let [[t err] (e/amb
                    ;; FIXME pressing Enter while an autosubmit commit is running will trigger a double submit and hang the app
                    (e/When show-button (e/apply Button! directive (mapcat identity (assoc props :type :submit)))) ; genesis ; (e/apply Button directive props) didn't work - props is a map
                    (let [submit-event (dom/On "submit" #(do (.preventDefault %) (.stopPropagation %) (when-not (or auto-submit disabled) %)) nil)]
                      submit-event ; force signal
                      (if auto-submit
                        (e/RetryToken form)
                        (e/When (not show-button) ; show-buttons will render an <button type=submit> auto handling Enter
                          (do (dom/On "keypress" (fn [e] (let [target (.-target e)] ; submit form on enter
                                                           (when (and (= "Enter" (.-key e)) (= "INPUT" (.-nodeName target)))
                                                             (.stopPropagation e)
                                                             (.requestSubmit (.-form target)) ; fire submit event
                                                             nil))) nil)
                              (let [[t err :as token] (e/RetryToken submit-event)]
                                (dom/props {:aria-invalid (some? err)})
                                token))))))]
      (if t ; TODO unify with FormSubmit! and Button!
        (let [[form-t form-v] form]
          [(fn token
             ([] (t) (when form-t (form-t))) ; reset controlled form and both buttons, cancelling any in-flight commit
             ([err] (t err) #_(form-t err))) ; redirect error to button ("retry"), leave uncommitted form dirty
           (if form-t [directive form-v] [directive nil])]) ; compat
        (e/amb)))))

(e/defn FormSubmitGenesis!
  "Spawns a new tempid/token for each submit. You must monitor the spawned entity's
lifecycle (e.g. for errors) in an associated optimistic collection view!"
  [directive & {:keys [disabled show-button label form #_auto-submit] :as props}] ; auto-submit unsupported
  (e/amb
    ;; TODO unify ButtonGenesis! and Button!
    (e/When show-button (ButtonGenesis! directive :disabled disabled :label label :form form)) ; button will intercept submit events to act as submit!
    ; But, button may hidden, so no default submit, so we need to explicitly handle this also
    (e/for [[btn-q _e] (dom/On-all "submit" #(do (.preventDefault %) (.stopPropagation %)
                                                 (when-not disabled (doto % (js/console.log 'FormSubmitGenesis!-submit)))))]
      (e/on-unmount #(prn "unmount genesis branch"))
      ;; TODO logic duplicated in ButtonGenesis!
      (let [[form-t form-v] (e/snapshot form)] ; snapshot to detach form before any reference to form, or spending the form token would loop into this branch and cause a crash.
        (form-t) ; immediately detach form
        [(fn token ; proxy genesis
           ([] (btn-q) #_(form-t))
           ([err] '... #_(btn-q err)))
         ;; abandon entity and clear form, ready for next submit -- snapshot to avoid clearing concurrent edits
         [directive form-v]]))))


(defn invert-fields-to-form [edit-merge edits]
  (when (seq edits)
    (let [ts (map first edits)
          kvs (map second edits)
          dirty-form (not-empty (apply edit-merge kvs)) ; collect fields into form, retain until commit/discard
          #_#_dirty-form-guess (apply merge (e/as-vec guess)) ; todo collisions
          form-t (fn token ; fresh if ts changes (should fields be disabled when commit pending?)
                   ([] (doseq [t ts] (t)))
                   #_([err] (doseq [t ts] (t err ::keep)))) ; we could route errors to dirty fields, but it clears dirty state
          ]
      [form-t dirty-form])))

(comment

  (invert-fields-to-form merge [])
  (invert-fields-to-form merge [[#() {:a "a"}] [#() {:b "b"}]])
  := [_ {:a "a", :b "b"}])

(defn call [f] (f))

(e/defn Form!*
  ([#_field-edits ; aggregate form state - implies circuit controls, i.e. no control dirty state
    edits ; concurrent edits are what give us dirty tracking
    & {:keys [debug commit discard show-buttons auto-submit edit-merge genesis name edit-monoid]
       :or {debug false show-buttons true edit-merge merge genesis false edit-monoid hash-map}}]
   (e/client
     (let [dirty-count (e/Count edits)
           clean? (zero? dirty-count)
           show-buttons (case show-buttons ::smart (not clean?) show-buttons)
           [form-t form-v :as form] (invert-fields-to-form edit-merge (e/as-vec edits))
           [tempids _ :as ?cs] (e/call (if genesis FormSubmitGenesis! FormSubmit!)
                                 ::commit :label "commit"  :disabled clean?
                                 :form form
                                 :auto-submit auto-submit ; (when auto-submit dirty-form)
                                 :show-button show-buttons)
           [_ _ :as ?d] (FormDiscard! ::discard :form form :disabled clean? :label "discard" :show-button show-buttons)]
       (e/amb
         (e/for [[btn-q [cmd form-v]] (e/amb ?cs ?d)]
           (case cmd ; does order of burning matter?
             ::discard (let [clear-commits ; clear all concurrent commits, though there should only ever be up to 1.
                             (partial #(run! call %) (e/as-vec tempids))] ; FIXME bug workaround - ensure commits are burnt all at once, calling `(tempids)` should work but crashes the app for now.
                         (case genesis
                           true (if-not discard ; user wants to run an effect on discard (todomvc item special case, genesis discard is always local!)
                                  (case (btn-q) (e/amb)) ; discard now and swallow cmd, we're done
                                  [btn-q ; its a t
                                   (nth discard 0) ; command
                                   (nth discard 1)]) ; prediction

                           ; reset form and BOTH buttons, cancelling any in-flight commit
                           false (if-not discard ; user wants to run an effect on discard (todomvc item special case, genesis discard is always local!)
                                   (case (do (btn-q) (clear-commits) (e/amb))) ; discard now and swallow cmd, we're done
                                   [(fn token
                                      ([] (btn-q) (clear-commits))
                                      ([err] (btn-q err)))
                                    (nth discard 0) ; command
                                    (nth discard 1)])))
             ::commit (let [form-v form-v ; dirty subset
                            tempid btn-q
                            [form-v guess] (if commit
                                             (commit form-v tempid #_dirty-form-guess) ; guess and form would be =
                                             [form-v #_{e dirty-form}]) ; no entity id, only user can guess
                            t (case genesis
                                true (do (reset-active-form-input! dom/node) btn-q) ; this is a q, transfer to list item
                                false btn-q)] ; this is a t
                        [t
                         (if name (edit-monoid name form-v) form-v) ; nested forms as fields in larger forms
                         guess])))

         (e/When debug
           (dom/span (dom/text " " dirty-count " dirty"))
           (dom/pre #_(dom/props {:style {:min-height "4em"}})
             (dom/text (pprint-str form-v :margin 80)))))))))

(defmacro Form! [fields1 & kwargs]
  `(dom/form ; for form "reset" event
     #_(dom/props kwargs) ; todo select dom props vs stage props
     (Form!* ~fields1 ~@kwargs))) ; place fields inside dom/form

(e/defn Reconcile-records [stable-kf sort-key as bs]
  (e/client
    (let [as! (e/as-vec as) ; todo differential reconciliation
          bs! (e/as-vec bs)]
      (js/console.log "Reconcile" {:as as! :bs bs!})
      (->> (merge-with (fn [a b] (or a b)) ; FIXME WIP this is only valid for create-new ; todo deep merge partial prediction (incl. edits)
             (index-by stable-kf as!)
             (index-by stable-kf bs!))
        vals
        (sort-by sort-key)
        #_(drop (count bs!)) ; todo fix glitch
        (e/diff-by stable-kf)))))

(declare Service)

(e/defn PendingController [stable-kf sort-key forms xs]
  (let [!pending (atom {}) ; [id -> guess]
        ps (val (e/diff-by key (e/watch !pending)))]
    (e/for [[t cmd guess :as form] forms #_(Service forms)]
      (prn 'PendingController cmd guess)
      (assert (<= (count guess) 1))
      (let [[tempid guess] (first guess)]
        (case guess
          nil nil ; guess is optional
          ::retract nil ; todo
          (do (swap! !pending assoc tempid (assoc guess ::pending form))
              #_(e/on-unmount #(swap! !pending dissoc tempid)) ; FIXME hangs tab with infinite loop
              ))
        (e/amb)))
    (Reconcile-records stable-kf sort-key xs ps)))

(def effects* {})

(defmacro try-ok [& body]
  `(try ~@body ::ok ; sentinel
     (catch Exception e# (doto ::fail (prn e#)))))

(e/defn Service
  [forms
   & {:keys [delay die] ; don't delay or die todomvc, client-only commands are impacted
      :or {delay 0, die false}}]
  (e/client ; client bias, t doesn't transfer
    (prn `Service (e/Count forms) 'forms (e/as-vec (second forms)))
    (e/for [[t form guess] forms #_(e/Filter some? forms)] ; remove any accidental nils from dom
      #_(case (e/Sleep delay form) (if die [::die]))
      ;(prn 'Service form 'now!) (e/server (prn 'Service form 'now!))
      (let [[effect & args] form
            Effect (effects* effect (e/fn default [& args] (doto ::effect-not-found (prn effect))))
            [status & extras :as res] #_[t form guess db] (e/Apply Effect args)] ; effect handlers span client and server
        ;(prn 'Service form 'result res) (e/server (prn 'Service form 'result res))
        (prn 'final-res res)
        (case status
          nil (prn 'res-was-nil-stop!)
          ::ok (t) ; sentinel, any unrecognized value is an error
          (t ::rejected)) ; feed error back into control for retry affordance
        res))))
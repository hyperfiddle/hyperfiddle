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

#?(:cljs (defn- blur-active-form-input! [form]
           (when-let [focused-input (.-activeElement js/document)]
             (when (.contains form focused-input)
               (.blur focused-input)))))

(e/defn FormDiscard! ; dom/node must be a form
  [directive & {:keys [disabled show-button label] :as props}]
  (e/client
    (dom/On "keyup" #(when (= "Escape" (.-key %)) (.stopPropagation %)
                       (.reset dom/node) nil) nil) ; proxy Esc to form's "reset" event
    (e/amb
      #_(e/When show-button) (Button! directive :disabled disabled :label label) ; todo fix
      (let [e (dom/On "reset" #(do (.log js/console %) (.preventDefault %)
                                 (blur-active-form-input! (.-target %)) %) nil)
            [t err] (e/RetryToken e)]
        (if t [t directive] (e/amb))))))

(e/defn FormSubmit!
  [directive & {:keys [disabled show-button label auto-submit] :as props}]
  (e/client
    (e/amb
      #_(e/When show-button) (Button! directive :disabled disabled :label label)
      (let [e (dom/On "submit" #(do (.preventDefault %) (.stopPropagation %)
                                  (when-not disabled %)) nil)
            [t err] (e/RetryToken (or e auto-submit))]
        (dom/props {:aria-invalid (some? err)})
        (if t [t directive] (e/amb)))))) ; None or Single

(e/defn FormSubmitGenesis!
  "Spawns a new tempid/token for each submit. You must monitor the spawned entity's
lifecycle (e.g. for errors) in an associated optimistic collection view!"
  [directive & {:keys [disabled show-button label #_auto-submit] :as props}] ; auto-submit unsupported
  (e/amb
    #_(e/When show-button) (ButtonGenesis! directive :disabled disabled :label label)
    #_(dom/On-all "submit" #(do (.preventDefault %) (.stopPropagation %)
                            (when-not disabled (doto directive (prn 'FormSubmitGenesis!-submit)))))))

(e/defn Form!*
  ([#_field-edits ; aggregate form state - implies circuit controls, i.e. no control dirty state
    [ts kvs guess :as edits] ; concurrent edits are what give us dirty tracking
    & {:keys [debug commit discard show-buttons auto-submit edit-merge genesis]
       :or {debug false show-buttons true edit-merge merge genesis false}}]
   (e/client
     (let [dirty-form (not-empty (apply edit-merge (e/as-vec kvs))) ; collect fields into form, retain until commit/discard
           ;dirty-form-guess (apply merge (e/as-vec guess)) ; todo collisions
           form-t (let [ts (e/as-vec ts) #_(map first field-edits)]
                    (fn token ; fresh if ts changes (should fields be disabled when commit pending?)
                      ([] (doseq [t ts] (t)))
                      #_([err] (doseq [t ts] (t err ::keep))))) ; we could route errors to dirty fields, but it clears dirty state
           dirty-count (e/Count edits)
           clean? (zero? dirty-count)
           [tempids _ :as ?cs] (e/call (if genesis FormSubmitGenesis! FormSubmit!)
                                ::commit :label "commit"  :disabled clean?
                                :auto-submit (when auto-submit dirty-form)
                                :show-button show-buttons )
           [dt _ :as ?d] (FormDiscard! ::discard :disabled clean? :label "discard" :show-button show-buttons)
           discard! (if (e/Some? tempids) ; refering to tempids JOINs this value to nothing when clicking discard before commit.
                      (fn [] (when-not genesis (tempids)) (dt) (form-t))
                      (fn [] (dt) (form-t)))] ; reset controlled form and both buttons, cancelling any in-flight commit
       (e/When debug (dom/span (dom/text " " dirty-count " dirty")))
       (e/amb
         (e/for [[btn-t cmd] (e/amb ?cs ?d)]
           (case cmd ; does order of burning matter?
             ::discard (if discard ; user wants to run an effect on discard (todomvc item special case, genesis discard is always local!)
                         [(fn token ; emit discard and stay busy (lag!)
                            ([] (discard!)) ; user says discard ok
                            ([err] (btn-t err))) ; user rejected discard
                          (nth discard 0) ; command
                          (nth discard 1)] ; prediction
                         (case (discard!) (e/amb))) ; otherwise discard now and swallow cmd, we're done
             ::commit (let [[dirty-form dirty-form-guess] (if commit (commit (e/snapshot dirty-form) btn-t ; tempid
                                                                       #_dirty-form-guess) ; guess and form would be =
                                                            [dirty-form #_{e dirty-form}])] ; no entity id, only user can guess
                        (case genesis
                          true (do (form-t) ; abandon entity and clear form, ready for next submit -- triggers second commit, hacked w/ snapshot
                                 [btn-t ; this is a q, transfer to list item
                                  dirty-form dirty-form-guess])
                          false [(fn token
                                   ([] (btn-t) (when-not genesis (form-t))) ; commit ok, reset controlled form
                                   ([err] (btn-t err) #_(form-t err))) ; leave dirty fields dirty, activates retry button
                                 dirty-form dirty-form-guess]))))

         (e/When debug
           (dom/pre #_(dom/props {:style {:min-height "4em"}})
             (dom/text (pprint-str dirty-form :margin 80)))))))))

(defmacro Form! [fields1 & kwargs]
  `(dom/form ; for form "reset" event
     #_(dom/props kwargs) ; todo select dom props vs stage props
     (Form!* ~fields1 ~@kwargs))) ; place fields inside dom/form

(e/defn Reconcile-records [stable-kf sort-key as bs]
  (e/client
    (let [as! (e/as-vec as) ; todo differential reconciliation
          bs! (e/as-vec bs)]
      (->> (merge ; todo deep merge partial predictions
             (index-by stable-kf as!)
             (index-by stable-kf bs!))
        vals
        (sort-by sort-key)
        #_(drop (count bs!)) ; todo fix glitch
        (e/diff-by stable-kf)))))

(declare Service)

(e/defn PendingController [kf sort-key forms xs]
  (let [!pending (atom {}) ; [id -> guess]
        ps (val (e/diff-by key (e/watch !pending)))]
    (e/for [[t cmd guess :as form] forms #_(Service forms)]
      #_(prn 'PendingController t cmds predictions)
      (assert (= 1 (count guess)))
      (let [[tempid guess] (first guess)]
        #_(prn 'pending-cmds cmds)
        (case guess
          ::retract nil ; todo
          (swap! !pending assoc tempid (assoc guess ::pending form)))
        (e/on-unmount #(swap! !pending dissoc tempid))
        (e/amb)))
    (Reconcile-records kf sort-key xs ps)))

(def *effects* {})

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
            Effect (*effects* effect (e/fn default [& args] (doto ::effect-not-found (prn effect))))
            res #_[t form guess db] (e/Apply Effect args)] ; effect handlers span client and server
        ;(prn 'Service form 'result res) (e/server (prn 'Service form 'result res))
        (prn 'final-res res)
        (case res
          nil (prn 'res-was-nil-stop!)
          ::ok (t) ; sentinel, any unrecognized value is an error
          (t ::rejected)))))) ; feed error back into control for retry affordance
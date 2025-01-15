(ns hyperfiddle.ui.wizard
  (:require
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.rcf :as rcf :refer [tests]]
   [hyperfiddle.ui.stepper-large :as step]
   [contrib.orderedmap :as ord]
   [hyperfiddle.router4 :as r]
   [hyperfiddle.electric-forms0]
   [hyperfiddle.electric-forms1]
   [hyperfiddle.ui.spinner]
   [heroicons.electric3.v24.solid :as icons]))

;;- A wizard is
;;  - a collection of forms
;;  - split into multiple views (aka. steps)
;;  - to focus user attention on one form (step) at a time
;;  - steps are
;;    - sequenced
;;    - conditional – user input has the ability to alter sequence of steps
;;    - validated – "next" button might be disabled until current step form is valid
;;  - form submission modes
;;    - as a whole
;;      – all forms are collected into one tx
;;      - user must restart from step 0 if state is lost
;;    - on each step
;;      - allows resuming the wizard
;;      - user decide of persistance mode – e.g. store as a draft vs persist partial state to db
;;    - à la carte
;;      - some steps might persist
;;      - some might impact the step sequences
;;      - some might be cosmetic only
;;
;;- Use case : Block sub from school
;;  - step 1 :: fill block-sub-from-school-form
;;  - step 2 :: decide if notification should be sent
;;    - Send blocked notification to:
;;      - [✓] School admins
;;      - [ ] School main contact
;;      - [ ] Blocked sub
;;  - Possible behaviors:
;;    - step by step
;;      - run step 1, if it succeeds: redirect to step 2
;;      - run step 2, if it succeeds: done.
;;    - collect then run (all at once)
;;      - fill-in step1, redirect to step2
;;      - fill-in step2, run
;;        - first run step1
;;          - if it succeeds run step 2
;;            - it it succeeds: done
;;            - if it fails redirect to step 2
;;          - if it fails redirect to step 1, keep step2 state
;;            - fill in step 1, redirect to pre-filled step2
;;    - single-tx (!)
;;      - collect step1 and step2
;;      - submit a single tx
;;        - assuming tx will succeed or fail as a whole
;;          - without partial state (partial fire missile)
;;            In our case we alter db state + send notifications
;;            notifications must not fire unless db state is validated
;;
;;* TODO
;;  - [ ] model edits accumulation

(defn successive-pairs [coll] (map vector coll (rest coll)))

(tests
  (successive-pairs nil)        := ()
  (successive-pairs ())         := ()
  (successive-pairs '(1))       := ()
  (successive-pairs '(1 2))     := '((1 2))
  (successive-pairs '(1 2 3))   := '((1 2) (2 3))
  (successive-pairs '(1 2 3 4)) := '((1 2) (2 3) (3 4))
  )

(defn initial-state [current-step-name steps] ; [[:a F] [:b F]]
  (let [step-names (map first steps)]
    {::step    current-step-name
     ::forward (into {} (successive-pairs step-names))
     ::back    (into {} (successive-pairs (reverse step-names)))
     ::steps   (into (ord/ordered-map) steps)
     }))

(tests
  (initial-state :a [[:a nil] [:b nil] [:c nil]])
  := {::step    :a
      ::forward {:a :b, :b :c}
      ::back    {:c :b, :b :a}
      ::steps   (ord/ordered-map :a nil :b nil :c nil)})

(defn next-step [{::keys [step forward]}] (forward step))
(defn prev-step [{::keys [step back]}] (back step))
(defn forward [state]  (update state ::step (fn [step] (or (next-step state) step))))
(defn back [state] (update state ::step (fn [step] (or (prev-step state) step))))

(tests
  (let [state (initial-state :a [[:a "step a"] [:b "step b"] [:c "step c"]])]
    (::step state)    := :a
    (next-step state) := :b
    (prev-step state) := nil
    (let [state (forward state)]
      (::step state)    := :b
      (next-step state) := :c
      (prev-step state) := :a
      (let [state (forward state)]
        (::step state)    := :c
        (next-step state) := nil
        (prev-step state) := :b
        (let [state (forward state)]
          (::step state)    := :c
          (next-step state) := nil
          (prev-step state) := :b)
        (let [state (back state)]
          (::step state)    := :b
          (next-step state) := :c
          (prev-step state) := :a
          (let [state (back state)]
            (::step state)    := :a
            (next-step state) := :b
            (prev-step state) := nil
            (let [state (back state)]
              (::step state)    := :a
              (next-step state) := :b
              (prev-step state) := nil)))))))

(defn subsequents [direction state] ; back | forward
  (map ::step (take-while some? (drop 1 (iterate #(let [x (direction %)] (when-not (= x %) x)) state)))))

(tests
  (let [state (initial-state :a [[:a "step a"] [:b "step b"] [:c "step c"]])]
    (subsequents forward state) := '(:b :c)
    (subsequents forward (forward state)) := '(:c)
    (subsequents forward (forward (forward state))) := '()
    (subsequents back (forward (forward state))) := '(:b :a)
    (subsequents back (forward state)) := '(:a)
    (subsequents back state) := '()
    ))

(defn step [{::keys [step steps]}] (get steps step))

(tests
  (let [state (initial-state :a [[:a "step a"] [:b "step b"] [:c "step c"]])]
    (step state) := "step a"
    (step (forward state)) := "step b"))

(defn ->state []
  (let [!state (atom nil)]
    (fn [initial-state]
      (doto !state (reset! initial-state)))))

(e/declare !state)
(e/declare state)

(defn goto!-impl [!state step-name] (swap! !state assoc ::step step-name))
(e/declare goto!)
(e/declare next-step!)
(e/declare prev-step!)
(e/declare complete!)

(defn last-step? [state] (empty? (subsequents forward state)))

(defn current-route-step [route state]
  (let [steps (set (keys (::steps state)))]
    (steps (if (sequential? route) (first route) route))))

(defn hijacked-route [step route] (cons step (if (sequential? route) route (list route))))

(comment
  (hijacked-route :step '()) := '(:step)
  (hijacked-route :step '["foo"]) := '(:step "foo")
  (hijacked-route :step '{}) := '(:step {})
  )

(e/defn Hijack-route! [Body]
  (if-some [current (current-route-step r/route state)]
    (do (when-not (= current (::step state))
          ;; FIXME we are driving the router instead of being driven by it.
          ;; FIXME use proper navigation, not replacestate
          (r/ReplaceState! ['. (hijacked-route (::step state) (rest r/route))]))
      (r/pop (Body)))
    (r/ReplaceState! ['. (hijacked-route (::step state) r/route)])))

(comment
  (current-route-step [42] {::steps {"foo" nil}}) := nil
  (current-route-step ["foo"] {::steps {"foo" nil}}) := "foo"
  )

(e/defn CmdTracker [[actual-commit expected-commit] t success?]
  (dom/ul (dom/props {:class "mt-4" :style {:padding-left "14rem"}}) ; demo only
    (dom/li
      (dom/props {:class "flex gap-4 items-center"})
      (dom/span
        (dom/props {:class "w-6 aspect-square flex items-center justify-center"})
        (cond
          t (hyperfiddle.ui.spinner/spinner)
          (true? success?) (icons/check-circle (dom/props {:class "text-green-500"}))
          (false? success?) (icons/x-circle (dom/props {:class "text-red-400"}))
          () (icons/pencil-square (dom/props {:class "w-5 self-center justify-self-center text-gray-400"}))))
      (dom/pre (dom/text (pr-str (or (hyperfiddle.electric-forms1/debug-cleanup-form-edit (first actual-commit))
                                   (first expected-commit)) ; discard guess
                           ))))))

(e/defn Wizard [{::keys [step steps]} Body]
  (binding [!state (#(doto (atom nil) (reset! %)) (initial-state (or step (ffirst steps)) steps))]
    (let [!commit (atom [nil nil])
          !success? (atom nil)] ; inspect form edit ahead of commit action, as a side channel - arguable
      (binding [state      (e/watch !state)
                next-step! #(do (reset! !commit [nil nil]) (reset! !success? nil) (swap! !state (comp forward (fn [state] (dissoc state ::complete)))) nil)
                prev-step! #(do (reset! !commit [nil nil]) (reset! !success? nil) (swap! !state (comp back (fn [state] (dissoc state ::complete)))) nil)
                goto!      #(do (goto!-impl !state %) nil)
                complete!  #(do (swap! !state assoc ::complete true) nil)]
        (Hijack-route!
          (e/fn []
            (let [edits-or-v (binding [hyperfiddle.electric-forms0/InspectFormCommit (e/fn [commit] (swap! !commit assoc 1 commit))
                                       hyperfiddle.electric-forms1/InspectFormCommit (e/fn [commit] (swap! !commit assoc 1 commit))]
                               (Body (hyperfiddle.ui.wizard/step state) (::complete state)))]
              (let [[t & edits :as x] (first (e/as-vec edits-or-v))]
                (CmdTracker (e/watch !commit) t (e/watch !success?))
                (if (e/amb ; detect form presence, map success commit to "next step", otherwise passes values through
                      (dom/On "reset" #(do (swap! !commit assoc 0 nil) true) false {:capture true})
                      (dom/On "submit" (constantly true) false {:capture true}))
                  (if t
                    (do (swap! !commit assoc 0 edits)
                        (into [(fn
                                 ([] (t) (reset! !success? true) (complete!) #_(next-step!))
                                 ([err] (reset! !success? false) (t err)))]
                          edits))
                    (e/amb))
                  edits-or-v)))))))))

(e/defn WizardStepper []
  (step/stepper
    (dom/props {:class "space-y-2"})
    (e/for [[step & _] (e/diff-by key (::steps state))]
      (let [current? (= step (::step state))
            completed? (some #{step} (subsequents back state))]
        (step/step {::step/current? current?
                    ::step/completed? completed?}
          (dom/On "click" (fn [^js e] (.preventDefault e) (goto! step)) nil) ; TODO move onto <a>
          (dom/props {:class ["text-nowrap text-sm font-medium" (cond
                                                                  completed? "text-gray-800"
                                                                  current? "text-indigo-600"
                                                                  () "text-gray-500")]})
          (dom/text step))))))

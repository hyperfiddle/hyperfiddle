(ns hyperfiddle.ui.wizard3
  (:require
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.router4 :as r]))

;; (e/defn WizardStepper []
;;   (step/stepper
;;     (dom/props {:class "space-y-2"})
;;     (e/for [[step & _] (e/diff-by key (::steps state))]
;;       (let [current? (= step (::step state))
;;             completed? (some #{step} (subsequents back state))]
;;         (step/step {::step/current? current?
;;                     ::step/completed? completed?}
;;           (dom/On "click" (fn [^js e] (.preventDefault e) (goto! step)) nil) ; TODO move onto <a>
;;           (dom/props {:class ["text-nowrap text-sm font-medium" (cond
;;                                                                   completed? "text-gray-800"
;;                                                                   current? "text-indigo-600"
;;                                                                   () "text-gray-500")]})
;;           (dom/text step))))))

(defn ascending [steps] (when (seq steps) (concat steps (list (last steps)))))
(defn descending [steps] (when (seq steps) (concat (reverse steps) (list (first steps)))))
(defn followings [steps current-step] (rest (drop-while (complement #{current-step}) (ascending steps))))
(defn precedings [steps current-step] (rest (drop-while (complement #{current-step}) (descending steps))))
(defn next-step [steps current-step] (first (followings steps current-step)))
(defn prev-step [steps current-step] (first (precedings steps current-step)))
(defn step? [steps step] ((set steps) step))
(defn start? [steps step] (= step (prev-step steps step)))
(defn end? [steps step] (= step (next-step steps step)))
(defn ensure-step [steps step] (or (step? steps step) (first steps)))

(comment
  (step? () nil) := nil
  (step? [:a :b :c] :a)
  (step? ["a" :b :c] "a")
  (step? [:a :b :c] :b)
  (step? [:a :b :c] :c)
  (step? [:a :b :c] :d)
  (ensure-step () nil) := nil
  (ensure-step [:a] nil) := :a
  (ensure-step [:a] :b) := :a
  (prev-step () :a)
  (prev-step [:a :b :c] :a)
  (prev-step [:a :b :c] :b)
  (prev-step [:a :b :c] :c)
  (next-step [:a :b :c] :a)
  (next-step [:a :b :c] :b)
  (next-step [:a :b :c] :c)

  (ascending ())
  (ascending [:a])
  (descending ())
  (descending [:a])
  (followings () :a)
  (followings [:a] :a)
  (followings [:a :b] :a)
  )

(defn collapse-nil-route-tail [route]
  (if (sequential? route)
    (reverse (drop-while nil? (reverse route)))
    route))

(e/defn Wizard [{:keys [steps] :or {steps ()}} Body]
  (e/client
    (r/pop
      (let [[step & route-state] r/route]
        (if-not (step? steps step)
          (r/ReplaceState! ['. (cons (ensure-step steps step) route-state)])
          (forms/Interpreter {::next (e/fn [state] (r/Navigate! ['. (cons (next-step steps step) (collapse-nil-route-tail (or state route-state)))]) [::forms/ok])
                              ::prev (e/fn [state] (r/Navigate! ['. (cons (prev-step steps step) (collapse-nil-route-tail (or state route-state)))]) [::forms/ok])
                              ::goto (e/fn [[step state]] (r/Navigate! ['. (cons step (collapse-nil-route-tail (or state route-state)))]) [::forms/ok])}
            (r/focus '[..]
              (Body step route-state #_(start? steps step) #_(end? steps step)))))))))

(e/defn Stepper [& {:keys [steps step]}]
  (dom/ol (dom/props {:role "tablist"})
    (e/for [step' (e/diff-by {} steps)]
      (dom/li
        (forms/Button! [::goto [step']] :label (name step') :role "tab" :aria-selected (= step step'))
        ))))
(ns hyperfiddle.navigator6.vertical
  (:require [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.electric-forms5 :as forms]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.hfql1.analyzer :as ana]
            [hyperfiddle.hfql1.function :refer [flip]]
            [hyperfiddle.hfql1.cofree :refer [unwrap]]
            [hyperfiddle.hfql1.monoid :refer [mappend]]
            [hyperfiddle.hfql2 :as hfql]
            [hyperfiddle.navigator6.rendering :as rendering]
            [hyperfiddle.navigator6.search :as search]
            [hyperfiddle.navigator6.selection :as selection]
            [hyperfiddle.navigator6.sorting :as sort]
            [hyperfiddle.navigator6.utils :as utils :refer [in glitch-guard caching-nth]]))

(def ^:private COLLECTION-LIMIT 10000)

(e/defn ColumnPicker [stepT]
  (e/server
    (when-let [pull-step (selection/select-pull-step stepT)]
      (let [stepT+ (hfql/augment-with-suggestions! pull-step)
            childrenT (remove (comp ::ana/recursive-child hfql/metas) (unwrap stepT+))
            static-columns (remove (comp ::hfql/suggested hfql/metas) childrenT)
            suggested-columns (filter (comp ::hfql/suggested hfql/metas) childrenT)
            pre-checked (#(or (hfql/pull-has-star? stepT+) (empty? static-columns)))
            from-spec (e/as-vec (e/for [columnT (e/diff-by hfql/symbolic-edge static-columns)]
                                  (e/When
                                      (dom/span
                                        (forms/Checkbox* true :label (e/server (str (hfql/symbolic-edge columnT)))
                                          :title (e/server (hfql/describe-formatted columnT))))
                                    columnT)))
            suggested (e/as-vec (e/for [columnT (e/diff-by hfql/symbolic-edge suggested-columns)]
                                 (e/When
                                     (dom/span
                                       (forms/Checkbox* pre-checked :label (e/server (str (hfql/symbolic-edge columnT)))
                                         :title (e/server (hfql/describe-formatted columnT))))
                                   columnT)))]
        (prn "pre-checked" pre-checked `(~'or ~(hfql/pull-has-star? stepT+) ~(empty? static-columns)) (count static-columns) (count suggested-columns) (map (comp ::hfql/suggested hfql/metas) childrenT))
        (mappend from-spec suggested)))))

(defn- select-columns "Similar to `cc/select-keys`. Given a row containing columns, and a collection of columns to keep, filter the row; excluding non-selected columns. Return a collection of columns."
  [rowT columnsT]
  (when rowT
    (let [id (comp ::ana/id hfql/metas)
          selected? (set (map id columnsT))]
      (filter (comp selected? id) (unwrap rowT)))))

(e/defn RenderVerticalMatrixBody [row-height columnsT rowsT selection]
  (e/client
    (let [fast-nth (e/server (caching-nth rowsT))]
      (->> (selection/WithLocalSelectedIndex `Select!
             (e/fn [selected-index !selected-index]
               (->> (forms/TablePicker! ::selection selected-index (e/server (count rowsT))
                      (e/fn [row-index]
                        (let [rowT (e/server (fast-nth row-index nil))
                              exists? (e/server (some? rowT))]
                          (e/Reconcile
                            (e/When exists?
                              (e/server (selection/select-row-if-match-url! !selected-index row-index (= rowT (second selection)))) ; TODO rename / clarify
                              (e/server
                                (e/for [columnT (e/diff-by hfql/id ((glitch-guard select-columns) rowT columnsT))] ; row × column cartesian product
                                  (rendering/Render* columnT)))))))
                      :row-height row-height
                      :column-count (e/server (max 1 (count columnsT)))
                      :as :tbody)
                 (forms/Parse (e/fn [{selected-index ::selection}] [`Select! selected-index])))))
        (utils/InterceptCommand `Select! (e/fn [[_ selected-index]]
                                           (e/server (e/Reconcile
                                                       (if-let [selected-rowT (fast-nth selected-index nil)]
                                                         [`selection/SetSelection! (utils/Delay selected-rowT)]
                                                         [`Noop])))))
        (forms/Interpreter {`Noop (e/fn [] [::forms/ok])})))))

(e/defn RenderVerticalMatrix
  ;; Even though a matrix is transposable, the DOM makes it hard to transpose a table. So we split vertical from horizontal rendering for our own sanity. G didn't find a robust CSS-only solution.
  [row-height columnsT rowsT selection]
  (e/client
    (dom/table
      (dom/props {:style {:--row-height (str row-height "px"),
                          :--column-count (e/server (max 1 (count columnsT)))
                          :--row-count (e/server (max 1 (count rowsT)))}})
      (e/amb
        (dom/thead
          (dom/tr
            (let [columns-index (e/server (into {} (map (juxt hfql/id identity)) columnsT))]
              (e/for [edge (e/diff-by identity (e/server (map hfql/id columnsT)))]
                (let [columnT (e/server (get columns-index edge))
                      form (e/server (str (hfql/symbolic-edge columnT)))]
                  (dom/th (dom/props {:scope :col :title (e/server (hfql/describe-formatted columnT))})
                          #_(rendering/RenderTooltip ::hfql/ColumnHeaderTooltip nil columnT nil)
                          (dom/text form)
                          (e/client
                            (let [!direction (atom (cycle ['> '<]))
                                  direction (dom/On "click" (fn [_event] (first (swap! !direction rest))) nil)
                                  [t err] (e/Token direction)]
                              (e/Reconcile (e/When t [t [`sort/Sort! (e/server (utils/Delay columnT)) direction]]))))))))))
        (RenderVerticalMatrixBody row-height columnsT rowsT selection)))))

(e/defn RenderStepVertical [stepT]
  (dom/fieldset (dom/props {:class "entity-children hyperfiddle-navigator6__block"})
    (let [legend (dom/legend (dom/props {:style {:display :flex, :flex-wrap :wrap}})
                   (rendering/Step-Title stepT)
                   dom/node)
          columnsT (e/server (in legend (ColumnPicker stepT)))
          stepT (e/server (selection/get-rows-container stepT))]
      (search/Filtered (e/server (search/eager-pull-search-sort-BIG-O-N-SQUARE))
        (e/server (map hfql/augment-with-suggestions! ; ensure search on all columns
                    (unwrap stepT))) ; rowsT
        (e/fn [rowsT]
          (in legend
            (dom/span (dom/props {:style {:order -1}})
                      (dom/text (rendering/format-collection-count (e/server (bounded-count COLLECTION-LIMIT rowsT)) ; 𝓞(n)
                                  COLLECTION-LIMIT))))
          (e/amb
            (in legend (dom/span (dom/props {:style {:order -2}}) (search/SearchInput! false)))
            (sort/Sorted rowsT
              (e/fn [rowsT]
                (RenderVerticalMatrix 24 columnsT rowsT selection/*local-selection)))))))))
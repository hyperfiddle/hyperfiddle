(ns hyperfiddle.navigator6.matrix
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

#?(:cljs
   (defn- find-clicked-cell-or-row [^js dom-event]
     (let [target (.-target dom-event)]
       (when-let [closest-td (.closest target "td")]
         (if-let [cellT (.-data-cellT closest-td)]
           cellT
           (when-let [closest-tr (.closest target "tr")]
             (if-let [rowT (.-data-rowT closest-tr)]
               rowT
               nil)))))))

(e/defn RenderMatrixBody [row-height columnsT rowsT selection]
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
                              (e/client (set! (.-data-rowT ^js dom/node) (e/server (utils/Delay rowT))) nil)
                              (dom/td (dom/props {:style {:min-width (str row-height "px")}}) (forms/Checkbox* (= selected-index row-index) :type :radio :style {:display :inline-block}))
                              (e/server (selection/select-row-if-match-url! !selected-index row-index (= ((glitch-guard hfql/value-at-point) rowT) ((glitch-guard hfql/value-at-point) (second selection))))) ; TODO cell selection
                              (e/server
                                (e/for [columnT (e/diff-by hfql/id ((glitch-guard select-columns) rowT columnsT))] ; row × column cartesian product
                                  (rendering/Render* columnT)))))))
                      :row-height row-height
                      :column-count (e/server (max 1 (count columnsT)))
                      :as :tbody)
                 (forms/Parse (e/fn [{selected-index ::selection, event ::forms/event}] [`Select! selected-index event])))))
        (utils/InterceptCommand `Select! (e/fn [[_ selected-index event]]
                                           (let [selectionT (e/client (find-clicked-cell-or-row event))]
                                             (e/client (prn "selectionT" selected-index selectionT))
                                             (e/server (e/Reconcile
                                                         (if selectionT
                                                           [`selection/SetSelection! selectionT]
                                                           [`Noop]))))))
        (forms/Interpreter {`Noop (e/fn [] [::forms/ok])})))))

(e/defn RenderMatrix
  ;; Even though a matrix is transposable, the DOM makes it hard to transpose a table. So we split vertical from horizontal rendering for our own sanity. G didn't find a robust CSS-only solution.
  [row-height columnsT rowsT selection transpose?]
  (e/client
    (dom/table
      (dom/props {:style {:--row-height (str row-height "px"),
                          :--column-count (e/server (max 1 (count columnsT)))
                          :--row-count (e/server (max 1 (count rowsT)))}
                  :class [(#(if transpose? "transpose" ""))]})
      (let [table dom/node]
        (e/amb
          (dom/thead
            (dom/tr
              (dom/th (dom/props {:class "diagonal" :scope :col :style {:width "100%"}})
                      (dom/On* "click" (fn [_event] (-> table .-classList (.toggle "transpose")) nil) nil))
              (let [columns-index (e/server (into {} (map (juxt hfql/id identity)) columnsT))]
                (e/for [edge (e/diff (e/server (map hfql/id columnsT)))]
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
          (RenderMatrixBody row-height columnsT rowsT selection))))))

(declare css)

(e/defn RenderStep [stepT & [transpose?]]
  (e/client (dom/style (dom/text css)))
  (dom/fieldset (dom/props {:class "entity-children hyperfiddle-navigator6__block"})
                (let [legend (dom/legend (dom/props {:style {:display :flex, :flex-wrap :wrap}})
                                         (rendering/Step-Title stepT)
                                         dom/node)
                      columnsT (e/server (in legend (ColumnPicker stepT)))
                      stepT (e/server (selection/get-rows-container stepT))
                      rowsT (e/server (#(if (= ::ana/pull (::ana/type (hfql/metas stepT))) ; FIXME smell
                                          [stepT]
                                          (unwrap stepT))))]
                  (search/Filtered (e/server (search/eager-pull-search-sort-BIG-O-N-SQUARE))
                    (e/server (map hfql/augment-with-suggestions! ; ensure search on all columns
                                rowsT)) ; rowsT
                    (e/fn [rowsT]
                      (in legend
                        (dom/span (dom/props {:style {:order -1}})
                                  (dom/text (rendering/format-collection-count (e/server (bounded-count COLLECTION-LIMIT rowsT)) ; 𝓞(n)
                                              COLLECTION-LIMIT))))
                      (e/amb
                        (in legend (dom/span (dom/props {:style {:order -2}}) (search/SearchInput! false)))
                        (sort/Sorted rowsT
                          (e/fn [rowsT]
                            (RenderMatrix 24 columnsT rowsT selection/*local-selection transpose?)))))))))


(def css
  "
table.transpose{
  grid-template-columns: min-content repeat(var(--row-count), auto);
  grid-auto-rows: var(--row-height);

  background: repeating-linear-gradient( to bottom, #f2f2f2 0, #f2f2f2 var(--row-height), transparent var(--row-height), transparent calc(2 * var(--row-height)) );
  background-attachment: local;
  background-size: 100% calc(2 * var(--row-height));

  height: calc(15 * var(--row-height));
  resize: vertical;

  overflow-y: auto;
  thead{
    display: grid;
    grid-column: 1;
    grid-template-columns: subgrid!important;
    grid-row: 1 / -1;
    z-index: 1;
    grid-auto-rows: inherit;
    height: 100%;
    tr{
      display: contents;
      th{
        display: unset;
        text-align: right;
      }
    }

}
  tbody{
    background: none;
    grid-row: 1!important;
    grid-column: 2 / -1!important;

    resize: none;
    tr{
      --column: calc(1 + var(--row-index));
      td {
        grid-row: var(--row-index);
        grid-column: var(--column);
      }
    }
    tr:hover{
      td:not(:hover) {
        background-color: unset;
      }
    }
    td:hover{
      background-color: #ddd;
    }
  }
}


.diagonal {
  background: linear-gradient(to bottom left,
    transparent 49.5%,
    black 49.5%,
    black 50.5%,
    transparent 50.5%
  );
}

")
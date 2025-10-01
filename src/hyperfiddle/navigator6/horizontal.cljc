(ns hyperfiddle.navigator6.horizontal
  (:require
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.hfql1.analyzer :as ana]
   [hyperfiddle.hfql1.cofree :refer [unwrap]]
   [hyperfiddle.hfql2 :as hfql]
   [hyperfiddle.navigator6.rendering :as rendering]
   [hyperfiddle.navigator6.search :as search]
   [hyperfiddle.navigator6.selection :as selection :refer [select-pull-step]]
   [hyperfiddle.navigator6.utils :as utils :refer [caching-nth glitch-guard in]]))

(def ^:private COLLECTION-LIMIT 10000)

(defn- select-horizontal-children [stepT] ; FIXME smell
  (case (::ana/type (hfql/metas stepT))
    ::ana/pull [stepT]
    ::ana/traversal (unwrap stepT)
    (throw (ex-info "Can't extract rows out of this hfql step" {:type (::ana/type (hfql/metas stepT))}))))

(defn- seed-cell [rowT columnT]
  (if (= ::ana/pull (::ana/type (hfql/metas columnT)))
    rowT
    (hfql/seed (hfql/scope rowT) columnT)))

(declare css)

(e/defn RenderHorizontalMatrixBody [row-height columnsT rowsT selection RenderHeaderCell]
  (e/client
    (dom/style (dom/text css))
    (let [|rowsT> (e/server (e/diff-by hfql/identify rowsT))
          fast-nth (e/server (caching-nth columnsT))]
      (->> (selection/WithLocalSelectedIndex `Select!
             (e/fn [selected-index !selected-index]
               (->> (forms/TablePicker! ::selection selected-index (e/server (count columnsT))
                      (e/fn [column-index]
                        (e/server
                          (let [columnT (fast-nth column-index nil)]
                            (e/Reconcile
                              (when (some? columnT)
                                (dom/th (dom/props {:scope :row :style {:grid-column 1}}) (RenderHeaderCell columnT))
                                (selection/select-row-if-match-url! !selected-index column-index (= (hfql/symbolic-edge columnT) (first selection))) ; TODO rename / clarify
                                (e/for [rowT |rowsT>] ; row × column cartesian product
                                  (rendering/Render* ((glitch-guard seed-cell) columnT rowT))))))))
                      :row-height row-height
                      :column-count (e/server (max 1 (count rowsT)))
                      #_#_:as :tbody
                      :class "horizontal")
                 (forms/Parse (e/fn [{selected-index ::selection}] [`Select! selected-index])))))
        (utils/InterceptCommand `Select! (e/fn [[_ selected-index]]
                                           (e/server (e/Reconcile
                                                       (if-let [selected-columnT (fast-nth selected-index nil)]
                                                         [`selection/SetSelection! (utils/Delay selected-columnT)]
                                                         [`Noop])))))
        (forms/Interpreter {`Noop (e/fn [] [::forms/ok])})))))

(e/defn RenderHorizontalMatrix
  ;; Even though a matrix is transposable, the DOM makes it hard to transpose a table. So we split vertical from horizontal rendering for our own sanity. G didn't find a robust CSS-only solution.
  [row-height columnsT rowsT selection]
  (e/client
    (RenderHorizontalMatrixBody row-height columnsT rowsT selection
      (e/fn HeaderCell [columnT]
        (e/server
          (let [form (hfql/symbolic-edge columnT)]
            (dom/props {:title (str form)})
            (rendering/RenderTooltip ::hfql/ColumnHeaderTooltip nil columnT nil)
            (dom/text (str form))))))))

(e/defn RenderStepHorizontal [stepT]
  (dom/fieldset
    (dom/props {:class "entity hyperfiddle-navigator6__block"})
    (let [legend (dom/legend (dom/props {:style {:display :grid, :grid-template-areas "'title search count'"}})
                             (rendering/Step-Title stepT)
                             dom/node)
          columnsT (e/server ((glitch-guard unwrap) (hfql/augment-with-suggestions! (select-pull-step stepT))))]
      (search/Filtered
        (e/server (search/eager-search-kvs-BIG-O-N-SQUARE))
        columnsT
        (e/server ((glitch-guard select-horizontal-children) stepT)) ; rowsT – TODO review, this is a smell
        (e/fn [columnsT-filtered rowsT-filtered]
          (in legend
            (dom/span (dom/props {:style {:grid-area "count"}})
                      (dom/text (rendering/format-collection-count (e/server (bounded-count COLLECTION-LIMIT columnsT-filtered)) ; 𝓞(n)
                                  COLLECTION-LIMIT))))
          (e/amb
            (in legend (search/SearchInput! false))
            (RenderHorizontalMatrix 24 columnsT-filtered rowsT-filtered
              selection/*local-selection
              )))))))

(def css
  "

.hyperfiddle-electric-forms5__table-picker.horizontal{
  overflow-x: auto;
  grid-template-columns: fit-content(25ch) repeat(var(--column-count), auto) !important;

  tr :is(th,td) {
    grid-row: auto;
    contain: layout;
    width: auto;
  }
  tr th{
    position: sticky;
    left: 0;
    z-index: 1;
    text-align: left;
  }
  tr:nth-child(even):not([aria-selected=\"true\"], [aria-checked=\"true\"]) th {
    background-color: #f2f2f2;
  }
  tr:nth-child(odd):not([aria-selected=\"true\"], [aria-checked=\"true\"]) th {
    background-color: white;
  }

}

:where(.hyperfiddle-electric-forms5__table-picker.horizontal tr:hover:has(*) :is(td, th)) { background-color: inherit; }
.hyperfiddle-electric-forms5__table-picker.horizontal tr:has(td:hover) :is(td:hover, th) { background-color: #ddd; }
"
  )
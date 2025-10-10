(ns hyperfiddle.navigator6.matrix2
  (:require
   [clojure.math :as math]
   [contrib.data :refer [clamp]]
   [contrib.missionary-contrib :as mx]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window IndexRing]]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.hfql1.analyzer :as ana]
   [hyperfiddle.hfql1.cofree :refer [unwrap]]
   [hyperfiddle.hfql1.monoid :refer [mappend]]
   [hyperfiddle.hfql2 :as hfql]
   [hyperfiddle.navigator6.rendering :as rendering]
   [hyperfiddle.navigator6.search :as search]
   [hyperfiddle.navigator6.selection :as selection]
   [hyperfiddle.navigator6.sorting :as sort]
   [hyperfiddle.navigator6.utils :as utils :refer [in]]
   [missionary.core :as m]))

(defn- maybe [f] (fn [x] (when x (f x))))

(e/defn Command [value-fn event after-ack]
  (let [[t err] (e/Token event)]
    (e/Reconcile
      (e/When t
        [(forms/after-ack t (partial after-ack event)) (value-fn event)]))))

(e/defn MatrixHeader [columnsT]
  (dom/thead
    (dom/tr
      (e/amb
        (dom/th (dom/props {:class "transpose-affordance matrix__diagonal" :aria-label "transpose" :style {:width "100%"}})
                (Command (constantly [`Transpose!]) (dom/On* "click" identity nil) (constantly nil)))
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
                          (e/Reconcile (e/When t [t [`sort/Sort! (e/server (utils/Delay columnT)) direction]]))))))))))))

#?(:cljs
   (defn- dispatch-event! [direction ^js node type detail]
     (let [[capture bubbles] (case direction
                               :both [true true]
                               :up   [false true]
                               :down [true false])]
       (.dispatchEvent node (new js/CustomEvent type #js{:capture capture, :bubbles bubbles, :detail detail})))))

(defn- cell-selected? [cellT local-selection]
  (let [[selected-column selected-cellT] local-selection]
    (when (and cellT selected-cellT) ; FIXME should be able to compare by direct equality without identifying
      (or (= cellT selected-cellT) ; Ideal path, always false for now. Should suffice to the check. This is what we want.
        (and (= selected-column (hfql/symbolic-edge cellT))
          (= (hfql/identify (hfql/value-at-point cellT))
            (hfql/identify (hfql/value-at-point selected-cellT))))))))

(e/defn MatrixCell [index cellT]
  (e/client
    (dom/td (dom/props {:style {:--cell-index index}
                        :role :radio
                        :tabindex "0"
                        :aria-selected (e/server (cell-selected? cellT selection/*local-selection))})
            (e/server (def _cellT cellT) nil)
            (let [GetCellT (e/server (utils/Delay cellT))
                  event (dom/On* "click" identity nil) ; TODO hoist
                  [token err] (e/Token event)]
              (e/Reconcile
                (when token
                  (dom/props {:aria-busy true})
                  (dispatch-event! :up dom/node (munge-str `Select!) GetCellT)
                  (dom/On* (munge-str `SelectAck!) (fn [_] (token) nil) nil {:capture true}))))
            (e/server (rendering/Render* cellT))))) ; :detail is the name of the spec-defined data slot. I didn't choose the name.

(defn- row-selected? [rowT local-selection]
  (let [[selected-column selected-rowT] local-selection]
    (when (and rowT selected-rowT) ; FIXME should be able to compare by direct equality without identifying
      (or (= rowT selected-rowT) ; Ideal path, always false for now. Should suffice to the check. This is what we want.
        (and (nil? selected-column) ; negative assertion – closed condition – restricting a set – baaaad 🐑
          (= (hfql/identify (hfql/value-at-point rowT))
            (hfql/identify (hfql/value-at-point selected-rowT))))))))

(e/defn RowHandle [group-id rowT]
  (dom/th (dom/props {:scope :row})
          (let [GetRowT (e/server (utils/Delay rowT))
                [token _] (forms/Checkbox! ::row (e/server (row-selected? rowT selection/*local-selection)) :type :radio :name (str group-id))]
            (e/Reconcile
              (when token
                (dispatch-event! :up dom/node (munge-str `Select!) GetRowT)
                (dom/On* (munge-str `SelectAck!) (fn [_] (token) nil) nil {:capture true}))))
          (e/amb)))

(e/defn MatrixRow [row-group-id columnsT index rowT offsetH limitH]
  (e/client
    (dom/tr
      (dom/props {:style {:--row-index index}})
      (e/client (set! (.-data-rowT ^js dom/node) (e/server (utils/Delay rowT))))
      (RowHandle row-group-id rowT)
      (let [cellsT (e/server ((maybe unwrap) rowT))
            selected-colsT (e/server (set (mapv hfql/id columnsT)))
            cellsT (e/server (filterv #(selected-colsT (hfql/id %)) cellsT))
            ]
        (e/server ;; trade off:
          ;; e/for on client = ultra smooth row positioning but glitchy row content
          ;; e/for on server = slower row rotation (still acceptable) but no row content glitch.
          (e/for [index (IndexRing limitH offsetH)]
            (MatrixCell index (e/server (nth cellsT index nil)))))))))

(e/defn MatrixBody [columnsT rowsT offsetV limitV offsetH limitH]
  (e/client
    (dom/tbody
      (let [fast-nth (e/server (utils/caching-nth rowsT))
            row-group-id (random-uuid)]
        (e/server ;; trade off:
          ;; e/for on client = ultra smooth row positioning but glitchy row content
          ;; e/for on server = slower row rotation (still acceptable) but no row content glitch.
          (e/for [index (IndexRing limitV offsetV)]
            (MatrixRow row-group-id columnsT index (e/server (fast-nth index nil)) offsetH limitH)))))))

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

#_(e/defn HandleSelection [commands]
  (->> commands
    (utils/InterceptCommand `Select! (e/fn [[_ GetElementT]]
                                       (let [selectionT (e/client (find-clicked-cell-or-row event))]
                                         (e/client (prn "selectionT" selected-index selectionT))
                                         (e/server (e/Reconcile
                                                     (if selectionT
                                                       [`selection/SetSelection! selectionT]
                                                       [`Noop]))))))))

(defn- bounds-by-axis [transpose? [offsetV limitV offsetH limitH :as initial]]
  (if transpose?
    [offsetH limitH offsetV limitV]
    initial))

#?(:cljs
   (defn- transpose! [table-node !transpose]
     (set! (.-scrollTop table-node) 0)
     (set! (.-scrollLeft table-node) 0)
     (swap! !transpose not)))

(e/defn Matrix [columnsT rowsT & {:keys [transpose? row-height column-width min-row-count] :or {row-height 24 #_"1lh" column-width 200 #_ "40ch"}}]
  (e/client
    (dom/table
      (dom/props {:role :radiogroup})
      (let [!transpose (atom true)
            transpose (e/watch !transpose)]
        (reset! !transpose (boolean transpose?))
        (let [rowsT-count (identity (e/server (count rowsT)))]
          (dom/props {:class ["matrix" ({true "transposed"} transpose)]
                      :style {:--row-height (str row-height "px")
                              :--column-width (str column-width "px")
                              :--row-count rowsT-count
                              :--column-count (e/server (count columnsT))}})
          (forms/Interpreter {`Transpose! (e/fn [] ((constantly [::forms/ok]) (transpose! dom/node !transpose)))
                              `Noop (e/fn [] [::forms/ok])}
            (e/amb
              (MatrixHeader columnsT)
              (let [[offsetV limitV offsetH limitH] (bounds-by-axis transpose (Scroll-window 24 rowsT-count dom/node {:column-width column-width}))]
                (MatrixBody columnsT rowsT offsetV limitV offsetH (min (e/server (count columnsT)) limitH)))
              (e/amb
                (Command (fn [^js event] [`selection/SetSelection! (.-detail event)]) (dom/On* (munge-str `Select!) identity nil)
                  (fn [event]
                    (dispatch-event! :down (.-target event) (munge-str `SelectAck!) nil)))))))))))


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
        (mappend from-spec suggested)))))

(def COLLECTION-LIMIT 10000)

(e/defn RenderMatrix [stepT & [transpose?]]
  (dom/fieldset
    (e/client (dom/link (dom/props {:rel :stylesheet, :href "/hyperfiddle/matrix.css"})))
    (e/client (dom/link (dom/props {:rel :stylesheet, :href "/hyperfiddle/matrix_vertical.css"})))
    (e/client (dom/link (dom/props {:rel :stylesheet, :href "/hyperfiddle/matrix_horizontal.css"})))
    (dom/props {:class "" #_"entity-children hyperfiddle-navigator6__block"})
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
                (Matrix columnsT rowsT :transpose? transpose?)))))))))

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

(e/defn MatrixHeader [columnsT]
  (dom/thead
    (dom/tr
      (dom/th (dom/props {:class "matrix__diagonal" :scope :col :style {:width "100%"}})
              #_(dom/On* "click" ...))
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
                        (e/Reconcile (e/When t [t [`sort/Sort! (e/server (utils/Delay columnT)) direction]])))))))))))

(e/defn MatrixCell [cellT]
  (e/server (rendering/Render* cellT)))

(e/defn RowHandle [selected?]
  (dom/th (dom/props {:scope :row})
    (forms/Checkbox* selected? :type :radio)))

(e/defn MatrixRow [columnsT index rowT]
  (e/client
    (dom/tr
      (dom/props {:style {:--row-index index}})
      (e/client (set! (.-data-rowT ^js dom/node) (e/server (utils/Delay rowT))))
      (RowHandle false)
      (let [cellsT (e/server ((maybe unwrap) rowT))
            selected-colsT (e/server (set (mapv hfql/id columnsT)))
            cellsT (e/server (filterv #(selected-colsT (hfql/id %)) cellsT))
            cellsT-index (e/server (into {} (map (juxt hfql/id identity)) cellsT))]
        (e/for [cell-id (e/diff-by identity (e/server (mapv hfql/id cellsT)))]
          (MatrixCell (e/server (get cellsT-index cell-id))))))))

#?(:cljs (defn resize-observer [node]
           (->>
             (m/observe (fn [!] (! [(.-clientHeight node)
                                    (.-clientWidth node)])
                          (let [obs (new js/ResizeObserver
                                      (fn [entries]
                                        (let [content-box-size (-> entries (aget 0) .-contentBoxSize (aget 0))]
                                          (! [(.-blockSize content-box-size)
                                              (.-inlineSize content-box-size)]))))]
                            (.observe obs node) #(.unobserve obs node))))
             (mx/throttle 100) ; Optimization – (un)mounting rows is expensive. 100ms is usually considered "minimum human-perceptible latency".
             (m/relieve {}))))

#?(:cljs (defn scroll-state [scrollable]
           (->> (m/observe
                  (fn [!]
                    (let [sample (fn [] (! [(.. scrollable -scrollTop) ; optimization - detect changes (pointless)
                                            (.. scrollable -scrollHeight) ; snapshot height to detect layout shifts in flipped mode
                                            (.. scrollable -clientHeight)]))] ; measured viewport height (scrollbar length)
                      (sample) #_(! [0 0 0]) ; don't emit 0 when flow is rebuilt
                      (.addEventListener scrollable "scroll" sample #js {"passive" true})
                      #(.removeEventListener scrollable "scroll" sample))))
             (mx/throttle 16) ; RAF interval
             (m/relieve {}))))

(e/defn MatrixBody [columnsT rowsT offset limit]
  (e/client
    (dom/tbody
      (let [fast-nth (e/server (utils/caching-nth rowsT))]
        (e/server ;; trade off:
          ;; e/for on client = ultra smooth row positioning but glitchy row content
          ;; e/for on server = slower row rotation (still acceptable) but no row content glitch.
          (e/for [index (IndexRing limit offset)]
            (MatrixRow columnsT index (e/server (fast-nth index nil)))))))))

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

(e/defn HandleSelection [commands]
  (->> commands
    (forms/Parse (e/fn [{selected-index ::selection, event ::forms/event}] [`Select! selected-index event]))
    (utils/InterceptCommand `Select! (e/fn [[_ selected-index event]]
                                       (let [selectionT (e/client (find-clicked-cell-or-row event))]
                                         (e/client (prn "selectionT" selected-index selectionT))
                                         (e/server (e/Reconcile
                                                     (if selectionT
                                                       [`selection/SetSelection! selectionT]
                                                       [`Noop]))))))
    (forms/Interpreter {`Noop (e/fn [] [::forms/ok])})))

(e/defn Matrix [columnsT rowsT & {:keys [transpose? row-height column-width min-row-count] :or {row-height 24 #_"1lh" column-width 400 #_ "40ch"}}]
  (e/client
    (dom/style (dom/text css)) ; TODO factorize
    (dom/table
      (let [rowsT-count (identity (e/server (count rowsT)))
            [offset limit] (Scroll-window 24 rowsT-count dom/node {})
            [scrollTop #_scrollHeight #_clientHeight] (e/input (scroll-state dom/node))]
        (dom/props {:class "matrix"
                    :style {:--row-height (str row-height "px")
                            :--column-width (str column-width "px")
                            :--row-count rowsT-count
                            :--column-count (e/server (count columnsT))}})
        (e/amb
          (MatrixHeader columnsT)
          (HandleSelection
            (MatrixBody columnsT rowsT offset limit)))))))


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

(def COLLECTION-LIMIT 10000)

(e/defn RenderMatrix [stepT & [transpose?]]
  (e/client (dom/link (dom/props {:rel :stylesheet, :href "/hyperfiddle/matrix.css"})))
  (dom/fieldset
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
                (Matrix columnsT rowsT #_#_selection/*local-selection transpose?)))))))))

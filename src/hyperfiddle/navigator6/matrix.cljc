(ns hyperfiddle.navigator6.matrix
  (:require
   [clojure.math :as math]
   [contrib.data :refer [clamp]]
   [contrib.missionary-contrib :as mx]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window IndexRing #?(:cljs resize-observer)]]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.hfql2.analyzer :as ana]
   [hyperfiddle.ct.cofree :refer [unwrap]]
   [hyperfiddle.ct.comonad :refer [extract]]

   [hyperfiddle.ct.monoid :refer [mappend]]
   [hyperfiddle.ct.either :refer [either]]
   [hyperfiddle.hfql2 :as hfql]
   [hyperfiddle.navigator6.rendering :as rendering]
   [hyperfiddle.navigator6.search :as search]
   [hyperfiddle.navigator6.selection :as selection]
   [hyperfiddle.navigator6.selection2 :as selection2]
   [hyperfiddle.navigator6.sorting :as sort]
   [hyperfiddle.navigator6.utils :as utils :refer [in glitch-guard]]
   #_[hyperfiddle.hfql2.utils :refer [imap?]]
   [dustingetz.loader :refer [Offload]]
   [missionary.core :as m]))

(defn- maybe [f] (fn [x] (when x (f x))))

(e/defn Command [value-fn event after-ack]
  (let [[t err] (e/Token event)]
    (e/Reconcile
      (e/When t
        [(forms/after-ack t (partial after-ack event)) (value-fn event)]))))


(defn- named? [x]
  (or (string? x)
    #?(:clj (instance? clojure.lang.Named x)
       :cljs (implements? INamed x))))

(defn- name-shortener [names]
  (let [key-fn #(if (named? %) (name %) %)
        groups (group-by key-fn names)
        conflicts? (into #{} (map key (filter (fn [[_k v]] (>= (count v) 2)) groups)))]
    (fn [x]
      (if (conflicts? (key-fn x))
        x
        (key-fn x)))))

(defn column-sort-order [columnT sort-spec]
  (when (and columnT sort-spec) ; glitch guard
    (or (some (fn [[sort-spec-columnT sort-spec-order]]
                (when (= (hfql/symbolic-edge columnT) (hfql/symbolic-edge sort-spec-columnT))
                  sort-spec-order))
          sort-spec)
      '.)))

(e/defn MatrixHeader [columnsT offsetH limitH]
  (dom/thead
    (dom/tr
      (e/amb
        (dom/th (dom/props {:class "transpose-affordance matrix__diagonal" :aria-label "transpose" :title "Transpose" :style {:width "100%"}})
                (Command (constantly [`Transpose!]) (dom/On* "click" identity nil) (constantly nil)))
        ;; shorten needs full column set to detect naming conflicts
        (let [shorten (e/server (name-shortener (into [] (comp (take COLLECTION-LIMIT) (map hfql/symbolic-edge)) columnsT)))]
          (e/for [col-index (IndexRing limitH offsetH)]
            ;; Nil-check on server — columnT is an HFQL tree node (unserializable),
            ;; so it must never cross to client. Only `form` (string) crosses the wire.
            (e/server
              (when-let [columnT (nth columnsT col-index nil)]
                (let [form (str (shorten (hfql/symbolic-edge columnT)))]
                  (e/client
                    (dom/th (dom/props {:scope :col, :style {:--cell-index col-index}})
                      (dom/span (dom/props {:class "column-name"}) (dom/text form)
                        (e/server (rendering/RenderTooltip ::hfql/ColumnHeaderTooltip nil columnT nil)))
                      (e/client
                        (dom/span
                          (dom/props {:class "sort-handle"
                                      :title "Sort"
                                      :data-sort-order (str (e/server (column-sort-order columnT sort/*sort-spec*)))
                                      :disabled sort/*disabled?*})
                          (let [!direction (atom (cycle ['. '> '<]))
                                direction (dom/On "click" (fn [_event] (first (swap! !direction rest))) nil)
                                [t err] (e/Token direction)]
                            (e/Reconcile (e/When t [t [`sort/Sort! (e/server (utils/Delay columnT)) direction]]))))))))))))))))

#?(:cljs
   (defn- dispatch-event! [direction ^js node type detail]
     (let [[capture bubbles] (case direction
                               :both [true true]
                               :up   [false true]
                               :down [true false])]
       (.dispatchEvent node (new js/CustomEvent type #js{:capture capture, :bubbles bubbles, :detail detail})))))

(def INTERRACTIVE-DOM-ELEMENT-SELECTOR
  "Elements matching this selector will be considered interactive. Interactions
  with interactive elements won't be interpreted by the Matrix. E.g. clicking on
  a link or button in a cell won't select the cell. Users may mark any element
  as interactive with the `data-role=widget` attribute. Users must not use
  `role=widget` directly. It can mess with browsers' internal logic and damages
  accessibility. See https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/Reference/Roles/widget_role."
  "[data-role=\"widget\"],button,a[href],input,select,textarea,[role=\"button\"],[contenteditable=\"\"],[contenteditable=\"true\"]")

#?(:cljs
   (defn- cell-ignore-click-on-interactive-content [^js event]
     (when-let [target (.-target event)]
       (if (or (.matches target INTERRACTIVE-DOM-ELEMENT-SELECTOR)
             (when-let [hit (.closest target INTERRACTIVE-DOM-ELEMENT-SELECTOR)]
               (.. event -currentTarget (contains hit))))
         nil
         event))))


(defn- row-positional-index-from-hfql-for-this-cell
  "Returns the positional index of the cell's parent row from HFQL metadata.
  We use the HFQL metadata index instead of the UI's row-index because after drilling
  into a selected row, the UI index reflects the position in the current view (e.g., 0
  for the first visible row), but the HFQL metadata contains the actual position in the
  parent collection (e.g., 1 if we drilled into the second row). The metadata index is
  the correct one for generating selection coordinates."
  [cellT]
  (::hfql/positional-index (meta (hfql/parent cellT))))

(e/defn MatrixCell [cell-index cellT]
  (e/client
    (dom/td (dom/props {:style {:--cell-index cell-index}
                        :role :radio
                        :tabindex "0"
                        :data-tooltip (e/server ({false (str "Can't select this cell. Column object is not Identifiable. " (type (hfql/symbolic-edge cellT)) " : " (hfql/symbolic-edge cellT))} (selection2/cell-selectable? cellT)))
                        :aria-selected (e/server (selection2/cell-selected? selection/*local-selection cellT selection/*local-selection-symbolic))})
            (let [GetCellT (e/server (utils/Delay cellT))
                  event (dom/On* "click" cell-ignore-click-on-interactive-content nil) ; TODO hoist
                  [token err] (e/Token event)]
              (e/Reconcile
                (when token
                  (dom/props {:aria-busy true})
                  (dispatch-event! :up dom/node (munge-str `Select!) [(e/server (row-positional-index-from-hfql-for-this-cell cellT)) GetCellT])
                  (dom/On* (munge-str `SelectAck!) (fn [_] (token) nil) nil {:capture true}))))
            (e/server (rendering/Render* cellT))))) ; :detail is the name of the spec-defined data slot. I didn't choose the name.

(e/defn RowHandle [group-id row-index rowT]
  (dom/th (dom/props {:scope :row})
          (let [value-at-point (e/server ((glitch-guard some? either identity identity) ((glitch-guard hfql/value-at-point) rowT)))
                GetRowT (e/server (utils/Delay rowT))
                [token _] (forms/Checkbox! ::row (e/server (selection2/row-selected? selection/*local-selection rowT selection/*local-selection-symbolic))
                            :type :radio
                            :name (str group-id))]
            (e/Reconcile
              (when token
                (dispatch-event! :up dom/node (munge-str `Select!) [row-index GetRowT])
                (dom/On* (munge-str `SelectAck!) (fn [_] (token) nil) nil {:capture true}))))
          (e/amb)))

(e/defn MatrixRow [row-group-id columnsT row-index rowT offsetH limitH]
  (e/client
    (dom/tr
      (dom/props {:style {:--row-index row-index}
                  ;; TODO improve – we have an extra nil row at the bottom when scrolling to the far bottom of a table.
                  ;; Because we render limit + 1 row for smooth scrolling experience (top and bottom rows are always clipped on scroll)
                  ;; This extra row is empty at the end of the collection, and we don't want it to show up at all. So we hide it with CSS.
                  :data-empty (str (e/server (nil? rowT)))})
      (e/client (set! (.-data-rowT ^js dom/node) (e/server (utils/Delay rowT))))
      (let [rowT (e/server (e/Offload #(hfql/augment-with-suggestions! rowT)))]
        (RowHandle row-group-id row-index rowT)
        (let [cellsT (e/server (e/Offload (fn [] (let [cellsT ((maybe unwrap) rowT)
                                                      selected-colsT (set (mapv hfql/id columnsT))]
                                                  (filterv #(selected-colsT (hfql/id %)) cellsT)))))
              ]
          (e/client ;; trade off:
            ;; e/for on client = ultra smooth row positioning but glitchy row content
            ;; e/for on server = slower row rotation (still acceptable) but no row content glitch.
            (e/for [col-index (IndexRing limitH offsetH)]
              (MatrixCell col-index (e/server (hfql/find-cell-by-column* (nth columnsT col-index nil) cellsT))))))))))

(e/defn MatrixBody [columnsT rowsT offsetV limitV offsetH limitH]
  (e/client
    (dom/tbody
      (let [row-group-id (random-uuid)]
        ;; e/for on client = ultra smooth row positioning but glitchy row content
        (e/for [index (IndexRing limitV offsetV)]
          (MatrixRow row-group-id columnsT
            index
            (e/server (e/Offload #(nth rowsT index nil))) offsetH limitH))))))

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


(defn- bounds-by-axis [transpose? [offsetV limitV offsetH limitH :as initial]]
  (if transpose?
    [offsetH limitH offsetV limitV]
    initial))

#?(:cljs
   (defn- transpose! [table-node !transpose]
     (set! (.-scrollTop table-node) 0)
     (set! (.-scrollLeft table-node) 0)
     (swap! !transpose not)))

(e/defn Matrix [columnTs rowTs-count rowTs & {:keys [associative? row-height column-width min-row-count] :or {row-height 24 #_"1lh" column-width 200 #_ "40ch"}}]
  (e/client
    (dom/table
      (dom/props {:role :radiogroup})
      ;; Associative views default to transposed — properties render as visual rows,
      ;; reusing the column IndexRing for lazy property pagination.
      (let [!transpose (atom (boolean associative?))
            transpose (e/watch !transpose)
            columnTs-count (e/server (count columnTs))
            ;; Stable header column width — computed from ALL shortened property names
            ;; (capped at COLLECTION-LIMIT to avoid forcing unbounded lazy seqs),
            ;; not just the IndexRing-visible subset, to prevent width jumps during scroll.
            max-label-ch (e/server (let [edges (into [] (comp (take COLLECTION-LIMIT)
                                                              (map hfql/symbolic-edge))
                                                        columnTs)
                                         shorten (name-shortener edges)]
                                     (transduce (map (comp count str shorten)) max 0 edges)))]
        (dom/props {:class ["matrix" ({true "transposed"} transpose) ({true "associative"} associative?)]
                    :style {:--row-height    (str row-height "px")
                            :--column-width  (str column-width "px")
                            :--row-count     rowTs-count
                            :--column-count  columnTs-count
                            :--max-label-ch  max-label-ch}})
        (forms/Interpreter {`Transpose! (e/fn [] ((constantly [::forms/ok]) (transpose! dom/node !transpose)))
                            `Noop (e/fn [] [::forms/ok])}
          ;; Hoist scroll computation so both MatrixHeader and MatrixBody share
          ;; the same pagination window. Cost: header waits ~1 frame for first
          ;; resize observation — practically imperceptible.
          (let [[_clientHeight clientWidth] (e/input (resize-observer dom/node))
                itemTs-count (#(if transpose columnTs-count rowTs-count))
                scroll-window (Scroll-window 24 itemTs-count dom/node {:column-width (#(if transpose column-width (/ clientWidth (max columnTs-count 1))))})
                [offsetV limitV offsetH limitH] (bounds-by-axis transpose scroll-window)
                col-limit (min columnTs-count limitH)]
            (e/amb
              ;; MatrixHeader renders property names as <th scope=col>. CSS transposition
              ;; turns them into visual row headers. Sort handles hidden via CSS for associative.
              (MatrixHeader columnTs offsetH col-limit)
              (MatrixBody columnTs rowTs
                offsetV (min rowTs-count limitV)
                offsetH col-limit)
              (e/amb
                (Command (fn [^js event] [`selection/SetSelection! (.-detail event)]) (dom/On* (munge-str `Select!) identity nil)
                  (fn [event]
                    (dispatch-event! :down (.-target event) (munge-str `SelectAck!) nil)))))))))))

(e/defn ColumnPicker [stepT]
  (e/server
    (when-let [pull-step (e/Snapshot (e/Filter some? ((glitch-guard selection/select-pull-step) stepT)))]
      (let [stepT+ (hfql/augment-with-suggestions! pull-step)
            childrenT (remove (every-pred hfql/recursion-point? hfql/recursive-child?) ((glitch-guard unwrap) stepT+)) ; TODO what is this line doing? Why?
            childrenT-order (zipmap childrenT (range))
            static-columns (remove (comp ::hfql/suggested meta) childrenT)
            suggested-columns (e/Snapshot (filter (comp ::hfql/suggested meta) childrenT))
            pre-checked (#(or ((glitch-guard hfql/pull-has-star?) stepT+) (empty? static-columns)))
            shorten (name-shortener (map hfql/symbolic-edge childrenT))
            from-spec (e/as-vec (e/for [columnT (e/diff-by hfql/symbolic-edge static-columns)]
                                  (e/When
                                      (dom/span
                                        (forms/Checkbox* true :label (e/server (str (shorten (hfql/symbolic-edge columnT))))
                                          :title (e/server (str (hfql/describe-formatted columnT)))))
                                    columnT)))
            suggested (e/as-vec (e/for [columnT (e/diff-by hfql/symbolic-edge suggested-columns)]
                                 (e/When
                                     (dom/span
                                       (forms/Checkbox* pre-checked :label (e/server (str (shorten (hfql/symbolic-edge columnT))))
                                         :title (e/server (str (hfql/describe-formatted columnT)))))
                                   columnT)))]
        (->> (mappend from-spec suggested)
          (sort-by childrenT-order <) ; FIXME order is still not respected WRT star. Suggestions should be injected where star is, if present.
          )))))

(def COLLECTION-LIMIT 10000)



(e/defn RenderMatrix [stepT & {:keys [associative?]}]
  (dom/fieldset
    (dom/props {:class [({true "error"} (e/server (some? (::hfql/error ((glitch-guard meta) stepT)))))]})
    (let [_ (e/server (Offload #(do ((glitch-guard hfql/nav) stepT) nil) {:label 'run-query}))
          [legend top-left] (dom/legend
                              [dom/node (dom/span (dom/props {:style {:float :inline-start, :display :flex, :flex-wrap :wrap}})
                                                  (rendering/Step-Title stepT)
                                                  dom/node)])
          ;; For collections: ColumnPicker lets users toggle columns.
          ;; For associative: bypass picker — all pull children are columns directly.
          picker-columnsT (e/server (when-not associative? (in legend (ColumnPicker stepT))))
          stepT (e/server ((glitch-guard selection2/find-collection-point) stepT))
          limit (e/server (::hfql/limit (extract (meta stepT)) COLLECTION-LIMIT))
          ;; For associative views: columns = pull children, row = the pull itself (single row).
          ;; The column IndexRing handles lazy pagination over properties.
          ;; For collections: columns from ColumnPicker, rows from unwrapping the collection.
          children (e/server ((glitch-guard unwrap) stepT))
          columnsT (e/server (or picker-columnsT children))
          rowsT (e/server (if associative? [stepT] children))
          rowsT-count (e/server (if associative? 1 (e/Offload #(bounded-count limit rowsT))))
          disabled-search-sort-because-a-row-is-selected (e/server (some? selection/*local-selection))
          ;; For associative views, swap search axes: filter properties (columns) by name/value
          ;; using the existing edge-name search path (columnTs = nil triggers it).
          search-items (e/server (if associative? columnsT rowsT))
          search-items-count (e/server (if associative? (count columnsT) rowsT-count))
          search-columnTs (e/server (when-not associative? columnsT))]
      (search/Filtered (e/server (search/eager-search-BIG-O-N-SQUARE))
        search-items-count
        (e/server search-items)
        (e/server search-columnTs)
        (e/fn [filtered-items]
          (let [;; For associative: filtered-items = filtered columns. For collection: filtered rows.
                effective-columnsT (e/server (if associative? filtered-items columnsT))
                effective-rowsT (e/server (if associative? rowsT filtered-items))
                entries-count (e/server (#(if associative?
                                            (count effective-columnsT)
                                            (bounded-count rowsT-count effective-rowsT))))]
            (in top-left
              (dom/span (dom/props {:style {:order -1}
                                    :class "count"})
                        (dom/text (#(if associative?
                                      (rendering/format-map-entries-count entries-count limit)
                                      (rendering/format-collection-count entries-count limit))))))
            (e/amb
              (in top-left (dom/span (dom/props {:style {:order -2}})
                                     (search/SearchInput! disabled-search-sort-because-a-row-is-selected)))
              (sort/Sorted disabled-search-sort-because-a-row-is-selected (e/server effective-rowsT)
                (e/fn [rowsT]
                  (binding [rendering/*rowT (e/server ((glitch-guard selection2/find-selectable-point) stepT))]
                    ;; For associative views, actual row count = 1 (single data row).
                    ;; entries-count is the property count — used for display only.
                    (Matrix effective-columnsT (if associative? 1 entries-count) rowsT :associative? associative?)))))))))))

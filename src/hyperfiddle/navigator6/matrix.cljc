(ns hyperfiddle.navigator6.matrix
  (:require
   [clojure.math :as math]
   [dustingetz.data :refer [clamp]]
   [dustingetz.missionary-contrib :as mx]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window IndexRing #?(:cljs resize-observer)]]
   [hyperfiddle.electric3 :as e]
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
   [dustingetz.loader :refer [Offload]]
   [missionary.core :as m]))

;; ── Data pipeline (RenderMatrix, line ~301) ───────────────────────────
;; Each stage transforms the data before rendering. Order matters:
;;
;;   1. ColumnPicker (line ~274)
;;      augment-with-suggestions! once on the pull step to discover all
;;      available columns for the picker UI (collection views only).
;;
;;   2. Search — eager-search-BIG-O-N-SQUARE (search.cljc)
;;      Per-row: augment-with-suggestions! to discover searchable cells,
;;      then string-match against search query. Filters rows (or columns
;;      for associative views).
;;
;;   3. Sort — sort-rows-by-columns (sorting.cljc)
;;      Per-row: augment-with-suggestions! so sorted column is present.
;;      Fallback: returns unsorted on error (logged, not silent).
;;
;;   4. Render — Matrix → MatrixRow (line ~171)
;;      Per-row: augment-with-suggestions! again — PERF redundant, same
;;      schema rediscovered per row. Associative views avoid this (line
;;      ~317 does it once at RenderMatrix level). Fixing requires
;;      heterogeneous collection support.

(defn- maybe [f] (fn [x] (when x (f x))))

(e/defn Command [value-fn event after-ack]
  (let [[t err] (e/Token event)]
    (e/Reconcile
      (e/When t
        [(forms/after-ack t (partial after-ack event)) (value-fn event)]))))


(def COLLECTION-LIMIT 10000)

(defn- named? [x]
  (or (string? x)
    #?(:clj (instance? clojure.lang.Named x)
       :cljs (implements? INamed x))))

(defn- display-edge
  "Strip the (quote ...) wrapper from runtime-discovered map keys (e.g. aggregate
   columns) so the UI shows (count ?x) instead of (quote (count ?x)).
   The quote is added by disambiguate-from-hfql-syntax to prevent HFQL from
   calling list keys as functions; this is the rendering-layer inverse."
  [edge]
  (if (and (seq? edge) (= 'quote (first edge)))
    (second edge)
    edge))

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

(e/defn MatrixHeader [columnsT offsetH limitH associative?]
  (dom/thead
    (dom/tr
      (e/amb
        ;; Only show transpose toggle for collection views — single-object forms
        ;; are always transposed, the toggle adds clutter with no benefit.
        (when (not associative?)
          (dom/th (dom/props {:class "transpose-affordance matrix__diagonal" :aria-label "transpose" :title "Transpose" :style {:width "100%"}})
                  (Command (constantly [`Transpose!]) (dom/On* "click" identity nil) (constantly nil))))
        ;; shorten needs full column set to detect naming conflicts
        (let [shorten (e/server (name-shortener (into [] (comp (take COLLECTION-LIMIT) (map (comp display-edge hfql/symbolic-edge))) columnsT)))]
          (e/for [col-index (IndexRing limitH offsetH)]
            ;; Nil-check on server — columnT is an HFQL tree node (unserializable),
            ;; so it must never cross to client. Only `form` (string) crosses the wire.
            (let [columnT (e/server (nth columnsT col-index nil))]
              (when (e/server (some? columnT))
                (let [form (e/server (str (shorten (display-edge (hfql/symbolic-edge columnT)))))]
                  (e/client
                    (dom/th (dom/props {:scope :col, :style {:--cell-index col-index, :cursor "pointer"}})
                      (dom/span (dom/props {:class "column-name"}) (dom/text form)
                        (e/server (rendering/RenderTooltip ::hfql/ColumnHeaderTooltip nil columnT nil)))
                      (dom/span ; visual-only sort indicator (arrows)
                        (dom/props {:class "sort-handle"
                                    :data-sort-order (str (e/server (column-sort-order columnT sort/*sort-spec*)))
                                    :disabled sort/*disabled?*}))
                      ;; Sort click handler on <th> — whole header is the click target
                      (let [!direction (atom (cycle ['. '< '>])) ; ASC first — matches spreadsheet convention
                            direction (dom/On "click" (fn [_event] (first (swap! !direction rest))) nil)
                            [t err] (e/Token direction)]
                        (e/Reconcile (e/When t [t [`sort/Sort! (e/server (utils/Delay columnT)) direction]]))))))))))))))

#?(:cljs
   (defn- dispatch-event! [direction ^js node type detail]
     (let [[capture bubbles] (case direction
                               :both [true true]
                               :up   [false true]
                               :down [true false])]
       (.dispatchEvent node (new js/CustomEvent type #js{:capture capture, :bubbles bubbles, :detail detail})))))

(def INTERACTIVE-DOM-ELEMENT-SELECTOR
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
       (if (or (.matches target INTERACTIVE-DOM-ELEMENT-SELECTOR)
             (when-let [hit (.closest target INTERACTIVE-DOM-ELEMENT-SELECTOR)]
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
  (when cellT
    (::hfql/positional-index (meta (hfql/parent cellT)))))

(e/defn MatrixCell [cell-index cellT]
  (e/client
    (let [has-cell? (e/server (some? cellT))]
      (dom/td (dom/props {:style {:--cell-index cell-index}
                          ;; Buffer cell beyond column count at far-bottom scroll —
                          ;; mirrors the data-empty pattern for nil rows (line 164).
                          :data-empty (str (not has-cell?))
                          :role (when has-cell? :radio)
                          :tabindex (when has-cell? "0")
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
              (e/server (rendering/Render* cellT)))))) ; :detail is the name of the spec-defined data slot. I didn't choose the name.

(e/defn RowHandle [group-id rowT]
  (dom/th (dom/props {:scope :row})
          (let [value-at-point (e/server ((glitch-guard some? either identity identity) ((glitch-guard hfql/value-at-point) rowT)))
                GetRowT (e/server (utils/Delay rowT))
                [token _] (forms/Checkbox! ::row (e/server (selection2/row-selected? selection/*local-selection rowT selection/*local-selection-symbolic))
                            :type :radio
                            :name (str group-id))]
            (e/Reconcile
              (when token
                ;; Use HFQL positional index (position in original collection) instead of
                ;; UI row-index (position in filtered/sorted view). Consistent with MatrixCell
                ;; which uses row-positional-index-from-hfql-for-this-cell.
                (dispatch-event! :up dom/node (munge-str `Select!)
                  [(e/server (::hfql/positional-index (meta rowT))) GetRowT])
                (dom/On* (munge-str `SelectAck!) (fn [_] (token) nil) nil {:capture true}))))
          (e/amb)))

(e/defn MatrixRow [row-group-id columnsT row-index rowT offsetH limitH & {:keys [associative?]}]
  (e/client
    (dom/tr
      (dom/props {:style {:--row-index row-index}
                  ;; TODO improve – we have an extra nil row at the bottom when scrolling to the far bottom of a table.
                  ;; Because we render limit + 1 row for smooth scrolling experience (top and bottom rows are always clipped on scroll)
                  ;; This extra row is empty at the end of the collection, and we don't want it to show up at all. So we hide it with CSS.
                  :data-empty (str (e/server (nil? rowT)))})
      (e/client (set! (.-data-rowT ^js dom/node) (e/server (utils/Delay rowT))))
      ;; PERF: Per-row augmentation — all visible rows re-discover the same schema via
      ;; augment-with-suggestions!. For a 10-row viewport with 100 columns, that's 10×
      ;; redundant IO + eager union. Associative views avoid this (line ~318 does it once).
      ;; Lazy alternative: lift suggestion computation to collection level, share across rows.
      (let [rowT (e/server (e/Offload #(hfql/augment-with-suggestions! rowT)))]
        (RowHandle row-group-id rowT)
        ;; Associative views show all discovered properties — no column filtering.
        ;; Collection views filter to ColumnPicker selection.
        (let [cellsT (e/server (e/Offload (fn [] (let [cellsT ((maybe unwrap) rowT)]
                                                  (if associative?
                                                    cellsT
                                                    (let [selected-colsT (set (mapv hfql/id columnsT))]
                                                      (filterv #(selected-colsT (hfql/id %)) cellsT)))))))
              ]
          (e/client ;; trade off:
            ;; e/for on client = ultra smooth row positioning but glitchy row content
            ;; e/for on server = slower row rotation (still acceptable) but no row content glitch.
            (e/for [col-index (IndexRing limitH offsetH)]
              (MatrixCell col-index (e/server (hfql/find-cell-by-column* (nth columnsT col-index nil) cellsT))))))))))

(e/defn MatrixBody [columnsT rowsT offsetV limitV offsetH limitH & {:keys [associative?]}]
  (e/client
    (dom/tbody
      (let [row-group-id (random-uuid)]
        ;; e/for on client = ultra smooth row positioning but glitchy row content
        (e/for [index (IndexRing limitV offsetV)]
          (MatrixRow row-group-id columnsT
            index
            (e/server (e/Offload #(nth rowsT index nil))) offsetH limitH
            :associative? associative?))))))

(defn- bounds-by-axis [transpose? [offsetV limitV offsetH limitH :as initial]]
  (if transpose?
    [offsetH limitH offsetV limitV]
    initial))

#?(:cljs
   (defn- transpose! [table-node !transpose]
     (set! (.-scrollTop table-node) 0)
     (set! (.-scrollLeft table-node) 0)
     (swap! !transpose not)))

(e/defn Matrix [columnTs rowTs-count rowTs & {:keys [associative? row-height column-width min-column-width min-row-count] :or {row-height 24 #_"1lh" column-width 200 #_ "40ch" min-column-width 150}}]
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
                                                              (map (comp display-edge hfql/symbolic-edge)))
                                                        columnTs)
                                         shorten (name-shortener edges)]
                                     (transduce (map (comp count str shorten)) max 0 edges)))]
        (dom/props {:class ["matrix" ({true "transposed"} transpose) ({true "associative"} associative?)]
                    :style {:--row-height        (str row-height "px")
                            :--column-width      (str column-width "px")
                            :--min-column-width  (str min-column-width "px")
                            :--row-count         rowTs-count
                            :--column-count      columnTs-count
                            :--max-label-ch      max-label-ch
                            ;; Smart initial sizing — fit data + header + 1 empty affordance row, clamped to min.
                            ;; Associative views have no header row (diagonal removed), so 1 fewer line.
                            :--min-row-count     (if associative? 2 3)
                            :--initial-row-count (let [visual-rows (if transpose columnTs-count rowTs-count)
                                                       header-rows (if associative? 0 1)]
                                                   (clamp (+ visual-rows header-rows 1) (if associative? 2 3) 15))}})
        (forms/Interpreter {`Transpose! (e/fn [] ((constantly [::forms/ok]) (transpose! dom/node !transpose)))
                            `Noop (e/fn [] [::forms/ok])}
          ;; Hoist scroll computation so both MatrixHeader and MatrixBody share
          ;; the same pagination window. Cost: header waits ~1 frame for first
          ;; resize observation — practically imperceptible.
          ;; Each axis scrolls over a different item set — vertical over the "main"
          ;; dimension (rows or columns depending on transpose), horizontal over the other.
          (let [[_clientHeight clientWidth] (e/input (resize-observer dom/node))
                v-count (#(if transpose columnTs-count rowTs-count))
                h-count (#(if transpose rowTs-count columnTs-count))
                ;; Effective column width: in normal view, columns fill the viewport
                ;; but never shrink below min-column-width. When too many columns to fit,
                ;; the grid overflows horizontally and IndexRing virtual-scrolls them.
                ;; Effective column width for IndexRing bounds — CSS handles visual sizing
                ;; independently via minmax(min-column-width, 1fr). This pixel value is
                ;; only used by Scroll-window to compute limitH/offsetH.
                effective-col-width (#(if transpose column-width (max min-column-width (/ clientWidth (max columnTs-count 1)))))
                scroll-window (Scroll-window 24 v-count dom/node {:column-width effective-col-width
                                                                   :column-record-count h-count})
                [offsetV limitV offsetH limitH] (bounds-by-axis transpose scroll-window)
                ;; +1 for associative: the transposed header is a column (no vertical
                ;; space consumed), so compute-scroll-window's limit is an exact fit.
                ;; Collection views get a free +1 because the sticky header row steals
                ;; vertical space. Match that buffer for smooth property scrolling.
                col-limit (min columnTs-count (#(if associative? (inc limitH) limitH)))]
            (e/amb
              ;; MatrixHeader renders property names as <th scope=col>. CSS transposition
              ;; turns them into visual row headers. Sort handles hidden via CSS for associative.
              (MatrixHeader columnTs offsetH col-limit associative?)
              (MatrixBody columnTs rowTs
                offsetV (min rowTs-count limitV)
                offsetH col-limit
                :associative? associative?)
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
            shorten (name-shortener (map (comp display-edge hfql/symbolic-edge) childrenT))
            from-spec (e/as-vec (e/for [columnT (e/diff-by hfql/symbolic-edge static-columns)]
                                  (e/When
                                      (dom/span
                                        (forms/Checkbox* true :label (e/server (str (shorten (display-edge (hfql/symbolic-edge columnT)))))
                                          :title (e/server (str (hfql/describe-formatted columnT)))))
                                    columnT)))
            suggested (e/as-vec (e/for [columnT (e/diff-by hfql/symbolic-edge suggested-columns)]
                                 (e/When
                                     (dom/span
                                       (forms/Checkbox* pre-checked :label (e/server (str (shorten (display-edge (hfql/symbolic-edge columnT)))))
                                         :title (e/server (str (hfql/describe-formatted columnT)))))
                                   columnT)))]
        (->> (mappend from-spec suggested)
          (sort-by childrenT-order <) ; FIXME order is still not respected WRT star. Suggestions should be injected where star is, if present.
          )))))



(defn- prepare-collection
  "Find collection point, augment suggestions for associative views,
   extract limit and children. Returns [coll-stepT limit children] or nil.
   Single Offload instead of 4 reactive nodes — all sequential server-side Clojure."
  [stepT associative?]
  (when-some [coll-stepT (selection2/find-collection-point stepT)]
    (let [coll-stepT (if associative?
                       (or (hfql/augment-with-suggestions! coll-stepT) coll-stepT)
                       coll-stepT)]
      [coll-stepT
       (::hfql/limit (extract (meta coll-stepT)) COLLECTION-LIMIT)
       (unwrap coll-stepT)])))

(defn- compute-search-axes
  "Compute axis-dependent bindings from already-resolved values.
   Associative views search columns; collection views search rows.
   Single thunk instead of 4 reactive nodes — atomic evaluation prevents glitches."
  [associative? picker-columnsT children coll-stepT]
  (let [columnsT (or picker-columnsT children)
        rowsT (if associative? [coll-stepT] children)]
    {:columnsT columnsT
     :rowsT rowsT
     :search-items (if associative? columnsT rowsT)
     :search-columnTs (when-not associative? columnsT)}))

(defn- compute-effective-axes
  "Post-filter: determine effective columns, rows, and entry count.
   Associative views filter columns; collection views filter rows.
   Single thunk instead of 3 reactive nodes."
  [associative? filtered-items columnsT rowsT rowsT-count]
  (let [eff-cols (if associative? filtered-items columnsT)
        eff-rows (if associative? rowsT filtered-items)]
    [eff-cols
     eff-rows
     (if associative? (count eff-cols) (bounded-count rowsT-count eff-rows))]))

(e/defn PrepareMatrixData
  "Data pipeline: nav → column selection → search filter → sort.
   Renders ColumnPicker UI (checkboxes) into the parent DOM context.
   Calls Body with prepared data: [coll-stepT columnsT sorted-rowsT entries-count limit]."
  [stepT associative? Body]
  ;; HFQL tree nodes are unserializable — each value must stay in its own
  ;; e/server to avoid client-side destructuring (which triggers wire transfer).
  ;; Batch functions run atomically on server; accessors extract from cached result.
  (let [_ (e/server (Offload #(do ((glitch-guard hfql/nav) stepT) nil) {:label 'run-query}))
        ;; ColumnPicker renders checkboxes into the matrix-view div
        picker-columnsT (e/server (when-not associative? (ColumnPicker stepT)))
        ;; Batch A: single Offload for find-collection-point + augment + limit + children
        coll-data (e/server (e/Offload #(prepare-collection stepT associative?)))
        coll-stepT (e/server (nth coll-data 0 nil))
        limit (e/server (nth coll-data 1 COLLECTION-LIMIT))
        children (e/server (nth coll-data 2 nil))
        ;; Batch B: atomic axis computation — all fields update together on input change
        search-axes (e/server (#(compute-search-axes associative? picker-columnsT children coll-stepT)))
        columnsT (e/server (:columnsT search-axes))
        rowsT (e/server (:rowsT search-axes))
        search-items (e/server (:search-items search-axes))
        search-columnTs (e/server (:search-columnTs search-axes))
        rowsT-count (e/server (e/Offload #(if associative? 1 (bounded-count limit rowsT))))
        search-items-count (e/server (#(if associative? (count columnsT) rowsT-count)))]
    (search/Filtered (e/server (search/eager-search-BIG-O-N-SQUARE associative?))
      search-items-count
      (e/server search-items)
      (e/server search-columnTs)
      (e/fn [filtered-items]
        ;; Batch C: atomic post-filter axes — effective columns/rows/count
        (let [eff-axes (e/server (#(compute-effective-axes associative? filtered-items columnsT rowsT rowsT-count)))
              effective-columnsT (e/server (nth eff-axes 0))
              effective-rowsT (e/server (nth eff-axes 1))
              entries-count (e/server (nth eff-axes 2))]
          (sort/Sorted (e/server effective-rowsT)
            (e/fn [sorted-rowsT]
              (Body coll-stepT effective-columnsT sorted-rowsT entries-count limit)))))
      ;; Check filtered? on the collection VALUE (source of truth), not on children seq metadata.
      ::search/filtered? (e/server (e/Offload #(either (constantly false) hfql/filtered? (hfql/nav* coll-stepT)))))))

(e/defn RenderMatrix
  "Pure rendering — title, count pill, search input, and Matrix table.
   Data pipeline delegated to PrepareMatrixData."
  [stepT & {:keys [associative?]}]
  (dom/div
    (dom/props {:class ["matrix-view" ({true "error"} (e/server (some? (::hfql/error ((glitch-guard meta) stepT)))))]})
    ;; Title in chrome bar
    (in utils/*card-chrome*
      (rendering/Step-Title stepT))
    (PrepareMatrixData stepT associative?
      (e/fn [coll-stepT columnsT sorted-rowsT entries-count limit]
        ;; Count pill in chrome bar
        (in utils/*card-chrome*
          (dom/div (dom/props {:class "count"})
            (dom/span (dom/props {:data-tooltip (#(rendering/format-count entries-count limit associative?))})
                      (dom/text (#(let [[l r] (if associative? ["{" "}"] ["[" "]"])]
                                    (str l entries-count r)))))))
        (e/amb
          ;; Search in chrome bar
          (in utils/*card-chrome*
            (search/SearchInput! sort/*disabled?*))
          ;; Matrix rendering
          (binding [rendering/*rowT (e/server ((glitch-guard selection2/find-selectable-point) coll-stepT))]
            (Matrix columnsT (#(if associative? 1 entries-count)) sorted-rowsT :associative? associative?)))))))

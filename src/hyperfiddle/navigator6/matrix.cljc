(ns hyperfiddle.navigator6.matrix
  (:require
   [clojure.math :as math]
   [contrib.data :refer [clamp]]
   [contrib.missionary-contrib :as mx]
   [hyperfiddle.api :as hf]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window IndexRing #?(:cljs resize-observer)]]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.hfql2.analyzer :as ana]
   [hyperfiddle.ct.cofree :refer [unwrap]]
   [hyperfiddle.ct.monoid :refer [mappend]]
   [hyperfiddle.hfql2 :as hfql]
   [hyperfiddle.navigator6.rendering :as rendering]
   [hyperfiddle.navigator6.search :as search]
   [hyperfiddle.navigator6.selection :as selection]
   [hyperfiddle.navigator6.selection2 :as selection2]
   [hyperfiddle.navigator6.sorting :as sort]
   [hyperfiddle.navigator6.utils :as utils :refer [in glitch-guard]]
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

(e/defn MatrixHeader [columnsT]
  (dom/thead
    (dom/tr
      (e/amb
        (dom/th (dom/props {:class "transpose-affordance matrix__diagonal" :aria-label "transpose" :style {:width "100%"}})
                (Command (constantly [`Transpose!]) (dom/On* "click" identity nil) (constantly nil)))
        (let [shorten (e/server (name-shortener (map hfql/symbolic-edge columnsT)))
              columns-index (e/server (into {} (map (juxt hfql/id identity)) columnsT))]
          (e/for [edge (e/diff (e/server (map hfql/id columnsT)))]
            (let [columnT (e/server (get columns-index edge))
                  form (e/server (str (shorten (hfql/symbolic-edge columnT))))]
              (dom/th (dom/props {:scope :col})
                      (dom/span (dom/props {:class "column-name"}) (dom/text form)
                        (e/server (rendering/RenderTooltip ::hfql/ColumnHeaderTooltip nil columnT nil)))
                      (e/client
                        (dom/span
                          (dom/props {:class "sort-handle"
                                      :title "Sort"
                                      :data-sort-order (str (e/server (column-sort-order columnT sort/*sort-spec*)))})
                          (let [!direction (atom (cycle ['. '> '<]))
                                direction (dom/On "click" (fn [_event] (first (swap! !direction rest))) nil)
                                [t err] (e/Token direction)]
                            (e/Reconcile (e/When t [t [`sort/Sort! (e/server (utils/Delay columnT)) direction]])))))))))))))

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


(e/defn MatrixCell [index cellT]
  (e/client
    (dom/td (dom/props {:style {:--cell-index index}
                        :role :radio
                        :tabindex "0"
                        :aria-selected (e/server (selection2/cell-selected? selection/*local-selection cellT))})
            (let [GetCellT (e/server (utils/Delay cellT))
                  event (dom/On* "click" cell-ignore-click-on-interactive-content nil) ; TODO hoist
                  [token err] (e/Token event)]
              (e/Reconcile
                (when token
                  (dom/props {:aria-busy true})
                  (dispatch-event! :up dom/node (munge-str `Select!) GetCellT)
                  (dom/On* (munge-str `SelectAck!) (fn [_] (token) nil) nil {:capture true}))))
            (e/server (rendering/Render* cellT))))) ; :detail is the name of the spec-defined data slot. I didn't choose the name.

(e/defn RowHandle [group-id rowT]
  (dom/th (dom/props {:scope :row})
          (let [identifiable? (dustingetz.loader/Initialized (e/server (some? (hfql/identify ((glitch-guard hfql/value-at-point) rowT)))) true)
                GetRowT (e/server (utils/Delay rowT))
                [token _] (forms/Checkbox! ::row (e/server (selection2/row-selected? selection/*local-selection rowT))
                            :type :radio
                            :name (str group-id)
                            :disabled (not identifiable?)
                            :title ({true "" false "Object isn't Identifiable and cannot be selected"} identifiable?)
                            :style {:cursor ({true "normal" false "not-allowed"} identifiable?)})]
            (e/Reconcile
              (when token
                (dispatch-event! :up dom/node (munge-str `Select!) GetRowT)
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
        (RowHandle row-group-id rowT)
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
        (e/client ;; trade off:
          ;; e/for on client = ultra smooth row positioning but glitchy row content
          ;; e/for on server = slower row rotation (still acceptable) but no row content glitch.
          (e/for [index (IndexRing limitV offsetV)]
            (MatrixRow row-group-id columnsT
              ;; HACK pick your second level of trade off:
              ;; Option 1 - instant scroll, no server roundtrip.
              ;; Rows will scroll as fast as the browser's main js thread allows it. Cell content will be inconsistent under load.
              ;; The worse the network quality, the less consistent.
              index
              ;; Option 2 – more consistency, slower scroll – round-trip the row index.
              ;; In-browser scroll will still happen as fast as JS allows it, but rows will be off-viewport until the server echoes the row-index.
              ;; If the server is busy, e.g. under load due to many rows × columns, or if network is slow, continuous time semantics will "relieve"
              ;; the row index at almost the same rate as the inconsistent cells' content.
              ;; The server will then echo a final big message containing both latest row indexes and latest cells content.
              ;; Not sure if this behavior is just luck or an emergent, fundamental aspect of the system.
              #_(e/server (identity index))
              ;; trades scroll speed for consistency, but slightly better than siting the entire e/for
              (e/server (e/Offload #(nth rowsT index nil))) offsetH limitH)))))))

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

(e/defn Matrix [columnTs rowTs-count rowTs & {:keys [transpose? single? is-map? row-height column-width min-row-count] :or {row-height 24 #_"1lh" column-width 200 #_ "40ch"}}]
  (e/client
    (dom/table
      (dom/props {:role :radiogroup})
      (let [!transpose (atom true)
            transpose (e/watch !transpose)
            columnTs-count (e/server (count columnTs))]
        (reset! !transpose (boolean transpose?))
        (dom/props {:class ["matrix" ({true "transposed"} transpose) ({true "single"} single?) ({true "map"} is-map?)]
                    :style {:--row-height (str row-height "px")
                            :--column-width (str column-width "px")
                            :--row-count rowTs-count
                            :--column-count columnTs-count}})
        (forms/Interpreter {`Transpose! (e/fn [] ((constantly [::forms/ok]) (transpose! dom/node !transpose)))
                            `Noop (e/fn [] [::forms/ok])}
          (e/amb
            (MatrixHeader columnTs)
            (let [[_clientHeight clientWidth] (e/input (resize-observer dom/node))
                  [offsetV limitV offsetH limitH] (bounds-by-axis transpose (Scroll-window 24 rowTs-count dom/node {:column-width (#(if transpose column-width (/ clientWidth (max columnTs-count 1))))}))]
              (MatrixBody columnTs rowTs
                offsetV (min rowTs-count limitV)
                offsetH (min columnTs-count limitH)))
            (e/amb
              (Command (fn [^js event] [`selection/SetSelection! (.-detail event)]) (dom/On* (munge-str `Select!) identity nil)
                (fn [event]
                  (dispatch-event! :down (.-target event) (munge-str `SelectAck!) nil))))))))))

(e/defn ColumnPicker [stepT]
  (e/server
    (when-let [pull-step (e/Snapshot (e/Filter some? ((glitch-guard selection/select-pull-step) stepT)))]
      (let [stepT+ (hfql/augment-with-suggestions! pull-step)
            childrenT (remove (comp (every-pred ::ana/recursion-point ::ana/recursive-child) hfql/metas) ((glitch-guard unwrap) stepT+))
            static-columns (remove (comp ::hfql/suggested hfql/metas) childrenT)
            suggested-columns (e/Snapshot (filter (comp ::hfql/suggested hfql/metas) childrenT))
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
        (mappend from-spec suggested)))))

(def COLLECTION-LIMIT 10000)

#_(let [monoid #?(:clj clojure.lang.PersistentQueue/EMPTY :cljs #queue [])]
  (defn stream-in [coll]
    (m/relieve
      (m/reductions conj monoid
        (m/seed coll)))))


#_(comment
  (m/? (m/reduce {} nil
         (m/reductions (fn [_ q] (prn [(count q) (sequence q)]) nil) nil
           (stream-in (range 10)))))

  )

(defn- reshape-maps [transpose? single? stepT]
  (when stepT
    (let [value (hfql/value-at-point stepT)]
      (if (and (utils/imap? value) (not (record? value)))
        [true false false (-> (hfql/hfql {* [key val]} (seq value) #_(map #(zipmap [:key :val] %) (seq value))) (hfql/set-parent stepT))]
        [false transpose? single? stepT]))))

(e/defn RenderMatrix [stepT & [transpose? single?]]
  (dom/fieldset
    (e/client (dom/link (dom/props {:rel :stylesheet, :href (e/server (str hf/*base-static-resource-path* "matrix.css"))})))
    (e/client (dom/link (dom/props {:rel :stylesheet, :href (e/server (str hf/*base-static-resource-path* "matrix_vertical.css"))})))
    (e/client (dom/link (dom/props {:rel :stylesheet, :href (e/server (str hf/*base-static-resource-path* "matrix_horizontal.css"))})))
    (dom/props {:class "" #_"entity-children hyperfiddle-navigator6__block"})
    (let [... (e/server (Offload #(do ((glitch-guard hfql/nav) stepT) (reshape-maps transpose? single? stepT)) {:label 'run-query}))
          is-map? (e/server (nth ... 0))
          transpose? (e/server (nth ... 1)) ; workaround sited destructuring
          single? (e/server (nth ... 2))
          stepT (e/server (nth ... 3))
          [legend top-left] (dom/legend
                              [dom/node (dom/span (dom/props {:style {:float :inline-start, :display :flex, :flex-wrap :wrap}})
                                                  (rendering/Step-Title stepT)
                                                  dom/node)])
          columnsT (e/server (in legend (ColumnPicker stepT)))
          stepT (e/server ((glitch-guard selection/get-rows-container) stepT))
          rowsT (e/server (#(if (= ::ana/pull (::ana/type ((glitch-guard hfql/metas) stepT))) ; FIXME smell
                              [stepT]
                              ((glitch-guard unwrap) stepT))))
          rowsT-count (e/server (e/Offload #(bounded-count COLLECTION-LIMIT rowsT)
                                  #_{:label 'count-rows})) ; 𝓞(n)
          ]
      (search/Filtered (e/server (search/eager-search-BIG-O-N-SQUARE))
        rowsT-count
        (e/server rowsT)
        (e/server columnsT)
        (e/fn [rowsT]
          (let [rowsT-count (e/server (bounded-count rowsT-count rowsT))]
            (in top-left
              (dom/span (dom/props {:style {:order -1}
                                    :class "count"})
                        (dom/text (#(if is-map?
                                      (rendering/format-map-entries-count rowsT-count COLLECTION-LIMIT)
                                      (rendering/format-collection-count rowsT-count COLLECTION-LIMIT))))))
            (e/amb
              (in top-left (dom/span (dom/props {:style {:order -2}})
                                   (search/SearchInput! false)))
              (sort/Sorted (e/server rowsT)
                (e/fn [rowsT]
                  (binding [rendering/*rowT (e/server ((glitch-guard selection/select-pull-step) stepT))]
                    (Matrix columnsT rowsT-count rowsT :transpose? transpose?, :single? single? :is-map? is-map?)))))))))))

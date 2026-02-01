(ns hyperfiddle.navigator6.selection2
  "G: Logic in this namespace is written in an uncommon form (boolean algebra) because:
  - selection, selection resolution, and navigation onto selected points have MANY edge
    cases.
  - I want truth tables to be covered. I want an exhaustive listing of all possible
    conditional branches. Because this code will be called from HFQL Navigator (Electric)
    and *THERE WILL* be glitches and inconsistent states. I want the ability to quickly
    detect and easily track them.
  - I want all branches to be explicit. E.g. no implicit nil, no implicit fallback branch.
    Implicit conditional branches cause Swiss Cheese bugs. I've been bitten too many times.
    Covering truth tables implies introducing redundancy and 'obvious' dead branches sooner
    or later. I don't consider this a problem. I'm not lazy and it communicates better.
    Trivial cases are never trivial. Uttering \"trivial\" is (not exhaustively) either
    symptomatic of Dunning–Kruger effect, a lack of understanding how the human brain works,
    or a lack of social skills.
  - Asserting for state consistency is not enough:
    - It may be disabled at any point from configuration (e.g. prod)
    - As of today, throwing can cause Electric to crash without reporting the exception.
    - To me, in this case, asserting is a communication tool, a dev-time convenience and all
      tests should pass with assertions disabled.
  "
  (:require
   [hyperfiddle.ct.pair :refer [pair? pair fst snd]]
   [hyperfiddle.hfql2 :as hfql :refer [hfql?]]
   [hyperfiddle.hfql2.protocols :refer [Identifiable -hfql-resolve]]
   [hyperfiddle.hfql2.analyzer :as ana]
   [hyperfiddle.hfql2.coalgebra :as coalgebra :refer [star-traversal?]]
   [hyperfiddle.ct.either :refer [either either? right left right? left? from-right from-left]]
   [hyperfiddle.navigator6 :as-alias navigator]
   [hyperfiddle.rcf :refer [tests]]
   ))

(comment
  (hyperfiddle.rcf/enable! true)
  (hyperfiddle.rcf/enable! false)
  )



;;;; Selection identification
;;;;;; Validating symbolic coordinates

(defn- symbolic-constructor-form? [value]
  {:post [(boolean? %)]}
  (and (seq? value)
    (qualified-symbol? (first value))))

(tests
  (symbolic-constructor-form? nil)
  := false
  (symbolic-constructor-form? 42)
  := false
  (symbolic-constructor-form? :db/ident)
  := false
  (symbolic-constructor-form? '[:db/ident :artist/name])
  := false
  (symbolic-constructor-form? '(d/entity [:db/ident :artist/name]))
  := true
  (symbolic-constructor-form? '(d/entity :artist/name))
  := true
  (symbolic-constructor-form? '(foo)) ; 'foo is ambiguous and may collide. We want qualified constructors.
  := false
  (symbolic-constructor-form? `(foo)) ; note the backtick
  := true
  (symbolic-constructor-form? '#'foo) ; expands to '(var foo) – not allowed. `var` is a special form in clojure but we don't want to treat it as such in this case.
  := false
  (symbolic-constructor-form? '(find-var foo))
  := false
  (symbolic-constructor-form? '(clojure.core/find-var foo))
  := true

  )

(defn- symbolic-column? [value]
  {:post [(boolean? %)]}
  (or (ident? value)
    (string? value)
    (symbolic-constructor-form? value)))

(tests
  (symbolic-column? nil)
  := false
  (symbolic-column? :foo)
  := true
  (symbolic-column? ::foo)
  := true
  (symbolic-column? 'foo)
  := true
  (symbolic-column? `foo)
  := true
  (symbolic-column? '#'foo)
  := false
  ;; Matches this HFQL syntax edge case: (hfql [#'str] {str true, 'str true})
  ;; This is documented in HFQL, but it's not an obvious case. So here is a reminder:
  ;; We want to express "call the `str` function on the entire map"
  ;; But the map contains the key `str` – a function value – and the symbol 'str.
  ;; What should HFQL do? Call `str`? looking it up by value? by symbolic form?
  ;; HFQL disambiguate this case with syntax. HFQL syntax says:
  ;; [#'str] means "always call the function str, don't look it up."
  ;; Note: to lookup a var object, quote it: ['#'str]
  (symbolic-column? '(clojure.core/find-var foo))
  := true
  (symbolic-column? "foo")
  := true
  (symbolic-column? 42)
  := false
  (symbolic-column? [:db/ident :artist/name])
  := false

  )

(defn- symbolic-selection-coordinates? [pair]
  ;; [column row]
  ;; Why column × row and not row × column? It's arbitrary.
  ;; I picked this order to mimick Excel formula, e.g. `A2`.
  ;; G — 11 dec 2025 No this is inaccurate. I now remember I picked column × row because [:artist/name `(d/entity lennon-id)] is isomorphic to (:artist/Name !lennon) – Note the function application syntax.
  ;; It can be toggled if needed.
  {:pre [(pair? pair)] ; G — This is for me. I want to track eventual glitches and catch them early.
   :post [(boolean? %)]}
  (or ;; P
    ;; A
    (and (pair? pair)
      (or
        (and ;; A ∧  B ∧  C ⇒  P
          (symbolic-column? (fst pair))
          (symbolic-constructor-form? (snd pair)))
        (and ;; A ∧  B  ∧ ¬C ⇒ ¬P
          (some? (fst pair)) ; B
          (or ; ¬C
            (nil? (snd pair))
            (not (symbolic-constructor-form? (snd pair))))
          false ; this branch is always false but is listed for communication: to cover the truth
                ; table in order, and guarantee branches exclusivity.
          )
        (and ;; A ∧ ¬B ∧  C ⇒ P
          (nil? (fst pair))
          (symbolic-constructor-form? (snd pair)))
        (and ;; A ∧ ¬B ∧ ¬C ⇒ P
          (nil? (fst pair))
          (nil? (snd pair)))
        ))
    ;; ¬A
    (and (not (pair? pair)) ; G — This is for me. I want to track eventual glitches and catch them
                            ; in prod where assertions may be elided.
      (println `symbolic-selection-coordinates? "received object is not a pair" (type pair) (pr-str pair))
      false)
    ))



#?(:clj
   (tests
     (symbolic-selection-coordinates? [nil nil])
     :throws AssertionError ; must be a proper pair, not a vector of size 2 posing as a pair. Size=2 may be accidental.

     ))

(tests
  (symbolic-selection-coordinates? (pair nil nil))
  := true

  (symbolic-selection-coordinates? (pair nil '(d/entity 42)))
  := true

  (symbolic-selection-coordinates? (pair :db/id '(d/entity 42)))
  := true

  (symbolic-selection-coordinates? (pair :db/id nil))
  := false

  )

;;;;;; Emitting symbolic coordinates

(def ^:private selection-point?
  ;; G — for now I believe not all HFQL points to be selectable.
  ;; - Some can be rendered but not selected from the UI. E.g. page root or an options list.
  ;; – Actually I'm not even sure. Not sure how to define this predicate.
  ;; - For now the predicate is the largest possible set. Shrink the set if needed.
  ;; - TODO figure it out.
  hfql?)

(defn- row? [point] (= ::ana/pull (::ana/type (hfql/metas point)))) ; A row is a pull. Children of a row are cells.
(defn- cell? [point] (let [parent (hfql/parent point)]
                       (and (some? parent)
                         (row? parent)))) ; A cell is a direct child of a row.

(defn- symbolize-form [point]
  (or (and (row? point) ; row is a pull and pull isn't an edge. A pull has not name on its own.
        nil)
    (and (cell? point) ; a cell cannot be a pull, so it must have a name. E.g. keyword or traversal.
      (hfql/symbolic-edge point))
    nil))

(defn- symbolize-value [point]
  (hfql/identify
    (from-right nil
      (or (and (row? point) ; a row holds the current value but doesn't project it on its own. Cells does.
            (hfql/value-at-point point))
        ;; a cell have the row value in scope and we want the symbolic coordinates to be [symbolic-column symbolic-row-value].
        ;; So we treat the same as rows with regard to coordinates.
        ;; Why? because UI concerns: We want to highlight the cell as "selected" onto a grid. So we must now which row and column of the grid.
        ;; One could ask if we should use the value-at-point – i.e. the row's value – or the value after calling `nav` – i.e. the cell's value.
        ;; The answer is we want the value-at-point (row) because we want to select the cell *in the context of a row*
        ;; If we call `nav` then we will select the cell's content, lose the row context, and we won't be able to tell onto which row it was selected.
        (and (cell? point)
          (hfql/value-at-point point))
        nil))))

(defn symbolic-selection-coordinates 
  ([point] (symbolic-selection-coordinates point nil))
  ([point row-positional-index]
   {:pre [(selection-point? point) (or (nil? row-positional-index) (nat-int? row-positional-index))]
    :post [(symbolic-selection-coordinates? %)]}
   (or
     ;; Selected point is identifiable.
     (and (selection-point? point) ; selected point is an HFQL point
       (hfql/identifiable? (from-right nil (hfql/value-at-point point))) ; selected point's value is Identifiable. This means we should be able to identify it. Not that identification is guaranteed to succeed.
       (some? (hfql/identify (from-right nil (hfql/value-at-point point)))) ; value is identified
       (symbolic-constructor-form? (symbolize-value point)) ; ensure identity is a valid symbolic form (qualified call)
       (or (and (row? point) ; selected point is a row
             (pair nil (symbolize-value point)))
         (and (cell? point) ; selected point is a cell
           (symbolic-column? (symbolize-form point)) ; ensure column is representable
           (pair (symbolize-form point) (symbolize-value point)))
         (and
           (println "Selected point cannot be selected because while it is a selectable HFQL point, it is neither a row nor a cell." (type point) point)
           (pair nil nil))))
     ;; Selected point is not identifiable.
     ;; We can still generate coordinates if:
     ;;  - the select point is a cell
     ;;  - and either:
     ;;    - the parent is identifiable
     ;;    - the parent's parent (grandparent) multiplicity is ONE
     ;; Why? because it implies the current point is a row (see definition of `cell?`).
     ;; And a row is necessarily a pull, so we know the cell is addressable by a column of the pull + the object at point.
     ;; Addressing the object at point will require DI, and can ONLY be defined under multiplicity one.
     (and (selection-point? point)
       (cell? point)
       (or (hfql/identifiable? (from-right nil (hfql/value-at-point (hfql/parent point))))
         (not (hfql/multiplicity-many? (hfql/parent (hfql/parent point)))))
       (symbolic-column? (symbolize-form point)) ; ensure column is representable (e.g. arbitrary object as a key in a map is not URL-representable)
       (pair (symbolize-form point) `(hfql/current-point)))
     ;; Selected point is not identifiable.
     ;; If it's a row then the row object isn't identifiable either.
     ;; If it's a cell then the row object for this cell is not identifiable either.
     ;; But if we know the row's positional index we can still generate coordinates.
     ;; Resolving those coordinates will require DI and can only be resolved under multiplicity MANY.
     ;; So if this is a row, the parent must be multiplicity MANY.
     ;; If this is a cell, the grandparent must be multiplicity MANY.
     (and (selection-point? point)
       (nat-int? row-positional-index)
       (row? point)
       (hfql/multiplicity-many? (hfql/parent point))
       (pair nil `(hfql/nth ~row-positional-index)))
     (and (selection-point? point)
       (nat-int? row-positional-index)
       (cell? point)
       (hfql/multiplicity-many? (hfql/parent (hfql/parent point)))
       (pair (symbolize-form point) `(hfql/nth ~row-positional-index)))
     ;; At this point we have a selectable HFQL point that is
     ;; not identifiable
     ;; the parent is not identifiable
     ;; there is no known positional index
     ;; We can't produce selection coordinates for this point.
     (and (selection-point? point)
       (not (nat-int? row-positional-index))
       (do (println "Selected point cannot be selected because it is neither Identifiable nor reliably addressable by positional index" (type point) point)
           (pair nil nil)))
     (do (println "Selected point cannot be selected because it is not a selectable HFQL point." (type point) point)
         (pair nil nil)))))

(tests
  (defrecord TestType [a b]
    Identifiable
    (-identify [_] `(->TestType ~a ~b)))

  (defmethod -hfql-resolve `->TestType [[_ a b]] (->TestType a b)))

(tests
  (symbolic-selection-coordinates (hfql/hfql [.-a] (->TestType 1 2)))
  := '[nil (hyperfiddle.navigator6.selection2/->TestType 1 2)]

  )

(tests
  (->> (hfql/hfql [.-a] (->TestType 1 2))
    (hfql/find-cell-by-symbolic-column '.-a)
    (symbolic-selection-coordinates))
  := '[.-a (hyperfiddle.navigator6.selection2/->TestType 1 2)]

  )



;;;; Selection resolution
;;;;;; Resolving identifiable objects

(defn resolve-identifiable-object [symbolic-constructor-form]
  {:pre [(or (nil? symbolic-constructor-form) (symbolic-constructor-form? symbolic-constructor-form))]
   :post [(or (nil? %) (either? %))]}
  (or (and (some? symbolic-constructor-form)
        (hfql/hfql-resolve* symbolic-constructor-form))
    nil))

(tests
  (resolve-identifiable-object nil)
  := nil
  (resolve-identifiable-object (hfql/identify (->TestType 1 2)))
  := (right (->TestType 1 2))

  )

;;;;;; Turning symbolic selection coordinates into an HFQL selection point

(defn- hydrate-point [point value-at-point]
  {:pre [(or (nil? value-at-point) (either? value-at-point))]}
  (or
    (and (some? point)
      (hfql? point)
      (hfql/update-scope point assoc '% value-at-point))
    nil))

(defn- find-value-at-point [selection-point]
  {:pre [(selection-point? selection-point)]}
  ;; special case ; TODO document
  (or (and (= ::hfql/map-entry (::hfql/value-type (hfql/metas selection-point)))
        (hfql/value-at-point (hfql/parent selection-point)))
    (hfql/value-at-point selection-point)))

(letfn [(traversal? [selection-point] (= ::ana/traversal (::ana/type (hfql/metas selection-point))))
        (pull? [selection-point] (and (some? selection-point)
                                   (= ::ana/pull (::ana/type (hfql/metas selection-point)))))
        (parent-is-pull? [selection-point] (pull? (hfql/parent selection-point)))
        (child-is-pull? [selection-point] (pull? (first (hfql/next-steps selection-point))))]
  (defn- traversal-with-pull-continuation_in-pull? [selection-point]
    {:pre [(selection-point? selection-point)]
     :post [(boolean? %)]}
    (and (traversal? selection-point)
      (parent-is-pull? selection-point)
      (child-is-pull? selection-point))))

(defn- step-over_traversal-with-pull-continuation_in-pull [selection-point]
  {:pre [(traversal-with-pull-continuation_in-pull? selection-point)]
   :post [(selection-point? selection-point)]}
  (first (hfql/next-steps selection-point)))

(defn- step-over-eventual_traversal-with-pull-continuation_in-pull [resolved-selection-point] ; Edge case
  {:pre [(or (nil? resolved-selection-point) (selection-point? resolved-selection-point))]
   :post [(or (nil? %) (selection-point? %))]}
  ;; Edge case in this HFQL form: [{:x [:y]}] – namely traversal with continuation in pull
  ;; Compare with this form: [:x] – namely regular pull, or leaf traversal in pull.
  ;; Problem: In the case of [{:x [:y]}], navigation sees a duplicate value for '%. producing an off-by-one UI: I.e. selecting a cell renders not the cell but the row itself.
  ;; This is not an HFQL bug per se. But it produces incorrect UIs.
  ;; Explanation: navigating an HFQL form (i.e. drilling down) is a step-by-step process, in tree-seq order.
  ;; Steps are:
  ;;  - For `[:x]`
  ;;    1a. current point       – e.g. {:x 1, :z 2} – raw value
  ;;    2a. pull for [:x]       – e.g. {:x 1} – pulled
  ;;    3a. traversal for :x    – e.g. 1      – traversed
  ;;  - For `[{:x [:y]}]`
  ;;    1b. current point       – e.g. {:x {:y 1}, :z 2} – raw value
  ;;    2b. pull for [:x]       – e.g. {:x {:y 1}} – pulled for :x. dropped :z
  ;;    3b. traversal for :x    – e.g. {:y 1}      – traversed
  ;;    4b. pull for [:y]       – e.g. {:y 1}      – pulled for :y, no change. Not a duplicate either. Please think what would have happened for `[:y *]`. This is crucial.
  ;;    5b. traversal for :y    – e.g. 1           – traversed
  ;; We can see that in the case of `[:x]`, selecting the cell for `:x` can only produce the leaf child of the pull: the traversal for :x. (step 3a)
  ;; In the case of `[{:x [:y]}]`, selecting the cell for `:x` also produce the traversal for `:x` (3b.), but it is an intermediate step, irrelevant to the UI.
  ;; We want the render the pull instead (4b.).
  ;; 2026-01-08: Buggy behavior is: % is the same for both step 2b and step 3b. That's why the navigator UI renders the row itself.
  ;;             Fix is to "step over" 3b.. And adapt `row-selected?` and `cell-selected?` accordingly.
  ;; Also considered:
  ;;  - Detecting this edge case at analysis time, and handling it differently in HFQL runtime.
  ;;    Ruled out because this is not a special case of the syntax. Pulls and traversals are allowed to nest. Nesting them do not make them special.
  ;;  - Filtering the tree-seqed HFQL at navigation time.
  ;;    Ruled out because the intermediate traversal is a legit step. It is not meaningful to a UI consumer, but it may be meaningful to others.
  (let [selection-point resolved-selection-point]
    (or (and (some? selection-point)
          (traversal-with-pull-continuation_in-pull? selection-point)
          (not= ::ana/many (::ana/multiplicity (hfql/metas selection-point))) ; TODO extract to a generic predicate
          (step-over_traversal-with-pull-continuation_in-pull selection-point))
      (and (some? selection-point)
        #_(not (traversal-with-pull-continuation_in-pull? selection-point))
        selection-point)
      (and (not (some? selection-point))
        nil))))

(declare selection-positional-index)

(defn resolve-selection-coordinates [selection-point symbolic-selection-coordinates]
  {:pre [(selection-point? selection-point)
         (or (nil? symbolic-selection-coordinates) (symbolic-selection-coordinates? symbolic-selection-coordinates))]
   :post [(or (nil? %) (selection-point? %))]}
  ;; (def _selection-point selection-point)
  (when-not (row? selection-point)
    (println `resolve-selection-coordinates "WARNING: the received selection point is not a row."))
  (or
    ;; selection coordinates are valid – we should be able to resolve an object
    (and (symbolic-selection-coordinates? symbolic-selection-coordinates)
      (let [[symbolic-column symbolic-constructor-form] symbolic-selection-coordinates
            cell-selection? (some? symbolic-column)
            ;; We call into userland to resolve.
            either-identifiable-object (binding [hfql/*collection-at-point* (some-> (hfql/parent selection-point) hfql/nav*) ; cached
                                                 hfql/*value-at-point* (find-value-at-point selection-point)]
                                         ;; An object can be Identifiable, identification can succeed, but resolution can fail (e.g. incorrect DI, external dependency failure, …)
                                         ;; `resolve-identifiable-object` can return:
                                         ;; - nil: object isn't resolvable
                                         ;; - Right `x`: object is resolvable and successfully resolved to `x`
                                         ;; - Left `x`: object is resolvable but resolution failed
                                         (resolve-identifiable-object symbolic-constructor-form))]
        ;; Resolution succeeded – we should be able to produce a row or a cell
        (or
          (and (right? either-identifiable-object)
            (let [seeded-selection-point (-> (hydrate-point selection-point either-identifiable-object)
                                           (coalgebra/vary-static-meta assoc ::hfql/positional-index (selection-positional-index symbolic-selection-coordinates)))]
              (or (and cell-selection?
                    (or (and ; there's a symbolic column – this is a cell selection – we should be able to hydrate the HFQL row and focus it on the corresponding cell.
                          (row? seeded-selection-point)
                          (step-over-eventual_traversal-with-pull-continuation_in-pull ; Edge case
                            (or ; perf optimization – attempt to resolve without suggesting.
                              (hfql/find-cell-by-symbolic-column symbolic-column seeded-selection-point)
                              (hfql/find-cell-by-symbolic-column symbolic-column (hfql/augment-with-suggestions! seeded-selection-point)))))
                      (and ; we are resolving a cell selection but the given selection point is not a row. We can't focus on a cell.
                        (not (row? seeded-selection-point))
                        (println "The given selection contains a column and so targets a cell. But the current selection point is not a row. Can't focus on a cell on an object that is not a row." (::ana/type (hfql/metas seeded-selection-point)))
                        (def _selection-point seeded-selection-point) ; G – for me, for debugging.
                        nil)))
                ;; there's no symbolic column to focus on. This is a row selection.
                seeded-selection-point)))
          ;; Object was Identifiable, got identified, but identification failed.
          (and (left? either-identifiable-object)
            ;; let user explore the resolution error. Doesn't differentiate row from cell selection.
            (let [target-point (or (and ; there's a symbolic column – this is a cell selection – we should be able to hydrate the HFQL row and focus it on the corresponding cell.
                                     cell-selection?
                                     (row? selection-point)
                                     (step-over-eventual_traversal-with-pull-continuation_in-pull ; Edge case
                                       (or ; perf optimization – attempt to resolve without suggesting.
                                         (hfql/find-cell-by-symbolic-column symbolic-column selection-point)
                                         (hfql/find-cell-by-symbolic-column symbolic-column (hfql/augment-with-suggestions! selection-point)))))
                                 ;; otherwise fallback to row selection
                                 selection-point)]
              (-> (hfql/hfql [*] (from-left nil either-identifiable-object)) ; reinject error as a value – intentionally severs the HFQL tree to render a generic error.
                (hfql/set-parent target-point) ; graft this generic HFQL tree onto the failed parent. Use case: render title in the context of parent selection.
                (coalgebra/vary-static-meta assoc
                  ::hfql/positional-index (selection-positional-index symbolic-selection-coordinates)
                  ::hfql/error (from-left nil either-identifiable-object)))))
          ;; Unexpected resolution outcome.
          (and (some? either-identifiable-object)
            (not (either? either-identifiable-object))
            (println "Unexpected selection coordinates resolution result. Expected one of \"unresolvable\", \"successfully resolved\" or \"failed to resolve\". Received: " (type either-identifiable-object) ".")
            nil)
          ;; Unresolvable object. Do nothing.
          (and (nil? either-identifiable-object)
            nil))))
    ;; no valid selection coordinates
    nil))


(comment
  (hfql/value-at-point _selection-point)
  (hfql/nav _selection-point)
  (hfql/nav (hfql/parent _selection-point))
  (resolve-selection-coordinates _selection-point (pair nil '(hyperfiddle.hfql2/nth 2)))
  )

(tests
  "Resolve a selected row"
  (def _selection-point (hfql/hfql [.-a] (->TestType 1 2)))
  (row? _selection-point)
  := true

  (hfql/values _selection-point)
  := '({.-a 1} 1)

  (symbolic-selection-coordinates _selection-point)
  := '[nil (hyperfiddle.navigator6.selection2/->TestType 1 2)]

  (def _resolved-selection-point
    (resolve-selection-coordinates
      (hfql/hfql [.-a] nil) ; not seeded!
      (symbolic-selection-coordinates _selection-point)))

  (selection-point? _resolved-selection-point)
  := true

  _resolved-selection-point := _selection-point

  (hfql/values _resolved-selection-point)
  := '({.-a 1} 1)

  )

(tests
  "Resolve a selected cell"
  (def _selection-point (hfql/find-cell-by-symbolic-column '.-a (hfql/hfql [.-a] (->TestType 1 2))))
  (cell? _selection-point)
  := true

  (hfql/values _selection-point)
  := '(1)

  (symbolic-selection-coordinates _selection-point)
  := '[.-a (hyperfiddle.navigator6.selection2/->TestType 1 2)]

  (def _resolved-selection-point
    (resolve-selection-coordinates
      (hfql/hfql [.-a] nil) ; not seeded!
      (symbolic-selection-coordinates _selection-point)))

  (selection-point? _resolved-selection-point)
  := true

  _resolved-selection-point := _selection-point

  (hfql/values _resolved-selection-point)
  := '(1)

  )

;;;;;; Finding the selectable point at the current point (currently navigated point)

(defn- points-syntactically-equal? [point-a point-b]
  (or (identical? point-a point-b)
    (and (hfql? point-a) (hfql? point-b)
      (= (::ana/id (hfql/metas point-a))
         (::ana/id (hfql/metas point-b))))))

(defn find-selectable-point
  ;; - A Selectable point is not to be confused with a Navigable point.
  ;;   - Navigable point :: Any HFQL point we can render (e.g {(attributes) […]}).
  ;;   - Selectable point :: An HFQL point onto which we can click.
  ;;     - It is advised to produce a UI advertising "selectablity" and "non selectablity".
  ;;     - Selecting such point will cause a navigation iff the value-at-point is Identifiable or
  ;;       if the point is tagged with `::navigator/select`.
  ;;     - Selecting a point with a non-Identifiable value-at-point and without
  ;;       `::navigator/select` is undefined behavior and, if the UI somehow allows it, should
  ;;       usually result in a no-op.
  ;;     - All Selectable points are Navigable. Not all Navigable points are Selectable (e.g. the
  ;;       page root, an options list, etc. — G I'm not actually sure).
  ;;     - Tagging a node with `::navigator/select` doesn't make it selectable. It only advise
  ;;       the navigator how to interpret the selection action.
  ;;
  ;; - Selection may happen on a row or a cell.
  ;; - Cell selection always happen on a given row.
  ;;   - It is not possible to select a cell without knowing its corresponding row. Therefore a
  ;;     selectable point is always a row.
  ;;   - Cell selection is formed by a row selection + the cell symbolic identifier. See
  ;;     `symbolic-selection-coordinates`.
  ;;
  ;; - Therefore the current point must either be:
  ;;   - Object case :: a pull (a row), in which case the selectable point is the current point
  ;;     – e.g. transposed row presented as a form.
  ;;   - Pull-many case :: a collection of pulls (a star traversal, aka a collection of rows),
  ;;     in which case any row is selectable, an so we define the first row as the selectable
  ;;     point.
  ;;     - Problem: what if the multiplicity is many but there are no rows available because
  ;;       the collection is empty? E.g. due to search. This is an ergonomic edge case of
  ;;       Cofree Comonads: where the Cofree tree is potentially infinite, is structurally
  ;;       known, but ends with no next value. How do we know the structure of the supposedly
  ;;       next value, in its absence? In that case we should be able to either:
  ;;       1. Inject a dummy, stub value and ask for the Cofree children again, so to get one
  ;;          dummy (but structurally correct) child. Laziness ~makes this safe~.
  ;;          - Addendum: I though this was safe but multiplicity many traversals do run IO for
  ;;            some reason I don't remember. So it's not that safe.
  ;;       2. OR re-unfold the static child structure from the captured AST into a fresh,
  ;;          unseeded Cofree point. Then we re-parent it to the current point and seed it
  ;;          accordingly.
  ;;       To me `1.` looks like a hack and `2.` looks like the idiomatic approach. But
  ;;       thinking about it, I'm tempted to think it is actually the opposite. `1.` is
  ;;       self-contained and will always be structurally correct. Even if the API may look
  ;;       odd. `2.` on the other hand implies grafting a Cofree tree onto an other. This can
  ;;       be brittle and hard to maintain.
  ;;   - Traversal case :: a traversal, in which case the traversal's continuation must be a
  ;;     row or a collection of rows
  ;;     - If the traversal's continuation is itself a traversal (e.g. {identity {identity
  ;;       []}}), then the continuation's continuation must be a row or a collection of rows,
  ;;       recursively.
  ;;     - The first traversal continuation found to be row will be the selectable point.
  ;;     - If the continuation is found to be a collection of rows, then the rules of finding
  ;;       the selectable point for a collection of rows applies (pull-many case).
  ;;     - Edge case: what about the recursive `{f ...}` form? In that case we never find a
  ;;       selectable point, because we defined a selectable point as a row. And this
  ;;       expression doesn't define any row. This is a UI constraint. We defined a selectable
  ;;       point as a row because we render grids. I believe it is possible to render a UI for
  ;;       `{f ...}`, though it doesn't strike me as obvious.
  ;;   - Default case :: a value point. Any point that is neither a traversal nor a pull. Such
  ;;     point is not selectable.
  ;;

  ([current-point]
   (find-selectable-point ::unset current-point))
  ([starting-point current-point] ; to guard against infinite recursion
   {:pre [(hfql? current-point)]
    :post [(or (nil? %) (selection-point? %))]}
   (or
     (and (not (points-syntactically-equal? starting-point current-point)) ; detect loops
       (or
         ;; Object case
         (and (row? current-point)
           current-point)
         ;; Special reshaped map case ; TODO document
         ;; (and (= ::hfql/map (::hfql/value-type (hfql/metas current-point)))
         ;;   current-point)
         ;; Pull-many case
         (and (star-traversal? (hfql/metas current-point))
           (or (first (hfql/next-steps current-point))
             ;; Option `1.`
             (first (hfql/next-steps (hydrate-point current-point (right '(nil))))))) ; should not run IO – not supposed to.
         ;; Traversal case, eventually recursive.
         (and (= ::ana/traversal (::ana/type (hfql/metas current-point)))
           (or (and (not (hfql/multiplicity-many? current-point))
                 (find-selectable-point current-point (first (hfql/next-steps current-point))))
             ;; The fact that a traversal is itself tagged as multiplicity many conflates it with the pull-many case.
             ;; It forces us to reproduce the `1.` logic for traversals.
             ;; Maybe traversal's implementation should change and maybe we can separate traversing from fan-out.
             (and (hfql/multiplicity-many? current-point)
               (find-selectable-point current-point
                 (or ;; option `1.`
                   (first (hfql/next-steps current-point))
                   ;; FIXME Technical – call to next-steps will run the edge function, eventually causing IO!
                   ;; Caching and laziness won't apply because we are altering the scope. Userland function WILL rerun.
                   ;; There may be a way to bypass it. It may require modifying `hyperfiddle.hfql2.coalgebra/unfoldT`
                   ;; E.g. using a sentinel value. Such that we get a dummy child.
                   (first (hfql/next-steps (hfql/update-scope current-point assoc ::coalgebra/BYPASS-USER-IO-TO-GET-AN-UNSEEDED-COFREE-CHILD-IN-EMPTY-MULTIPLICITY-MANY-CASE true))))))))
         ;; Value case
         nil
         ))
     ;; We detected a recursive structure. We went through it and are back to where we started.
     ;; there is no selection point to be found here.
     nil)))

(tests
  (def _object-case (hfql/hfql [:a] {:a 1}))
  (find-selectable-point _object-case)
  := _object-case

  (def _pull-many-case-non-empty (hfql/hfql {* [:a]} [{:a 1}]))
  (find-selectable-point _pull-many-case-non-empty)
  := (first (hfql/next-steps _pull-many-case-non-empty))

  (hfql/values (find-selectable-point _pull-many-case-non-empty))
  := '({:a 1} 1)

  (def _pull-many-case-empty (hfql/hfql {* [:a]} []))
  (->> (find-selectable-point _pull-many-case-empty)
    (hfql/seed {'% {:a 2}}) ; must be seeded, original collection was empty
    (hfql/values))
  := '({:a 2} 2)

  (def _traversal-case-direct-object (hfql/hfql {identity [:a]} {:a 1}))
  (find-selectable-point _traversal-case-direct-object)
  := (first (hfql/next-steps _traversal-case-direct-object))

  (def _traversal-case-direct-pull-many (hfql/hfql {identity {* [:a]}} {:a 1}))
  (find-selectable-point _traversal-case-direct-pull-many)
  := (first (hfql/next-steps _traversal-case-direct-pull-many))

  (def _traversal-case-direct-pull-many-empty (hfql/hfql {identity {* [:a]}} []))
  (->> (find-selectable-point _traversal-case-direct-pull-many-empty)
    (hfql/seed {'% {:a 2}}) ; must be seeded, original collection was empty
    (hfql/values))
  := '({:a 2} 2)

  (def _traversal-case-nested-object (hfql/hfql {identity {identity [:a]}} {:a 1}))
  (hfql/values _traversal-case-nested-object)
  := '({:a 1} {:a 1} {:a 1} 1)

  (find-selectable-point _traversal-case-nested-object)
  := (->> _traversal-case-nested-object hfql/next-steps first hfql/next-steps first)

  (hfql/values (find-selectable-point _traversal-case-nested-object))
  := '({:a 1} 1)

  (def _traversal-case-nested-pull-many (hfql/hfql {identity {identity {* [:a]}}} {:a 1}))
  (find-selectable-point _traversal-case-nested-pull-many)
  := (->> _traversal-case-nested-pull-many hfql/next-steps first hfql/next-steps first)

  (def _traversal-case-recursive-syntactically-finite (hfql/hfql {identity [.-a ...]} (->TestType 1 2)))
  (take 6 (hfql/values _traversal-case-recursive-syntactically-finite))
  := (list (->TestType 1 2) {'.-a 1} 1  (->TestType 1 2) {'.-a 1} 1   )
  ;;                                   ⬑ recur point                ⬑ recur point … ad infinitum

  (take 8 (hfql/values (find-selectable-point _traversal-case-recursive-syntactically-finite)))
  := (list {'.-a 1} 1  (->TestType 1 2) {'.-a 1} 1  (->TestType 1 2) {'.-a 1} 1 )
  ;;                  ⬑ recur point               ⬑ recur point               ⬑ recur point … ad infinitum
  ;; Note values are shifted compared to previous test case.

  (symbolic-selection-coordinates (find-selectable-point _traversal-case-recursive-syntactically-finite))
  := `[nil (->TestType 1 2)]

  (def _traversal-case-recursive-syntactically-infinite (hfql/hfql {identity ...} 1))
  (take 10 (hfql/values _traversal-case-recursive-syntactically-infinite))
  := '(1 1 1 1 1 1 1 1 1 1)

  (points-syntactically-equal?
    _traversal-case-recursive-syntactically-infinite
    (first (hfql/next-steps _traversal-case-recursive-syntactically-infinite)))
  := true

  (find-selectable-point _traversal-case-recursive-syntactically-infinite)
  := nil

  ;; advanced recursivity case, find selectable point THROUGH recursivity
  (def _recursive-case-select-upwards (hfql/hfql [{:a ...}] {:a {:a 1}}))
  (take 5 (hfql/values _recursive-case-select-upwards))
  := '({:a {:a 1}} {:a 1} {:a 1} 1 {:a nil})

  (def _starting-point (hfql/find-cell-by-symbolic-column :a _recursive-case-select-upwards))
  (take 4 (hfql/values _starting-point))
  := '({:a 1} {:a 1} 1 {:a nil})

  (points-syntactically-equal?
    (find-selectable-point _starting-point)
    _recursive-case-select-upwards)
  := true

  ;; value case
  (find-selectable-point (hfql/hfql identity 1))
  := nil

  )




;;;; Navigation from selection
;;;;;; Finding the next navigation point from the current point and an HFQL selection point

#_(defn find-next-point [current-point selected-point]
  (or
    ;; [✓] if a column is selected, selected-point is expected to already be a cellT
    ;; [✓] if a row is selected, selected-point is expected to already be a rowT
    ;; [✓] if selected-point can be a recursive point, it is expected for selected-point to already be the recursive point

    ;; [✓] if selected-point has ::navigator/select metas, selected-point is expected to already have been looked up and hydrated from sitemap
    ;;   Should we look for an ::navigator/select point? no. Selectability is not defined by `::navigator/select`. `::navigator/select` only advise how to interpret a selection action.
    ;; reshaping (expanding collection) is not a matter of navigation and is of no concern here
    ;; therefore this function should not exist
    )
  )

(declare sitemap-lookup normalize-navigator-select-command hydrate-navigator-select-command)

(defn find-next-navigable-point [sitemap selected-point]
  ;; - Selecting a selectable point may trigger a navigation.
  ;; - Navigation happens if the selected point either:
  ;;   - is tagged with `::navigator/select`
  ;;   - value-at-point is Identifiable
  ;;   - value-at-point is not Identifiable by a column is  FIXME
  ;; - If navigation happens, it may or may not happen on the select point itself.
  ;;   - Navigation happens on the selected point itself:
  ;;     - if all the followings are true:
  ;;       - the selected point's value-at-point is Identifiable
  ;;       - the selected point is NOT tagged with `::navigator/select`
  ;;     - and furthermore:
  ;;       - navigation *always* happen on a recursion point
  ;;         - even if recursion limit has been reached!
  ;;         - if the recursion limit has been reached, the navigator is expected
  ;;           to render the value-at-point as an anonymous value. I.e. after
  ;;           recursion limit has been reached the navigator will not account for
  ;;           the HFQL expression in rendering the last (leaf) value.
  ;;         - This behavior is essential to allow cell selection at the
  ;;           recursion's end.
  ;;         - This behavior won't cause an infinite recursion as long as the
  ;;           Navigator ignores the HFQL expression when rendering the leaf
  ;;           value.
  ;;   - Navigation *does not* happen on the selected point itself:
  ;;     - if the selected point is tagged with `::navigator/select`. In which
  ;;       case the value associated with `::navigator/select` will be
  ;;       interpreted to resolve an other HFQL point.
  ;;       - The resolved HFQL point, if found, will be seeded with the selected
  ;;         point's navigated value (not its value-at-point).
  (or
    (and selected-point
      (let [navigator-select-command (normalize-navigator-select-command
                                       (or (::navigator/select (hfql/metas selected-point))
                                         (::hfql/select (hfql/metas selected-point)) ; TODO legacy, remove
                                         ))]
        (or
          ;; Navigation happens on the selected point:
          (and #_(hfql/identifiable? (from-right nil (hfql/value-at-point selected-point))) ; not all points are Identifiable (e.g. maps) but can be navigated anyway using `hfql/current-point`'s DI.
            (not (some? navigator-select-command))
            selected-point)
          ;; Navigation doesn't happen on the selected point
          (and (some? navigator-select-command)
            (sitemap-lookup sitemap
              (hydrate-navigator-select-command navigator-select-command selected-point) ; may run IO – cached
              ))
          ;; TODO what about finding the recursive child? is it already handled by `find-selectable-point`?
          )))
    nil))


;;;;; Resolving ::navigator/select form sitemap

(defn- navigator-select-command? [navigator-select-command]
  ;; Only difference with `symbolic-constructor-form?` is this one allows unqualified symbols. Why are they so close?
  {:post [(boolean? %)]}
  (and (seq? navigator-select-command)
    (symbol? (first navigator-select-command))))

(defn- sitemap-lookup [sitemap [sitemap-entry value-at-point :as navigator-select-command]]
  {:pre [(associative? sitemap)
         (navigator-select-command? navigator-select-command)] ; is this the right predicate? If so this is interesting.
   :post [(or (nil? %) (hfql? %))]}
  (let [point (get sitemap sitemap-entry)]
    (or (and (some? point)
          (hydrate-point point value-at-point)) ; note `value-at-point` doesn't serialize through the URL, no need to resolve.
      nil)))

(defn normalize-navigator-select-command [navigator-select-command]
  (or
    ;; already normalized
    (and (navigator-select-command? navigator-select-command)
      navigator-select-command)
    (and ;; We pick `ident?` over symbol? or even qualified-symbol? because as of today nothing forbids users to name their sitemap entries with idents. We may restrict it later.
      (ident? navigator-select-command)
      (list navigator-select-command '%))
    ;; invalid command
    (and (some? navigator-select-command)
      (println "Invalid `select` expression:" navigator-select-command " . A `select` expression must be a keyword, a symbol, or a symbolic expression as in `(page arg0 % arg2) where args and % are optional.")
      nil)
    ;; no command
    (and (nil? navigator-select-command)
      nil)))

(defn- hydrate-navigator-select-command [navigator-select-command selected-point]
  {:pre [(navigator-select-command? navigator-select-command)]}
  (let [[sitemap-symbolic-entry & args] navigator-select-command]
    (cons sitemap-symbolic-entry (map (fn [symbolic-arg]
                                        (case symbolic-arg
                                          %  (hfql/value-at-point selected-point)
                                          %v (hfql/nav* selected-point)
                                          symbolic-arg))
                                   args))))

;;;; Dectecting selection state in UI
;;;;; Detecting positional indexing

(defn- positional-symbolic-constructor-form? [value]
  {:post [(boolean? %)]}
  (and (symbolic-constructor-form? value)
    (= `hfql/nth (first value))
    (nat-int? (second value))))

(defn- positional-symbolic-selection-coordinates? [pair]
  {:post [(boolean? %)]}
  (and (symbolic-selection-coordinates? pair)
    (let [[_symbolic-column symbolic-constructor-form] pair]
      (positional-symbolic-constructor-form? symbolic-constructor-form))))

;;;;; Extracting positional index out of symbolic coordinates

(defn- selection-positional-index [selection-coordinates]
  {:pre [(symbolic-selection-coordinates? selection-coordinates)]
   :post [(or (nil? %) (nat-int? %))]}
  (or (and (positional-symbolic-selection-coordinates? selection-coordinates)
        (let [[_symbolic-column symbolic-constructor-form] selection-coordinates
              [_symbol-named-nth index] symbolic-constructor-form]
          index))
    nil))

;;;;; Detecting a selected row

(defn row-selected?
  ([selected-point current-point] (row-selected? selected-point current-point nil))
  ([selected-point current-point selected-point-symbolic]
   {:pre [(or (nil? selected-point) (selection-point? selected-point))
          (or (nil? current-point) (selection-point? current-point))
          (or (nil? selected-point-symbolic) (symbolic-selection-coordinates? selected-point-symbolic))]
    :post [(boolean? %)]}
   (or (and (some? selected-point)
         (some? current-point)
         (selection-point? selected-point)
         (selection-point? current-point)
         (row? selected-point)
         (or (= selected-point current-point) ; Ideal path FIXME G — I don't remember why but this is always false for now.
           ;; If we have symbolic coordinates, compare them with the current point directly.
           ;; This is not just a shortcut, the selected point can fail to resolve (see `resolve-selection-coordinates`).
           (and (some? selected-point-symbolic)
             (nil? (first selected-point-symbolic)) ; not a cell selection
             (hfql/identifiable? (from-right nil (hfql/value-at-point current-point)))
             (= (second selected-point-symbolic)
               (hfql/identify (from-right nil (hfql/value-at-point current-point)))))
           ;; We don't have symbolic selection coordinates, or they don't match, but we have a selected object.
           ;; This can happen when the selected object isn't globally identifiable (e.g. positional indexing)
           (and
             (hfql/identifiable? (from-right nil (hfql/value-at-point selected-point)))
             (hfql/identifiable? (from-right nil (hfql/value-at-point current-point)))
             (= (hfql/identify (from-right nil (hfql/value-at-point selected-point)))
               (hfql/identify (from-right nil (hfql/value-at-point current-point)))))
           (and (some? (::hfql/positional-index (hfql/metas selected-point)))
             (some? (::hfql/positional-index (hfql/metas current-point)))
             (= (::hfql/positional-index (hfql/metas selected-point))
               (::hfql/positional-index (hfql/metas current-point))))))
     false))
  )

;;;;; Detecting a selected cell

(defn cell-selectable? [selected-point]
  {:pre [(or (nil? selected-point) (selection-point? selected-point))]
   :post [(boolean? %)]}
  (and (some? selected-point)
    (selection-point? selected-point)
    (cell? selected-point)
    (symbolic-column? (symbolize-form selected-point))))

(defn cell-selected?
  ([selected-point current-point] (cell-selected? selected-point current-point nil))
  ([selected-point current-point selected-point-symbolic]
   {:pre [(or (nil? selected-point) (selection-point? selected-point))
          (or (nil? current-point) (selection-point? current-point))
          (or (nil? selected-point-symbolic) (symbolic-selection-coordinates? selected-point-symbolic))]
    :post [(boolean? %)]}
   (or (and (some? selected-point)
         (some? current-point)
         (selection-point? selected-point)
         (selection-point? current-point)
         (or
           ;; Base case
           ;; (cell? selected-point) ;; TODO why was this necessary? Is it still necessary? Removing it seem harmless for now.
           (= selected-point current-point) ; Ideal path FIXME G — I don't remember why but this is always false for now.
           ;;; First, check with symbolic coordinates
           ;; Case where the current point is not identifiable.
           (and (= (first selected-point-symbolic) (hfql/symbolic-edge current-point)) ; column matches
             (= `(hfql/current-point) (second selected-point-symbolic))
             (or
               ;; this is redundant with the `hfql/current-point` check above, but we want to ensure they always line up.
               (not (hfql/identifiable? (from-right nil (hfql/value-at-point current-point))))
               (and (println "Invariant violation: `hfql/current-point` is selected but the current point is Identifiable. Selection should use the object's symbolic identity instead.")
                 nil)))
           ;; Case where the current point is identifiable
           (and
             (= (first selected-point-symbolic) (hfql/symbolic-edge current-point)) ; column matches
             (hfql/identifiable? (from-right nil (hfql/value-at-point current-point))) ; current point is identifiable
             (= (second selected-point-symbolic) ; current row's identity matches the symbolic constructor
               (hfql/identify (from-right nil (hfql/value-at-point current-point)))))
           ;; Then, check with resolved selected point
           (and
             (= (hfql/symbolic-edge selected-point) (hfql/symbolic-edge current-point)) ; columns matches
             (let [selected-identity (hfql/identify (from-right nil (hfql/value-at-point selected-point)))
                   current-identity (hfql/identify (from-right nil (hfql/value-at-point current-point)))]
               (or
                 ;; Case where both are identifiable – this cell is selected if their identities match.
                 (and (some? selected-identity)
                   (some? current-identity)
                   (= selected-identity current-identity))
                 ;; Case where the current point isn't identifiable but we can match by positional index.
                 (and
                   ;; (some? selected-identity) ; TODO is this check necessary? why?
                   (some? (some-> selected-point hfql/parent hfql/metas ::hfql/positional-index))
                   (some? (some-> current-point hfql/parent hfql/metas ::hfql/positional-index))
                   (= (some-> selected-point hfql/parent hfql/metas ::hfql/positional-index)
                     (some-> current-point hfql/parent hfql/metas ::hfql/positional-index))))))
           ;; 2026-01-08 Edge case: traversal with a pull continuation inside of a pull.
           ;; See comments for `step-over-eventual_traversal-with-pull-continuation_in-pull`.
           ;; In this case the selected-point is one extra step ahead of current-point. So we want to match against its parent – the actual cell.
           ;; This behavior is not a band-aid. It is specific to a UI-based navigator over a tree-seqed HFQL structure. It is a UX concern.
           ;; REPLs – for instance – are not impacted by this edge case.
           (and
             (traversal-with-pull-continuation_in-pull? current-point)
             (cell-selected? (hfql/parent selected-point) current-point))))
     false)))

;;;; Naming points
;;;;; Infering a title from navigation point

(defn point-symbolic-name
  ([current-point] (point-symbolic-name current-point nil))
  ([current-point parent-selection-coordinates]
   {:pre [(or (nil? current-point) (hfql? current-point))
          (or (nil? parent-selection-coordinates) (symbolic-selection-coordinates? parent-selection-coordinates))]
    :post [(string? %)]}
   (or (and (some? current-point)
         (or (let [symbolic-column (symbolize-form current-point)
                   symbolic-constructor-form (symbolize-value current-point)]
               (or
                 ;; Case where both symbolic column and value are known at the current point.
                 (and (some? symbolic-column)
                     (some? symbolic-constructor-form)
                     (pr-str (list symbolic-column (symbolize-value current-point))))
                 ;; Case where only the column is known at the current point and the value isn't Identifiable.
                 (and (some? symbolic-column)
                   (nil? symbolic-constructor-form)
                   (pr-str symbolic-column))
                 ;; Case where no column is selected but the selected value is Identifiable
                 ;; We want to infer "from which column" this point was selected.
                 ;; So we look at the parent, if any, to infer selection in the context of the parent.
                 (and (nil? symbolic-column)
                   (some? symbolic-constructor-form)
                   (let [parent-edge (and (hfql/parent current-point) ; there's a parent.
                                       (symbolize-form (hfql/parent current-point)))
                         parent-symbolic-value (and (hfql/parent current-point)
                                                 (symbolize-value (hfql/parent current-point)))]
                     (or
                       ;; Edge case: both parent and the current selected *cell* are Identifiable
                       ;; We want to favor `(column parent-row)` over just `cell`'s symbolic identity
                       ;; More so, we want to prevent the incorrect `(column cell)` symbolic form.
                       ;; Real-world case: jgit's GitRepository/.getRepository return a FileRepository. Both are Identifiable.
                       ;; As of 2026-01-06 the UI produced `(.getRepository FileRepository)` which is wrong.
                       ;; The correct form is `(.getRepository GitRepository)`.
                       (and (some? parent-edge)
                         (some? parent-symbolic-value)
                         (pr-str (list parent-edge parent-symbolic-value)))
                       ;; Edge case: the parent is a cell selection on a non-identifiable value AND the selected cell is identifiable.
                       ;; We know the parent's column but not the parent's row. We also know the current point's symbolic value.
                       ;; Real-world case: selecting `type` on an Exception instance:
                       ;; - `type` is the column
                       ;; - Exceptions are not Identifiable
                       ;; - `type` points to a Class object, which are identifiable – by canonical name.
                       ;; Which one should we favor?
                       ;; A. `type` – local, path based, relative.
                       ;; B. `(Class/forName <canonicalName>)` – global, absolute.
                       ;; We decide we prefer `A.` over `B.`
                       (and (some? parent-edge)
                         (nil? parent-symbolic-value)
                         (some? parent-selection-coordinates)
                         (let [[symbolic-column symbolic-constructor-form] parent-selection-coordinates]
                           (or
                             ;; Case where we select "hfql/current-point", a non-identifiable object only addressable by the current scope.
                             (and (= `hfql/current-point (first symbolic-constructor-form))
                               (some? symbolic-column)
                               (pr-str symbolic-column))
                             ;; Below is duplicated – Ⓐ – TODO cleanup
                             ;; Case where we select a row by positional index.
                             (and (= `hfql/nth (first symbolic-constructor-form)) ; This is a positional addressing situation.
                                 (nil? symbolic-column)
                                 (pr-str (list 'nth '% (second symbolic-constructor-form))))
                             ;; Case where we select a cell by positional index.
                             (and (= `hfql/nth (first symbolic-constructor-form))
                               (some? symbolic-column)
                               (pr-str (list symbolic-column (list 'nth '% (second symbolic-constructor-form)))))))))))
                 ;; Case where the current point is anonymous (e.g. a non-identifiable row).
                 ;; So there's no selected column, and the row coordinate is unknown.
                 ;; UNLESS it can be identified by positional index.
                 (and (nil? symbolic-column)
                   (nil? symbolic-constructor-form)
                   (some? parent-selection-coordinates) ; Anonymous point but we may have more info by interpreting symbolic coordinates.
                   ;; Below is duplicated – Ⓐ – TODO cleanup
                   (let [[symbolic-column symbolic-constructor-form] parent-selection-coordinates]
                     (or
                       ;; Case where we select a row by positional index.
                       (and (= `hfql/nth (first symbolic-constructor-form)) ; This is a positional addressing situation.
                           (nil? symbolic-column)
                           (pr-str (list 'nth '% (second symbolic-constructor-form))))
                       ;; Case where we select a cell by positional index.
                       (and (= `hfql/nth (first symbolic-constructor-form))
                         (some? symbolic-column)
                         (pr-str (list symbolic-column (list 'nth '% (second symbolic-constructor-form))))))))
                 ;; Case where we render a navigation (selection) error. E.g. when resolving fails.
                 ;; The title is the one of the expected navigation, not the one of the error object.
                 (and (::hfql/error (hfql/metas current-point)) ; error flag
                   (or (and (some? parent-selection-coordinates)
                         (or (and (some? (first parent-selection-coordinates)) ; col × row
                               (pr-str (list* parent-selection-coordinates)))
                           (pr-str (second parent-selection-coordinates)))) ; row only
                     ;; We don't know the current point's corrdinates because there's no parent. (e.g. top-level navigation failed)
                     ;; The best title we can produce is the one of the error itself.
                     ;; It's better than nothing.
                     (str (type (::hfql/error (hfql/metas current-point))))))))
           ;; Case where we have:
           ;; - no error
           ;; - no selected column
           ;; - couldn't identify selection in parent's context (relative, path based)
           ;;   - can't tell if selection was by positional index
           ;; We attempt to generate a title from the current point's value.
           (or
             (some-> (symbolize-value current-point) pr-str)
             (some-> (hfql/symbolic-edge-computed current-point) pr-str))
           ;; Fallback case: we can't generate a title for the current point. It means:
           ;; - no error
           ;; - no selected column
           ;; - couldn't identify selection in parent's context (relative, path based)
           ;;   - can't tell if selection was by positional index
           ;; - value at point isn't Identifiable.
           ;;   - can't generate a title from current point.
           ;; We assume this is a cell selection and we retry with the parent.
           (point-symbolic-name (hfql/parent current-point))))
     "Anonymous value")))


(comment
  (point-symbolic-name _current-point (pair :kw '(hyperfiddle.hfql2/nth 3)))
  )

;;;; Reshaping values
;;;;;; Detecting and rendering anonymous collections as tables
;;;;;; Rendering final multiplicity many recursion step as an anonymous collection
;;;;;; Rendering the multiplicity-many part of a traversal

(defn find-collection-point [point]
  (case (::ana/type (hfql/metas point))
    ::ana/traversal (or (and (hfql/multiplicity-many? point)
                          point)
                      (find-collection-point (first (hfql/next-steps point))))
    point))

;;;;;; Auto-selecting the `val` slot when selecting a MapEntry TODO
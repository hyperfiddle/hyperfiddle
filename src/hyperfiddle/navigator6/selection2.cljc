(ns hyperfiddle.navigator6.selection2
  "G: Logic in this namespace is written in an uncommon form (boolean algebra) because:
  - selection, selection resolution, and navigation onto selected points have MANY edge cases.
  - I want truth tables to be covered. I want an exhaustive listing of all possible conditional branches.
    Because this code will be called from HFQL Navigator (Electric) and THERE WILL be glitches and inconsistent states.
    I want to be able to detect them quickly and track them easily.
  - I want all branches to be explicit. E.g. no implicit nil, no implicit fallback branch.
    Implicit conditional branches cause Swiss Cheese bugs. I've been bitten too many times.
    Covering truth tables implies introducing redundancy and 'obvious' dead branches sooner or later.
    I don't consider this a problem. I'm not lazy and it communicates better. Trivial cases are never trivial.
    Uttering \"trivial\" is (not exhaustively) either symptomatic of Dunning–Kruger effect, a lack of understanding how the human brain works, or a lack of social skills.
  - Asserting for state consistency is not enough:
      - It may be disabled at any point from configuration (e.g. prod)
      - As of today, throwing can cause Electric to crash without reporting the exception.
      - To me, in this case, asserting is a communication tool, a dev-time convenience and all tests should pass with assertions disabled.
  "
  (:require
   [hyperfiddle.ct.pair :refer [pair? pair fst snd]]
   [hyperfiddle.hfql2 :as hfql :refer [hfql?]]
   [hyperfiddle.hfql2.protocols :refer [Identifiable -hfql-resolve]]
   [hyperfiddle.hfql2.analyzer :as ana]
   [hyperfiddle.hfql2.coalgebra :refer [star-traversal?]]
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
  := false
  (symbolic-column? 42)
  := false
  (symbolic-column? [:db/ident :artist/name])
  := false

  )

(defn- symbolic-selection-coordinates? [pair]
  ;; [column row]
  ;; Why column × row and not row × column? It's arbitrary.
  ;; I picked this order to mimick Excel formula, e.g. `A2`.
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

(def ^:private selection-point? hfql?) ; all HFQL points are potential selection points – not sure if this is true. TODO refine.

(defn- row? [point] (= ::ana/pull (::ana/type (hfql/metas point)))) ; A row is a pull. Children of a row are cells.
(defn- cell? [point] (row? (::hfql/parent (hfql/metas point)))) ; A cell is a direct child of a row.

(defn- symbolize-form [point]
  (or (and (row? point) ; row is a pull and pull isn't an edge. A pull has not name on its own.
        nil)
    (and (cell? point) ; a cell cannot be a pull, so it must have a name. E.g. keyword or traversal.
      (hfql/symbolic-edge point))
    nil))

(defn- symbolize-value [point]
  (hfql/identify
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
      nil)))

(defn symbolic-selection-coordinates [point]
  {:pre [(selection-point? point)]
   :post [(symbolic-selection-coordinates? %)]}
  (or
    (and (selection-point? point) ; selected point is an HFQL point
      (or (and (row? point) ; selected point is a row
            (pair nil (symbolize-value point)))
        (and (cell? point) ; selected point is a cell
          (pair (symbolize-form point) (symbolize-value point)))
        (and
          (println "selected point is a selectable HFQL point but is neither a row nor a cell" (type point) point)
          (pair nil nil))))
    (do (println "selected object is not an HFQL point and cannot be selected" (type point) point)
        (pair nil nil))))

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
   :post [(or (nil? %) (hfql/identifiable? %))]}
  (or (and (some? symbolic-constructor-form)
        (hfql/hfql-resolve symbolic-constructor-form))
    nil))

(tests
  (resolve-identifiable-object nil)
  := nil
  (resolve-identifiable-object (hfql/identify (->TestType 1 2)))
  := (->TestType 1 2)

  )

;;;;;; Turning symbolic selection coordinates into an HFQL selection point
(defn resolve-selection-coordinates [selection-point symbolic-selection-coordinates]
  {:pre [(selection-point? selection-point)
         (or (nil? symbolic-selection-coordinates) (symbolic-selection-coordinates? symbolic-selection-coordinates))]
   :post [(or (nil? %) (selection-point? %))]}
  (when-not (row? selection-point)
    (println `resolve-selection-coordinates "WARNING: the received selection point is not a row."))
  (or
    ;; selection coordinates are valid – we should be able to resolve an object
    (and (symbolic-selection-coordinates? symbolic-selection-coordinates)
        (let [[symbolic-column symbolic-constructor-form] symbolic-selection-coordinates
              identifiable-object (resolve-identifiable-object symbolic-constructor-form)]
          (or (and (some? identifiable-object) ; we've found an identifiable object – we should be able to produce a row or a cell
                (let [seeded-selection-point (hfql/update-scope selection-point assoc '% identifiable-object)]
                  (or (and (some? symbolic-column)
                        (or (and ; there's a symbolic column – this is a cell selection – we should be able to hydrate the HFQL row and focus it on the corresponding cell.
                              (row? seeded-selection-point)
                              (or ; perf optimization – attempt to resolve without suggesting.
                                (hfql/find-cell-by-symbolic-column symbolic-column seeded-selection-point)
                                (hfql/find-cell-by-symbolic-column symbolic-column (hfql/augment-with-suggestions! seeded-selection-point))))
                          (and ; we are resolving a cell selection but the given selection point is not a row. We can't focus on a cell.
                            (not (row? seeded-selection-point))
                            (println "The given selection contains a column and so targets a cell. But the current selection point is not a row. Can't focus on a cell on an object that is not a row." (::ana/type (hfql/metas seeded-selection-point)))
                            (def _selection-point seeded-selection-point) ; G – for me, for debugging.
                            nil)))
                    ;; there's no symbolic column to focus on. This is a row selection.
                    seeded-selection-point)))
            nil)))
    ;; no valid selection coordinates
    nil))

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

;;;;;; Finding the selectable point at the current point (current step)

(defn find-selectable-point
  ;; Selection may happen on a row or a cell. Cell selection always happen on a given row.
  ;; It is not possible to select a cell without knowing its row.
  ;; Therefore a selectable point is always a row.
  ;; Cell selection is formed by a row selection + the cell symbolic identifier. See `symbolic-selection-coordinates`.
  ;;
  ;; Therefore the current point must either:
  ;; - be a pull (a row), in which case the selectable point is the current point — Object case (transposed row).
  ;; - be a collection of pulls (a star traversal, aka a collection of rows), in which case any row is selectable, an so we define
  ;;   the first row as the selectable point – This is the pull-many case.
  ;;   - Problem: what if the multiplicity is many but there are no rows available because the collection is empty? E.g. due to search.
  ;;     This is an ergonomic edge case of Cofree Comonads: where the Cofree tree is potentially infinite, is structurally known,
  ;;     but ends with no next value. How do we know the structure of the supposedly next value, in its absence?
  ;;     In that case we should be able to:
  ;;       a. Inject a dummy, stub value and ask for the Cofree children again, so to get one dummy (but structurally correct) child.
  ;;         Laziness makes this safe.
  ;;       b. re-unfold the static child structure from the captured AST into a fresh, unseeded Cofree point.
  ;;         Then we re-parent it to the current point and seed it accordingly.
  ;;     To me a. looks like a hack and b. looks like the idiomatic approach. But thinking about it, I'm tempted to think it is actually
  ;;     the opposite. `a.` is self-contained and will always be structurally correct. Even if the API may look odd.
  ;;     `b.` on the other hand implies grafting a Cofree tree onto an other. This can be brittle and hard to maintain.
  ;; - a traversal, in which case the traversal's continuation must be a row or a collection of rows
  ;;   If the traversal's continuation is itself a traversal (e.g. {identity {identity []}}), then the continuation's continuation must
  ;;   be a row or a collection of rows, recursively.
  ;;   The first traversal continuation found to be row will be the selectable point.
  ;;   If the continuation is found to be a collection of rows, then the rules of finding the selectable point for a collection
  ;;   of rows applies.
  ;;   Edge case: what about the recursive `{f ...}` form.
  ;;   In that case we never find a selectable point, because we defined a selectable point as a row. And this expression doesn't define any row.
  ;;   This is a UI constraint. We defined a selectable point as a row because we render grids.
  ;;   I believe it is possible to render a UI for `{f ...}`, though it doesn't strike me as obvious.
  ;; - a value point. Any point that is neither a traversal nor a pull. Such point is not selectable.
  ([current-point]
   (find-selectable-point ::unset current-point))
  ([starting-point current-point] ; to guard against infinite recursion
   {:pre [(hfql? current-point)]
    :post [(or (nil? %) (selection-point? %))]}
   (or
     (and (not= current-point starting-point)
       (or
         ;; Object case
         (and (row? current-point)
           current-point)
         ;; Pull-many case
         (and (star-traversal? (hfql/metas current-point))
           (or (first (hfql/next-steps current-point))
             ;; Option `a.`
             (first (hfql/next-steps (hfql/update-scope current-point assoc '% '(nil))))))
         ;; Traversal case, eventually recursive.
         (and (= ::ana/traversal (::ana/type (hfql/metas current-point)))
           (or (and (not (hfql/multiplicity-many? current-point))
                 (find-selectable-point starting-point (first (hfql/next-steps current-point))))
             ;; The fact that a traversal is itself tagged as multiplicity many conflates it with the pull-many case.
             ;; It forces us to reproduce the `a.` logic for traversals.
             ;; Maybe traversal's implementation should change and maybe we can separate traversing from fan-out.
             (and (hfql/multiplicity-many? current-point)
               (find-selectable-point starting-point
                 (or ;; option `a.`
                   (first (hfql/next-steps current-point))
                   ;; FIXME Technical – call to next-steps will run the edge function, eventually causing IO!
                   ;; Caching and laziness won't apply because we are altering the scope. Userland function WILL rerun.
                   ;; There may be a way to bypass it. It may require modifying `hyperfiddle.hfql2.coalgebra/unfoldT`
                   ;; E.g. using a sentinel value. Such that we get a dummy child.
                   (first (hfql/next-steps (hfql/update-scope current-point assoc '% '(nil)))))))))
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
  (find-selectable-point _traversal-case-direct-pull-many-empty)

  ;; TODO test nested traversal case
  ;; TODO test recursive traversal case – no infinite loop
  ;; TODO test recursive traversal case – infinite loop
  ;; TODO test value case

  )




;;;; Navigation from selection
;;;;;; Finding the next navigation step from the current step and an HFQL selection point

(defn find-next-point [current-point selected-point]
  (or
    ;; [✓] if a column is selected, selected-point is expected to already be a cellT
    ;; [✓] if a row is selected, selected-point is expected to already be a rowT

    ;; [ ] if selected-point as ::hfql/select metas, selected-point is expected to already have been looked up and hydrated from sitemap
    ;; [ ] if selected-point can be a recursive point, it is expected for selected-point to already be the recursive point
    ;; reshaping (expanding collection) is not a matter of navigation and is of no concern here
    ;; therefore this function should not exist
    )
  )

;;;;; Resolving ::hfql/select form sitemap

;;;; Reshaping values
;;;;;; Detecting and rendering anonymous collections as tables
;;;;;; Rendering final multiplicity many recursion step as an anonymous collection
;;;;;; Auto-selecting the `val` slot when selecting a MapEntry
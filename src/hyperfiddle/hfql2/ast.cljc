(ns hyperfiddle.hfql2.ast
  "HFQL AST node API. Specs, constructors, accessors, predicates, and mutators
  for AST nodes. Consumers should access nodes through this API rather than raw
  keyword lookup, so that the map representation remains an implementation detail."
  (:require [clojure.spec.alpha :as s]
            [hyperfiddle.rcf :refer [tests]]))

;;;; Specs -- keys

(s/def ::type #{::star ::recur ::literal ::keyword ::string ::symbol
                ::var ::quoted-var ::pull ::fn ::lambda ::call ::traversal})
(s/def ::form any?)            ; the original source form as read by the analyzer
(s/def ::lexical-env (s/map-of symbol? any?)) ; symbol->value bindings captured at analysis time
(s/def ::partial boolean?)     ; true while the node still expects arguments (e.g. `%`)
(s/def ::id int?)              ; content-derived hash, see `compute-node-id`
(s/def ::symbolic-edge any?)   ; the edge label for this node in its parent pull (keyword, symbol, string, ...)
(s/def ::symbolic-edges (s/coll-of any?)) ; ordered edge labels for children of a pull node
(s/def ::form-resolved any?)   ; alias-resolved form. May be a qualified keyword (::foo), qualified
                               ; symbol (clojure.core/inc), or a full call form ((my.ns/fn arg1 %)).
                               ; The type depends on the node -- this spec is intentionally open.
(s/def ::form-short any?)      ; shortest unambiguous form for display
(s/def ::resolved-runtime-value any?) ; the value obtained by resolving this node's form (e.g. the function
                                      ; object for a symbol, the var for a var ref). Only present on nodes where
                                      ; resolution produces something distinct from the form itself.
(s/def ::var (s/nilable var?))  ; the Clojure var backing this node, if any
(s/def ::parameters sequential?) ; symbolic parameter list (symbols like ['db '%])
(s/def ::arguments sequential?)  ; runtime argument values provided at a call site
(s/def ::pull-has-star? boolean?) ; whether a pull expression contains the `*` wildcard. Pull nodes only.
(s/def ::recursion-point boolean?) ; true when this node is the Target of a `...` recursion
(s/def ::recursion-limit (s/or :bounded nat-int? :unbounded #{##Inf})) ; remaining recursion depth
(s/def ::multiplicity #{::one ::many}) ; cardinality of the node's result: ::one for single values, ::many for collections
(s/def ::kind #{::star ::keyword-fn}) ; syntactic origin of this node. Refines ::type: where ::type says WHAT a node
                                      ; is structurally (e.g. ::traversal, ::lambda), ::kind says HOW it was generated.
                                      ; ::star (traversals only): from `{* ...}` -- "for all children". The value at
                                      ;   point is expected to be a collection; the continuation applies to each element.
                                      ; ::keyword-fn (lambdas only): from `{:k [...]}` keyword-as-function sugar.
                                      ; Matters for the interpreter which handles star traversals differently.
(s/def ::edge (s/and map? #(s/valid? ::node %))) ; for traversals: the inner AST node (itself a full node map) that describes the
                               ; edge function. E.g. in `{:user/name [...]}`, the traversal's edge is the
                               ; ::keyword node for `:user/name`. In `{(all-users) [...]}`, the edge is a
                               ; ::call node for `(all-users)`.
(s/def ::leaf boolean?)        ; whether a traversal is terminal (no children to unfold)
;; Interpreter-set keys (not part of the static node spec produced by phi)
(s/def ::recursion-root boolean?)  ; marks the root of a recursive structure at runtime
(s/def ::recursive-child boolean?) ; marks a child that was produced by recursive unfolding

;;;; Specs -- node multi-spec
;;
;; Uses `s/multi-spec` dispatched on ::type rather than `s/or` because multi-spec
;; is open (new node types = new defmethod), gives named variants in explain paths,
;; and conforms without tagging the output.
;;
;; Each defmethod declares the required and optional keys for that node type.
;; Use `(s/assert ::node m)` during development to catch malformed nodes early;
;; `s/*compile-asserts*` elides the checks in production.

(defmulti node-spec
  "Return the spec for an AST node map, dispatched on ::type."
  ::type)

(defmethod node-spec ::star [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id]))

(defmethod node-spec ::recur [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id ::recursion-limit]))

(defmethod node-spec ::literal [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id]))

(defmethod node-spec ::keyword [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id ::form-short]))

(defmethod node-spec ::string [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id]))

(defmethod node-spec ::symbol [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id
                ::resolved-runtime-value ::var ::form-resolved ::form-short]))

(defmethod node-spec ::var [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id
                ::form-resolved ::form-short]))

(defmethod node-spec ::quoted-var [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id
                ::symbolic-edge ::form-resolved ::form-short]))

(defmethod node-spec ::pull [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id
                ::pull-has-star?]
          :opt [::recursion-point ::recursion-limit]))

(defmethod node-spec ::fn [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id]))

(defmethod node-spec ::lambda [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id
                ::symbolic-edge ::parameters]
          :opt [::kind ::form-resolved ::form-short]))

(defmethod node-spec ::call [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id
                ::resolved-runtime-value ::var ::parameters ::arguments ::form-resolved]
          :opt [::lexical-env]))

(defmethod node-spec ::traversal [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id ::multiplicity]
          :opt [::kind ::edge ::recursion-point ::recursion-limit ::leaf]))

(s/def ::node (s/multi-spec node-spec ::type))

;;;; Internal helpers

(defn compute-node-id
  "Derive a stable identity hash from a node's semantic content.
  Two nodes with the same type, form, and display forms will share the same id.
  Used to compare structural equality across independently constructed trees --
  e.g. two cells representing the same column in the same pull will have the
  same id even if they belong to different rows. `::symbolic-edge` is included
  because quoted-var and lambda have a distinct edge label from their form."
  [node]
  (hash (select-keys node #{::type ::form ::symbolic-edge ::form-short ::form-resolved})))

(def ^:private node-defaults
  "Default values merged with lowest precedence by `finalize`. Any key
  explicitly set by a constructor (or via form-meta) takes priority."
  {::lexical-env {}
   ::partial     true})

(defn- finalize
  "Merge defaults, compute ::id, and validate. Called by every constructor.
  `node-defaults` is merged underneath so explicit values always win."
  [node]
  (s/assert ::node (assoc (merge node-defaults node) ::id (compute-node-id node))))

;;;; Constructors
;;
;; Each constructor takes `form-meta` (metadata from the original source form,
;; may be nil) as first argument, followed by required fields for that node type.
;; `form-meta` is merged first, then AST keys are assoc'd on top -- matching how
;; the phi algebra works. The resulting map is finalized (defaults + id + spec
;; check).

(defn ->star
  "Construct a star (`*`) wildcard node. Signals \"include all suggested edges\"
  inside a pull expression. Form is always `'*` and lexical-env is empty."
  [form-meta]
  (finalize (merge form-meta {::type ::star, ::form '*})))

(defn- ->recur [form-meta recursion-limit]
  (finalize (merge form-meta {::type ::recur, ::form (if (= ##Inf recursion-limit) '... recursion-limit)
                              ::recursion-limit recursion-limit})))

(defn ->bounded-recursion
  "Construct a bounded recur node (`... 5`). `limit` is the maximum recursion
  depth (a nat-int)."
  [form-meta limit]
  (->recur form-meta limit))

(defn ->unbounded-recursion
  "Construct an unbounded recur node (`...`). Recurses until the data is
  exhausted."
  [form-meta]
  (->recur form-meta ##Inf))

(defn ->literal
  "Construct a literal value node (number, boolean, nil, etc.)."
  [form-meta form lexical-env]
  (finalize (merge form-meta {::type ::literal, ::form form, ::lexical-env lexical-env})))

(defn ->keyword
  "Construct a keyword node. Keywords are the most common edge type in pull
  expressions, e.g. `:user/name` in `[:user/name :user/email]`. Keywords
  auto-resolve at read time, so `form-resolved` and `symbolic-edge` are always
  the form itself -- accessors derive them. Only `form-short` (display alias)
  can genuinely differ."
  [form-meta form lexical-env form-short]
  (finalize (merge form-meta {::type ::keyword, ::form form, ::lexical-env lexical-env
                              ::form-short form-short})))

(defn ->string
  "Construct a string edge node. Strings name edges in map-like structures
  where keys are strings (e.g. JSON-shaped data)."
  [form-meta form lexical-env]
  (finalize (merge form-meta {::type ::string, ::form form, ::lexical-env lexical-env})))

(defn ->symbol
  "Construct a symbol node. Represents a bare symbol reference that resolves to a
  value at runtime. Distinguished from `->var` by not being var-backed and from
  `->fn` by not being callable."
  [form-meta form lexical-env value var form-resolved form-short]
  (finalize (merge form-meta {::type ::symbol, ::form form, ::lexical-env lexical-env
                              ::resolved-runtime-value value, ::var var
                              ::form-resolved form-resolved, ::form-short form-short})))

(defn ->var
  "Construct a var node. Represents a Clojure var reference with known parameters.
  The form is the var form itself (e.g. `#'inc`). `symbolic-edge`, `node-value`,
  and `parameters` are all derived: symbolic-edge and value are the form,
  parameters are always `'(%)`."
  [form-meta form lexical-env form-resolved form-short]
  (finalize (merge form-meta {::type ::var, ::form form, ::lexical-env lexical-env
                              ::form-resolved form-resolved, ::form-short form-short})))

(defn ->quoted-var
  "Construct a quoted-var node (`#'foo`). The var is referenced but not called;
  its value is the var object itself."
  [form-meta form lexical-env symbolic-edge form-resolved form-short]
  (finalize (merge form-meta {::type ::quoted-var, ::form form, ::lexical-env lexical-env
                              ::symbolic-edge symbolic-edge, ::form-resolved form-resolved
                              ::form-short form-short})))

(defn ->pull
  "Construct a pull node. A pull collects a set of edges (its children) from a
  map-like value. `symbolic-edges` is derived from the form (the vector of edge
  labels). `pull-has-star?` indicates whether `*` was present, which triggers
  dynamic suggestion augmentation at runtime."
  [form-meta form lexical-env pull-has-star?
   & {:keys [recursion-point recursion-limit]}]
  (finalize (cond-> (merge form-meta {::type ::pull, ::form form, ::lexical-env lexical-env
                                      ::pull-has-star? pull-has-star?})
              recursion-point (assoc ::recursion-point true, ::recursion-limit recursion-limit))))

(defn ->fn
  "Construct an fn node. Represents a plain function value (not a var) used as
  an edge. The function will be applied to the value at point during traversal.
  Both `symbolic-edge` and `node-value` are derived from the form (the function
  object itself)."
  [form-meta form lexical-env]
  (finalize (merge form-meta {::type ::fn, ::form form, ::lexical-env lexical-env})))

(defn ->lambda
  "Construct a lambda node. Represents an inline function expression. Lambda
  nodes arise from two HFQL syntactic forms:
  - Explicit: `[#(:db/id %)]` -- user-written lambda, no `kind`.
  - Keyword-as-function: `{:db/id [...]}` -- the analyzer desugars the keyword
    edge into a lambda equivalent to `#(:db/id %)`. `kind` is `::keyword-fn`.
  `node-value` is derived from the form (the lambda expression itself).
  `symbolic-edge` genuinely differs from form (it's a display summary like
  `(fn name ...)`)."
  [form-meta form lexical-env symbolic-edge parameters
   & {:keys [kind form-resolved form-short]}]
  (finalize (cond-> (merge form-meta {::type ::lambda, ::form form, ::lexical-env lexical-env
                                      ::symbolic-edge symbolic-edge, ::parameters parameters})
              kind          (assoc ::kind kind)
              form-resolved (assoc ::form-resolved form-resolved)
              form-short    (assoc ::form-short form-short))))

(defn ->call
  "Construct a call node. Represents a function call expression like
  `(user-by-id 42 %)`. The analyzer resolves the var, extracts parameters from
  arglists, and records the provided arguments -- i.e. all values that are
  bound at analysis time (literals, lexical bindings, etc.), as opposed to
  unbound parameters like `%` which are resolved later at runtime.
  `symbolic-edge` is derived from the form (the full call form)."
  [form-meta form lexical-env value var parameters arguments
   partial form-resolved]
  (finalize (merge form-meta {::type ::call, ::form form, ::lexical-env lexical-env
                              ::resolved-runtime-value value, ::var var
                              ::parameters parameters, ::arguments arguments, ::partial partial
                              ::form-resolved form-resolved})))

(defn- ->traversal
  [form-meta form lexical-env partial
   & {:keys [kind multiplicity edge recursion-point recursion-limit]}]
  (finalize (cond-> (merge form-meta {::type ::traversal, ::form form
                                      ::lexical-env lexical-env, ::partial partial
                                      ::multiplicity (or multiplicity ::one)})
              kind             (assoc ::kind kind)
              edge             (assoc ::edge edge)
              recursion-point  (assoc ::recursion-point true
                                      ::recursion-limit recursion-limit))))

(defn ->star-traversal
  "Construct a star traversal node. Produced from the pair form `{* ...}`, meaning
  \"for all children\": the value at point is expected to be a collection and
  the continuation is applied to each element. Always `::many` with
  `::kind ::star`. There is no edge node because there is no named edge -- the
  traversal fans out over the collection itself, and `*` is just syntax. Not to
  be confused with pull star (`*` inside a pull like `[:a :b *]`), which means
  \"suggest all edges\" via the Suggestable protocol. E.g. `{* inc}` on `(1 2
  3)`, `{* [:foo]}` on `({:foo 1} {:foo 2})`."
  [form-meta form partial
   & {:keys [recursion-point recursion-limit]}]
  (->traversal form-meta form {} partial
    :kind ::star :multiplicity ::many
    :recursion-point recursion-point :recursion-limit recursion-limit))

(defn ->edge-traversal
  "Construct an edge traversal node. Represents navigation from a parent value
  to a child value through a specific edge (keyword, symbol, call, etc.). The
  `edge` argument is the inner AST node describing the edge function -- e.g. a
  ::keyword node for `:user/name`, a ::call node for `(all-users)`. `lexical-env`
  is inherited from the edge. `multiplicity` defaults to `::one`; pass `::many`
  when the traversal fans out over a collection."
  [form-meta form edge partial
   & {:keys [multiplicity recursion-point recursion-limit]}]
  (->traversal form-meta form (::lexical-env edge) partial
    :edge edge :multiplicity multiplicity
    :recursion-point recursion-point :recursion-limit recursion-limit))

;;;; Accessors
;;
;; Each function extracts a single field from an AST node map. Prefer these over
;; raw keyword access so that (1) the map representation stays an implementation
;; detail, (2) tooling (ns-publics, IDE go-to-definition) can discover the API,
;; and (3) documentation is attached to a navigable Clojure var.

(defn node-type
  "Return the type of `node`. Node types are defined by the `::type` spec."
  [node] (::type node))

(defn form
  "Return the original source form of `node` as read by the analyzer."
  [node] (::form node))

(defn form-short
  "Return the shortest unambiguous form for display. For keywords this is the
  unqualified name when unambiguous; for symbols it drops the namespace when the
  var is unique in the current context. Falls back to the original form for node
  types where no shortening applies (literals, strings, stars, etc.)."
  [node] (or (::form-short node) (::form node)))

(defn form-resolved
  "Return the alias-resolved form. E.g. `::foo` becomes `:ns/foo`, `inc`
  becomes `clojure.core/inc`. The type depends on the node: a qualified keyword
  for keyword nodes, a qualified symbol for var/symbol nodes, or a full call
  form like `(my.ns/fn arg1 %)` for call nodes. Falls back to the original form
  for node types where no resolution applies (literals, strings, stars, etc.)."
  [node] (or (::form-resolved node) (::form node)))

(defn symbolic-edge
  "Return the edge label for this node in its parent pull. This is what
  identifies a column in a table view -- e.g. `:user/name`, `'identity`,
  `\"json-key\"`. For most edge types (literal, keyword, string, symbol, var,
  fn, call) the edge label is the form itself. Only quoted-var and lambda store
  a distinct symbolic-edge. Returns nil for structural nodes (pull, star, recur,
  traversal) that don't represent a single edge."
  [node]
  (if (contains? node ::symbolic-edge)
    (::symbolic-edge node)
    (case (node-type node)
      (::literal ::keyword ::string ::symbol ::var ::fn ::call) (::form node)
      nil)))

(defn symbolic-edges
  "Return the ordered list of child edge labels for a pull node. Determines
  column order in table views. Derived from the form (pull forms and their
  edge list are the same value)."
  [node] (when (= ::pull (node-type node)) (::form node)))

(defn node-value
  "Return the runtime value of this node. For nodes that carry a distinct
  resolved runtime value (symbol, call), returns it. For nodes where the form IS
  the value (keyword, literal, string, fn, var, lambda), returns the form.
  Returns nil for structural nodes (pull, star, recur, traversal, quoted-var)
  that don't represent a single value."
  [node]
  (if (contains? node ::resolved-runtime-value)
    (::resolved-runtime-value node)
    (case (node-type node)
      (::keyword ::literal ::string ::fn ::var ::lambda) (::form node)
      nil)))

(defn node-var
  "Return the Clojure var backing this node, if any. Present on var, symbol, and
  call nodes when the form resolved to a var. Nil for anonymous functions,
  literals, keywords."
  [node] (::var node))

(defn parameters
  "Return the symbolic parameter list of this node. Present on var, call, and
  lambda nodes. Parameters are symbols like `['db '%]` extracted from the var's
  arglists or the lambda's argument vector. Var nodes always have `'(%)` (derived).
  These are names, not values -- see `arguments` for runtime values."
  [node]
  (if (contains? node ::parameters)
    (::parameters node)
    (when (= ::var (node-type node)) '(%))))

(defn arguments
  "Return the runtime argument values provided at a call site. Present on call
  nodes. Unlike `parameters` (which are symbolic names), arguments are the
  actual values passed to the function at analysis time, e.g. `[42 :unbound]`
  in `(user-by-id 42 %)`."
  [node] (::arguments node))

(defn lexical-env
  "Return the lexical environment captured at analysis time. A map from symbols
  to values representing the bindings in scope when this node was analyzed."
  [node] (::lexical-env node))

(defn node-id
  "Return the content-derived identity hash of this node. Two nodes with the
  same type, form, and edge labels share the same id, even across independently
  constructed trees. Useful for comparing structural equality -- e.g. two cells
  representing the same column in the same pull expression."
  [node] (::id node))

(defn pull-has-star?
  "Return true if this pull node's expression contains the `*` wildcard.
  Only meaningful on pull nodes. When true, the runtime augments the pull with
  dynamically suggested edges from the `Suggestable` protocol."
  [node] (::pull-has-star? node))

(defn edge
  "Return the inner AST node describing a traversal's edge function. Present on
  traversal nodes. The edge node is itself a full AST node map. For example, in
  `{:user/name [...]}` the traversal's edge is the `::keyword` node for
  `:user/name`. In `{(all-users) [...]}` the edge is a `::call` node for
  `(all-users)`. The interpreter reads the edge to determine how to navigate
  from parent to child value."
  [node] (::edge node))

(defn kind
  "Return the syntactic origin tag for traversal and lambda nodes. Where
  `node-type` says WHAT a node is structurally (e.g. `::traversal`), `kind`
  says HOW it was generated:
  - `::star` (traversals only): from `{* ...}` -- \"for all children\". The value
    at point is a collection and the continuation applies to each element.
  - `::keyword-fn` (lambdas only): from `{:k [...]}` keyword-as-function sugar.
  Nil on nodes without a special syntactic origin. The interpreter uses kind to
  choose between navigation strategies."
  [node] (::kind node))

(defn multiplicity
  "Return the cardinality of this traversal node's result. `::one` for single-
  valued traversals (e.g. `{:user/name [...]}`), `::many` for collection-valued
  traversals. Star traversals (`{* ...}`) are always `::many`. Always present on
  traversal nodes; defaults to `::one` at construction time."
  [node] (::multiplicity node))

(defn recursion-limit
  "Return the remaining recursion depth for this node. `##Inf` for unlimited
  recursion (`...`), a nat-int for bounded recursion (`{:attr 5}`, `[* 5]`), counting down
  at each unfolding. Absent when the node is not recursive."
  [node] (::recursion-limit node))

;;;; Type predicates
;;
;; Each predicate tests whether a node is of a specific ::type. Prefer these
;; over `(= ::ast/pull (ast/node-type x))` for readability and encapsulation.

(defn star?       "True if `node` is a pull star (`*`) wildcard." [node] (= ::star (node-type node)))
(defn recur-node? "True if `node` is a recursion marker (`...` in `[* ...]` or `{x ...}`, `5` in `[* 5]` or `{x 5}`)" [node] (= ::recur (node-type node)))
(defn literal?    "True if `node` is a literal value (number, boolean, nil, ...)." [node] (= ::literal (node-type node)))
(defn keyword-node? "True if `node` is a keyword edge." [node] (= ::keyword (node-type node)))
(defn string-node?   "True if `node` is a string edge." [node] (= ::string (node-type node)))
(defn symbol-node?        "True if `node` is a symbol reference." [node] (= ::symbol (node-type node)))
(defn var-node?   "True if `node` is a var reference." [node] (= ::var (node-type node)))
(defn quoted-var? "True if `node` is a quoted var (`'#'foo`). Note the quote in front." [node] (= ::quoted-var (node-type node)))
(defn pull?       "True if `node` is a pull."   [node] (= ::pull (node-type node)))
(defn fn-node?    "True if `node` is a plain function value used as an edge." [node] (= ::fn (node-type node)))
(defn lambda?     "True if `node` is an inline lambda expression." [node] (= ::lambda (node-type node)))
(defn call?       "True if `node` is a function call expression." [node] (= ::call (node-type node)))
(defn traversal?  "True if `node` is a traversal." [node] (= ::traversal (node-type node)))

;;;; Semantic predicates

(defn star-traversal?
  "True if this is a star traversal (`{* ...}` -- \"for all children\"). The value
  at point is a collection and the continuation applies to each element."
  [node] (and (traversal? node) (= ::star (kind node))))

(defn multiplicity-many?
  "True if this node fans out over a collection. Equivalent to checking that
  `(multiplicity node)` is `::many`."
  [node] (= ::many (multiplicity node)))

(defn recursion-point?
  "True if this node is the TARGET of a `...` recursion. The interpreter uses
  this to find where to loop back when unfolding recursive structures."
  [node] (boolean (::recursion-point node)))

(defn bounded-recursion?
  "True if this recur node has a finite recursion limit (e.g. `... 5`)."
  [node] (and (recur-node? node) (not= ##Inf (recursion-limit node))))

(defn unbounded-recursion?
  "True if this recur node has unlimited recursion (`...`)."
  [node] (and (recur-node? node) (= ##Inf (recursion-limit node))))

(defn partial?
  "True if this node still expects arguments to be provided (e.g. `%` is
  unbound). A partial node cannot be fully evaluated until seeded."
  [node] (boolean (::partial node)))

(defn leaf?
  "True if this node is terminal -- it has no children to unfold. The analyzer
  sets `::leaf false` on edge nodes (keyword, call, etc.) inside a traversal
  when the traversal has a continuation. Absent key is treated as true."
  [node] (not (false? (::leaf node))))

(defn projection-type
  "Classify an AST node for interpreter dispatch. Returns the `::type` value
  for most nodes, but refines `::symbol` to `::fn` when the node's runtime
  value is a function. This distinction matters because symbols that resolve to
  functions are called during traversal, while other symbols are looked up as
  keys."
  [point]
  (case (node-type point)
    ::symbol (if (fn? (node-value point)) ::fn ::symbol)
    (node-type point)))

;;;; Mutators
;;
;; For keys set by the interpreter at runtime, outside of the static AST
;; produced by phi. These are not part of the constructor signatures because
;; they are discovered during tree traversal, not analysis.

(defn mark-recursion-root
  "Mark this node as the root of a recursive structure. Set by the interpreter
  when it detects a `...` recursion and identifies which ancestor is the
  recursion target."
  [node] (assoc node ::recursion-root true))

(defn recursion-root?
  "True if this node has been marked as a recursion root by the interpreter."
  [node] (boolean (::recursion-root node)))

(defn mark-recursive-child
  "Mark this node as a child produced by recursive unfolding. Used by the
  navigator UI to filter out recursive children from column pickers."
  [node] (assoc node ::recursive-child true))

(defn recursive-child?
  "True if this node was produced by recursive unfolding."
  [node] (boolean (::recursive-child node)))

(defn dec-recursion-limit
  "Decrement the recursion limit by one, clamping at zero. Called by the
  interpreter each time it unfolds one level of a recursive structure. When the
  limit reaches zero, recursion stops and the node becomes a leaf."
  [node] (update node ::recursion-limit #(max 0 (dec %))))

(defn set-leaf
  "Set whether this traversal node is terminal. `terminal?` is a boolean: true
  means the traversal has no children to unfold (it is a leaf), false means the
  edge has a continuation and the interpreter should produce children for it.
  The analyzer sets this to false on edges with pull or traversal continuations."
  [node terminal?] (assoc node ::leaf terminal?))

;;;; Tests

;; Helper: all constructed nodes must pass the ::node spec.
(defn- valid-node? [node] (s/valid? ::node node))

(tests
 "->star"
 (let [n (->star nil)]
   (valid-node? n) := true
   (star? n) := true
   (form n) := '*
   (partial? n) := true
   ))

(tests
 "->star with form-meta"
 (let [n (->star {:line 42})]
   (star? n) := true
   (:line n) := 42))

(tests
 "->unbounded-recursion"
 (let [n (->unbounded-recursion nil)]
   (valid-node? n) := true
   (recur-node? n) := true
   (form n) := '...
   (unbounded-recursion? n) := true
   (bounded-recursion? n) := false
   (recursion-limit n) := ##Inf
   (recursion-limit (dec-recursion-limit n)) := ##Inf
   ))

(tests
  "->bounded-recursion"
  (let [n (->bounded-recursion nil 3)]
    (valid-node? n) := true
    (recur-node? n) := true
    (unbounded-recursion? n) := false
    (bounded-recursion? n) := true
    (recursion-limit n) := 3

    (recursion-limit (dec-recursion-limit n)) := 2
    (-> n
      (dec-recursion-limit) ; 2
      (dec-recursion-limit) ; 1
      (dec-recursion-limit) ; 0
      (dec-recursion-limit) ; 0
      (recursion-limit)
      ) := 0
    ))

(tests
 "->literal"
 (let [n (->literal nil 42 {})]
   (valid-node? n) := true
   (literal? n) := true
   (node-value n) := 42
   (symbolic-edge n) := 42
   (partial? n) := true
   ))

(tests
 "->keyword"
 (let [n (->keyword nil :db/ident {} :ident)]
   (valid-node? n) := true
   (keyword-node? n) := true
   (form n) := :db/ident
   (symbolic-edge n) := :db/ident
   (form-resolved n) := :db/ident
   (form-short n) := :ident
   (partial? n) := true
   ))

(tests
 "->string"
 (let [n (->string nil "name" {})]
   (valid-node? n) := true
   (string-node? n) := true
   (form n) := "name"
   (symbolic-edge n) := "name"
   (partial? n) := true
   ))

(tests
 "->symbol (non-fn value)"
 (let [n (->symbol nil 'x {} 42 nil 'my.ns/x 'x)]
   (valid-node? n) := true
   (symbol-node? n) := true
   (form n) := 'x
   (symbolic-edge n) := 'x
   (node-value n) := 42
   (node-var n) := nil
   (form-resolved n) := 'my.ns/x
   (form-short n) := 'x
   (partial? n) := true
   ))

(tests
  "->symbol (fn value)"
  (let [n (->symbol nil 'inc {} inc #'inc 'clojure.core/inc 'inc)]
    (valid-node? n) := true
    (symbol-node? n) := true
    (node-value n) := inc
    (node-var n) := #'inc
    (form-resolved n) := 'clojure.core/inc
    (form-short n) := 'inc
    (partial? n) := true
    ))

(tests
 "->var"
 (let [n (->var nil #'inc {} 'clojure.core/inc 'inc)] ; TODO don't pass the resolved form, it can be infered from the var object itself.
   (valid-node? n) := true
   (var-node? n) := true
   (node-value n) := #'inc
   (parameters n) := '(%)
   (form-resolved n) := 'clojure.core/inc
   (form-short n) := 'inc
   (partial? n) := true
   ))

(tests
 "->quoted-var"
 (let [n (->quoted-var nil '#'inc {} '#'inc 'clojure.core/inc 'inc)] ; TODO no need to pass '#'inc twice.
   (valid-node? n) := true
   (quoted-var? n) := true
   (symbolic-edge n) := '#'inc
   (form-resolved n) := 'clojure.core/inc
   (form-short n) := 'inc
   (partial? n) := true
   ))

(tests
 "->pull without recursion"
 (let [n (->pull nil [:a :b] {} false)]
   (valid-node? n) := true
   (pull? n) := true
   (form n) := [:a :b]
   (symbolic-edges n) := [:a :b] ; TODO symbolic-edges as an accessor function should not exist. It should be `symbolic-edge` that, when called on pull does return the symbolic edges value.
   (pull-has-star? n) := false
   (partial? n) := true
   ))

(tests
 "->pull with star"
 (let [n (->pull nil [:a '*] {} true)]
   (valid-node? n) := true
   (pull? n) := true
   (pull-has-star? n) := true
   (symbolic-edges n) := [:a '*] ; TODO same comment as above.
   (partial? n) := true
   ))

(tests
 "->pull with recursion"
 (let [n (->pull nil [:a] {} false :recursion-point true :recursion-limit ##Inf)] ; TODO recursion point and limit should not be passed as an arg to the constructor, instead AST nodes should have ::children and form a Rose Tree. Recursion point becomes a derived (computed) value and recursion limit is derived from the node-value of the eventual recusion node child.
   (valid-node? n) := true
   (recursion-point? n) := true
   (recursion-limit n) := ##Inf
   (symbolic-edges n) := [:a] ; TODO same comment about symbolic-edge vs symbolic-edges. ; TODO also as a consequence of the comment about recursion-point, the returned value here should contain `'...` .
   (partial? n) := true
   ))

(tests
 "->fn"
 (let [n (->fn nil identity {})]
   (valid-node? n) := true
   (fn-node? n) := true
   (node-value n) := identity
   (partial? n) := true
   ))

(tests
 "->lambda explicit (no kind)"
 (let [n (->lambda nil '(fn [%] (:db/id %)) {} '(fn [%] (:db/id %)) '(%))]
   (valid-node? n) := true
   (lambda? n) := true
   (parameters n) := '(%)
   (kind n) := nil
   (partial? n) := true
   ))

(tests
 "->lambda keyword-fn"
 (let [n (->lambda nil '(fn [%] (:db/id %)) {} :db/id '(%)
                   :kind ::keyword-fn :form-resolved :db/ident :form-short :ident)]
   (valid-node? n) := true
   (kind n) := ::keyword-fn
   (form-resolved n) := :db/ident
   (form-short n) := :ident
   ))

(tests
 "->call"
 (let [n (->call nil '(+ 42 a %) {'a 1} +  #'+ '(42 a %) '(42 1 '%) true 'clojure.core/+)] ; TODO partial should be infered and computed. ; TODO form resolved should be derived from the resolved var, if any. If `f` resolve to a lexical binding then there's no var and we can reused the symbolic form.
   (valid-node? n) := true
   (call? n) := true
   (node-value n) := +
   (node-var n) := #'+
   (parameters n) := '(42 a %)
   (arguments n) := '(42 1 '%)
   (form-resolved n) := 'clojure.core/+
   (partial? n) := true
   ))

(tests
 "->call non-partial"
 (let [n (->call nil '(inc 42) {} inc #'inc '(x) '(42) false 'clojure.core/inc)] ; TODO same comment(s) as above.
   (valid-node? n) := true
   (call? n) := true
   (partial? n) := false
   ))

(tests
 "->star-traversal"
 (let [n (->star-traversal nil '{* inc} true)]
   (valid-node? n) := true
   (traversal? n) := true
   (star-traversal? n) := true
   (kind n) := ::star
   (multiplicity n) := ::many
   (edge n) := nil ; correct. '* is purely syntactic.
   (lexical-env n) := {}
   (partial? n) := true
   ))

(tests
 "->star-traversal with recursion"
 (let [n (->star-traversal nil '{* ...} true :recursion-point true :recursion-limit ##Inf)] ; TODO same comment as for pull with recursion.
   (valid-node? n) := true
   (traversal? n) := true
   (star-traversal? n) := true
   (kind n) := ::star
   (multiplicity n) := ::many
   (recursion-point? n) := true
   (recursion-limit n) := ##Inf
   (lexical-env n) := {}
   (partial? n) := true
   ))

(tests
 "->edge-traversal defaults to ::one"
 (let [kw-edge (->keyword nil :user/name {} :name)
       n (->edge-traversal nil '{:user/name [...]} kw-edge true)]
   (valid-node? n) := true
   (traversal? n) := true
   (multiplicity n) := ::one
   (kind n) := nil
   (edge n) := kw-edge
   (lexical-env n) := (lexical-env kw-edge) ; good!
   (identical? (lexical-env n) (lexical-env kw-edge)) := true ; even better!
   ))

(tests
 "->edge-traversal with ::many"
 (let [kw-edge (->keyword nil :users {} :users)
       n (->edge-traversal nil '{:users [...]} kw-edge true :multiplicity ::many)] ; TODO this test is not a thing. Multiplicity many comes from the use of star traversal. This test should be removed.
   (valid-node? n) := true
   (multiplicity n) := ::many))

(tests
 "->edge-traversal with recursion"
 (let [kw-edge (->keyword nil :children {} :children)
       n (->edge-traversal nil '{:children ...} kw-edge true
                           :recursion-point true :recursion-limit 3)] ; TODO this test is legit, but the user of `...` implies recursion limit should be ##Inf. Also we should have a sibling test for `{:children 3}`.
   (valid-node? n) := true
   (recursion-point? n) := true
   (recursion-limit n) := 3))

(tests
 "node-value: keyword returns form"
 (node-value (->keyword nil :db/id {} :id)) := :db/id)

(tests
 "node-value: literal returns form"
 (node-value (->literal nil 42 {})) := 42)

(tests
 "node-value: literal nil returns nil (via form branch)"
 (node-value (->literal nil nil {})) := nil)

(tests
 "node-value: string returns form"
 (node-value (->string nil "x" {})) := "x")

(tests
 "node-value: symbol returns resolved-runtime-value"
 (node-value (->symbol nil 'x {} 99 nil 'x 'x)) := 99)

(tests
 "node-value: fn returns resolved-runtime-value"
 (node-value (->fn nil inc {})) := inc)

(tests
 "node-value: var returns resolved-runtime-value"
 (node-value (->var nil #'inc {} 'clojure.core/inc 'inc)) := #'inc)

(tests
 "node-value: lambda returns resolved-runtime-value"
 (node-value (->lambda nil '(fn [%] %) {} '(fn [%] %) '(%))) := '(fn [%] %)) ; TODO this test is not great. ->lambda should take a parameter for the lambda's runtime value and here we should compare the value, not the quoted form.

(tests
 "node-value: call returns resolved-runtime-value"
 (node-value (->call nil '(f) {} 'f nil '() '() false 'f)) := 'f) ; TODO same as for lambda just above.

(tests
 "node-value: pull returns nil (structural)"
 (node-value (->pull nil [:a] {} false)) := nil)

(tests
 "node-value: star returns nil (structural)"
 (node-value (->star nil)) := nil)

(tests
 "node-value: recur returns nil (structural)"
 (node-value (->unbounded-recursion nil)) := nil)

(tests
 "node-value: traversal returns nil (structural)"
 (node-value (->star-traversal nil '{* x} true)) := nil)

(tests
 "node-value: quoted-var returns nil (structural)"
 (node-value (->quoted-var nil '#'inc {} '#'inc 'clojure.core/inc 'inc)) := nil) ; TODO no this should return the quoted var form

(tests
 "form-short returns ::form-short when present"
 (form-short (->keyword nil :db/ident {} :ident)) := :ident)

(tests
 "form-short falls back to ::form"
 (form-short (->literal nil 42 {})) := 42
 (form-short (->star nil)) := '*
 (form-short (->string nil "x" {})) := "x")

(tests
 "form-resolved returns ::form-resolved when present"
 (form-resolved (->symbol nil 'inc {} inc #'inc 'clojure.core/inc 'inc)) := 'clojure.core/inc)

(tests
 "form-resolved falls back to ::form"
 (form-resolved (->literal nil 42 {})) := 42
 (form-resolved (->star nil)) := '*)

(tests
 "star?"
 (star? (->star nil)) := true
 (star? (->literal nil 42 {})) := false)

(tests ; TODO remove this test, already covered above
 "recur-node?"
 (recur-node? (->unbounded-recursion nil)) := true
 (recur-node? (->star nil)) := false)

(tests ; TODO remove this test, already covered above
 "literal?"
 (literal? (->literal nil 42 {})) := true
 (literal? (->keyword nil :a {} :a)) := false)

(tests ; TODO remove this test, already covered above
 "keyword-node?"
 (keyword-node? (->keyword nil :a {} :a)) := true
 (keyword-node? (->literal nil 42 {})) := false)

(tests ; TODO remove this test, already covered above
 "string-node?"
 (string-node? (->string nil "x" {})) := true
 (string-node? (->keyword nil :a {} :a)) := false)

(tests ; TODO remove this test, already covered above
 "symbol-node?"
 (symbol-node? (->symbol nil 'x {} 1 nil 'x 'x)) := true
 (symbol-node? (->keyword nil :a {} :a)) := false)

(tests ; TODO remove this test, already covered above
 "var-node?"
 (var-node? (->var nil #'inc {} 'clojure.core/inc 'inc)) := true
 (var-node? (->symbol nil 'x {} 1 nil 'x 'x)) := false)

(tests ; TODO remove this test, already covered above
 "quoted-var?"
 (quoted-var? (->quoted-var nil '#'inc {} '#'inc 'clojure.core/inc 'inc)) := true
 (quoted-var? (->var nil #'inc {} 'clojure.core/inc 'inc)) := false)

(tests ; TODO remove this test, already covered above
 "pull?"
 (pull? (->pull nil [:a] {} false)) := true
 (pull? (->star nil)) := false)

(tests ; TODO remove this test, already covered above
 "fn-node?"
 (fn-node? (->fn nil inc {})) := true
 (fn-node? (->symbol nil 'x {} 1 nil 'x 'x)) := false)

(tests ; TODO remove this test, already covered above
 "lambda?"
 (lambda? (->lambda nil '(fn [%] %) {} '(fn [%] %) '(%))) := true
 (lambda? (->fn nil inc {})) := false)

(tests ; TODO remove this test, already covered above
 "call?"
 (call? (->call nil '(f) {} 'f nil '() '() false 'f)) := true
 (call? (->lambda nil '(fn [%] %) {} '(fn [%] %) '(%))) := false)

(tests ; TODO remove this test, already covered above
 "traversal?"
 (traversal? (->star-traversal nil '{* x} true)) := true
 (traversal? (->pull nil [:a] {} false)) := false)

(tests ; TODO remove this test, already covered above
 "star-traversal? checks both traversal type and star kind"
 (star-traversal? (->star-traversal nil '{* x} true)) := true
 (let [kw (->keyword nil :a {} :a)]
   (star-traversal? (->edge-traversal nil '{:a [...]} kw true))) := false
 (star-traversal? (->star nil)) := false) ; pull star is NOT a star traversal

(tests ; TODO remove this test, already covered above
 "multiplicity-many?"
 (multiplicity-many? (->star-traversal nil '{* x} true)) := true
 (let [kw (->keyword nil :a {} :a)]
   (multiplicity-many? (->edge-traversal nil '{:a [...]} kw true))) := false
 (let [kw (->keyword nil :a {} :a)]
   (multiplicity-many? (->edge-traversal nil '{:a [...]} kw true :multiplicity ::many))) := true)

(tests ; TODO remove this test, not meaningful after the AST rose tree refactor.
 "recursion-point?"
 (recursion-point? (->pull nil [:a] {} false :recursion-point true :recursion-limit ##Inf)) := true
 (recursion-point? (->pull nil [:a] {} false)) := false
 (recursion-point? (->star-traversal nil '{* ...} true :recursion-point true :recursion-limit 3)) := true)

(tests ; TODO remove this test, already covered above
 "bounded-recursion?"
 (bounded-recursion? (->bounded-recursion nil 5)) := true
 (bounded-recursion? (->unbounded-recursion nil)) := false
 (bounded-recursion? (->star nil)) := false) ; non-recur node

(tests ; TODO remove this test, already covered above
 "unbounded-recursion?"
 (unbounded-recursion? (->unbounded-recursion nil)) := true
 (unbounded-recursion? (->bounded-recursion nil 5)) := false
 (unbounded-recursion? (->star nil)) := false) ; non-recur node

(tests ; TODO remove this test, already covered above
 "partial? defaults to true"
 (partial? (->star nil)) := true
 (partial? (->keyword nil :a {} :a)) := true)

(tests ; TODO remove this test, already covered above
 "partial? respects explicit false"
 (partial? (->call nil '(f 1) {} 'f nil '[x] '[1] false 'f)) := false)

(tests ; TODO leaf? should be derived from the presence of children in the rose tree.
 "leaf? absent key is true"
 (leaf? (->keyword nil :a {} :a)) := true)

(tests
 "leaf? explicit false"
 (leaf? (assoc (->keyword nil :a {} :a) ::leaf false)) := false)

(tests
 "leaf? explicit true"
 (leaf? (assoc (->keyword nil :a {} :a) ::leaf true)) := true)

;; TODO figure out if projection-type brings anything more than `node-type`. If no, remove it.
(tests
 "projection-type: keyword"
 (projection-type (->keyword nil :a {} :a)) := ::keyword)

(tests
 "projection-type: symbol with non-fn value"
 (projection-type (->symbol nil 'x {} 42 nil 'x 'x)) := ::symbol)

(tests
 "projection-type: symbol with fn value -> ::fn"
 (projection-type (->symbol nil 'inc {} inc #'inc 'clojure.core/inc 'inc)) := ::fn)

(tests
 "projection-type: pull"
 (projection-type (->pull nil [:a] {} false)) := ::pull)

(tests
 "projection-type: traversal"
 (projection-type (->star-traversal nil '{* x} true)) := ::traversal)

(tests
 "projection-type: fn"
 (projection-type (->fn nil inc {})) := ::fn)

(tests
 "projection-type: call"
 (projection-type (->call nil '(f) {} 'f nil '() '() false 'f)) := ::call)

(tests
 "projection-type: literal"
 (projection-type (->literal nil 42 {})) := ::literal)

(tests
 "projection-type: lambda"
 (projection-type (->lambda nil '(fn [%] %) {} '(fn [%] %) '(%))) := ::lambda)

(tests
 "mark-recursion-root"
 (let [n (->pull nil [:a] {} false)
       n' (mark-recursion-root n)]
   (pull? n) := true
   (pull? n') := true
   (form n) := (form n')
   (recursion-root? n) := false
   (recursion-root? n') := true
   ))

(tests
 "mark-recursive-child"
 (let [n (->star-traversal nil '{* x} true) ; arbitrary node
       n' (mark-recursive-child n)]
   (star-traversal? n) := true
   (star-traversal? n') := true
   (= (form n) (form n')) := true
   (recursive-child? n) := false
   (recursive-child? n') := true
   ))

(tests ; TODO remove this test, already covered above 
 "dec-recursion-limit counts down"
 (let [n (->bounded-recursion nil 3)]
   (recursion-limit n) := 3
   (recursion-limit (dec-recursion-limit n)) := 2
   (recursion-limit (dec-recursion-limit (dec-recursion-limit n))) := 1
   (recursion-limit (dec-recursion-limit (dec-recursion-limit (dec-recursion-limit n)))) := 0))

(tests ; TODO remove this test, already covered above
 "dec-recursion-limit clamps at zero"
 (let [n (->bounded-recursion nil 0)]
   (recursion-limit (dec-recursion-limit n)) := 0))

(tests ; TODO leaf? should be derived from the presence of children in the rose tree or from when recursion limit hits 0
 "set-leaf"
 (let [kw (->keyword nil :a {} :a)]
   (leaf? kw) := true
   (leaf? (set-leaf kw false)) := false
   (leaf? (set-leaf kw true)) := true))

(tests
 "compute-node-id: same content produces same id"
 (let [a (->keyword nil :db/id {} :id)
       b (->keyword nil :db/id {} :id)]
   (node-id a) := (node-id b)))

(tests
 "compute-node-id: different content produces different id"
 (let [a (->keyword nil :db/id {} :id)
       b (->keyword nil :db/name {} :name)]
   (not= (node-id a) (node-id b)) := true))

(tests ; TODO this test case is boggus. ->string for 42 is forbidden. BTW we need the spec to reflect that. We should find a better test example.
 "compute-node-id: different types, same form -> different id"
 (let [a (->literal nil 42 {})
       b (->string nil 42 {})]
   (not= (node-id a) (node-id b)) := true))

(tests
 "partial defaults to true when not provided" ; TODO partial should not be provided. It should be infered, and therefore doesn't "default" to anything.
 (partial? (->star nil)) := true
 (partial? (->keyword nil :a {} :a)) := true
 (partial? (->literal nil 42 {})) := true)

(tests
 "->edge-traversal multiplicity defaults to ::one" ; TODO we could rather say that the default multiplicity is ::one, unless explicitely marked ::many by the use of star traversal
 (let [kw (->keyword nil :a {} :a)]
   (multiplicity (->edge-traversal nil '{:a [...]} kw true)) := ::one))

(tests
 "->star-traversal is always ::many"
 (multiplicity (->star-traversal nil '{* x} true)) := ::many)

(tests ; TODO remove this test, already covered above
 "->edge-traversal inherits lexical-env from edge"
 (let [kw (->keyword nil :a '{x 1} :a)]
   (lexical-env (->edge-traversal nil '{:a [...]} kw true)) := '{x 1}))

(tests ; TODO remove this test, already covered above 
 "->star-traversal has empty lexical-env"
 (lexical-env (->star-traversal nil '{* x} true)) := {})
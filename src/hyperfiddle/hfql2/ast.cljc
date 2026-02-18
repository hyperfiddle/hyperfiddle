(ns hyperfiddle.hfql2.ast
  "HFQL AST node API. Specs, constructors, accessors, predicates, and mutators
  for AST nodes. Consumers should access nodes through this API rather than raw
  keyword lookup, so that the map representation remains an implementation detail."
  (:require [clojure.spec.alpha :as s]))

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
(s/def ::var any?)             ; the Clojure var backing this node, if any
(s/def ::parameters any?)      ; symbolic parameter list (symbols like ['db '%])
(s/def ::arguments any?)       ; runtime argument values provided at a call site
(s/def ::pull-has-star? boolean?) ; whether a pull expression contains the `*` wildcard. Pull nodes only.
(s/def ::recursion-point boolean?) ; true when this node is the Target of a `...` recursion
(s/def ::recursion-limit any?) ; remaining recursion depth. ##Inf for unlimited (`...`), nat-int for bounded (`... 5`).
(s/def ::multiplicity #{::one ::many}) ; cardinality of the node's result: ::one for single values, ::many for collections
(s/def ::kind #{::star ::keyword-fn}) ; syntactic origin of this node. Refines ::type: where ::type says WHAT a node
                                      ; is structurally (e.g. ::traversal, ::lambda), ::kind says HOW it was generated.
                                      ; ::star (traversals only): from `{* ...}` -- "for all children". The value at
                                      ;   point is expected to be a collection; the continuation applies to each element.
                                      ; ::keyword-fn (lambdas only): from `{:k [...]}` keyword-as-function sugar.
                                      ; Matters for the interpreter which handles star traversals differently.
(s/def ::edge map?)            ; for traversals: the inner AST node (itself a full node map) that describes the
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
  (s/keys :req [::type ::form ::lexical-env ::partial ::id ::symbolic-edge]))

(defmethod node-spec ::keyword [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id
                ::symbolic-edge ::form-resolved ::form-short]))

(defmethod node-spec ::string [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id ::symbolic-edge]))

(defmethod node-spec ::symbol [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id
                ::symbolic-edge ::resolved-runtime-value ::var ::form-resolved ::form-short]))

(defmethod node-spec ::var [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id
                ::symbolic-edge ::resolved-runtime-value ::parameters ::form-resolved ::form-short]))

(defmethod node-spec ::quoted-var [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id
                ::symbolic-edge ::form-resolved ::form-short]))

(defmethod node-spec ::pull [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id
                ::symbolic-edges ::pull-has-star?]
          :opt [::recursion-point ::recursion-limit]))

(defmethod node-spec ::fn [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id
                ::symbolic-edge ::resolved-runtime-value]))

(defmethod node-spec ::lambda [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id
                ::symbolic-edge ::parameters ::resolved-runtime-value]
          :opt [::kind ::form-resolved ::form-short]))

(defmethod node-spec ::call [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id
                ::symbolic-edge ::resolved-runtime-value ::var ::parameters ::arguments ::form-resolved]
          :opt [::lexical-env]))

(defmethod node-spec ::traversal [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id ::multiplicity]
          :opt [::kind ::edge ::recursion-point ::recursion-limit ::leaf]))

(s/def ::node (s/multi-spec node-spec ::type))

;;;; Internal helpers

(defn compute-node-id
  "Derive a stable identity hash from a node's semantic content.
  Two nodes with the same type, form, edge labels, and resolved/short forms will
  share the same id. Used to compare structural equality across independently
  constructed trees -- e.g. two cells representing the same column in the same
  pull will have the same id even if they belong to different rows."
  [node]
  (hash (select-keys node #{::type ::form ::symbolic-edge ::symbolic-edges ::form-short ::form-resolved})))

(defn- finalize
  "Set defaults and validate. Called by every constructor.
  - Defaults ::partial to true if absent.
  - Computes ::id from semantic content.
  - Validates the resulting map against ::node (dev-time only, via s/assert)."
  [node]
  (let [node (if (contains? node ::partial) node (assoc node ::partial true))]
    (s/assert ::node (assoc node ::id (compute-node-id node)))))

;;;; Constructors
;;
;; Each constructor takes `form-meta` (metadata from the original source form,
;; may be nil) as first argument, followed by required fields for that node type.
;; `form-meta` is merged first, then AST keys are assoc'd on top -- matching how
;; the phi algebra works. The resulting map is finalized (defaults + id + spec
;; check).

(defn ->star
  "Construct a star (`*`) wildcard node. Signals \"include all suggested edges\"
  inside a pull expression."
  [form-meta form lexical-env]
  (finalize (merge form-meta {::type ::star, ::form form, ::lexical-env lexical-env})))

(defn ->recur
  "Construct a recur (`...`) node. Signals recursive unfolding with a depth limit."
  [form-meta form lexical-env recursion-limit]
  (finalize (merge form-meta {::type ::recur, ::form form, ::lexical-env lexical-env
                              ::recursion-limit recursion-limit})))

(defn ->literal
  "Construct a literal value node (number, boolean, nil, etc.)."
  [form-meta form lexical-env symbolic-edge]
  (finalize (merge form-meta {::type ::literal, ::form form, ::lexical-env lexical-env
                              ::symbolic-edge symbolic-edge})))

(defn ->keyword
  "Construct a keyword node. Keywords are the most common edge type in pull
  expressions, e.g. `:user/name` in `[:user/name :user/email]`."
  [form-meta form lexical-env symbolic-edge form-resolved form-short]
  (finalize (merge form-meta {::type ::keyword, ::form form, ::lexical-env lexical-env
                              ::symbolic-edge symbolic-edge, ::form-resolved form-resolved
                              ::form-short form-short})))

(defn ->string
  "Construct a string edge node. Strings name edges in map-like structures
  where keys are strings (e.g. JSON-shaped data)."
  [form-meta form lexical-env symbolic-edge]
  (finalize (merge form-meta {::type ::string, ::form form, ::lexical-env lexical-env
                              ::symbolic-edge symbolic-edge})))

(defn ->symbol
  "Construct a symbol node. Represents a bare symbol reference that resolves to a
  value at runtime. Distinguished from `->var` by not being var-backed and from
  `->fn` by not being callable."
  [form-meta form lexical-env symbolic-edge value var form-resolved form-short]
  (finalize (merge form-meta {::type ::symbol, ::form form, ::lexical-env lexical-env
                              ::symbolic-edge symbolic-edge, ::resolved-runtime-value value, ::var var
                              ::form-resolved form-resolved, ::form-short form-short})))

(defn ->var
  "Construct a var node. Represents a Clojure var reference with known parameters.
  The var's metadata (arglists, etc.) informs parameter resolution."
  [form-meta form lexical-env symbolic-edge value parameters form-resolved form-short]
  (finalize (merge form-meta {::type ::var, ::form form, ::lexical-env lexical-env
                              ::symbolic-edge symbolic-edge, ::resolved-runtime-value value, ::parameters parameters
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
  map-like value. `symbolic-edges` lists the edge labels in declaration order.
  `pull-has-star?` indicates whether `*` was present, which triggers dynamic
  suggestion augmentation at runtime."
  [form-meta form lexical-env symbolic-edges pull-has-star?
   & {:keys [recursion-point recursion-limit]}]
  (finalize (cond-> (merge form-meta {::type ::pull, ::form form, ::lexical-env lexical-env
                                      ::symbolic-edges symbolic-edges, ::pull-has-star? pull-has-star?})
              recursion-point (assoc ::recursion-point true, ::recursion-limit recursion-limit))))

(defn ->fn
  "Construct an fn node. Represents a plain function value (not a var) used as
  an edge. The function will be applied to the value at point during traversal."
  [form-meta form lexical-env symbolic-edge value]
  (finalize (merge form-meta {::type ::fn, ::form form, ::lexical-env lexical-env
                              ::symbolic-edge symbolic-edge, ::resolved-runtime-value value})))

(defn ->lambda
  "Construct a lambda node. Represents an inline function expression. Lambda
  nodes arise from two HFQL syntactic forms:
  - Explicit: `[#(:db/id %)]` -- user-written lambda, no `kind`.
  - Keyword-as-function: `{:db/id [...]}` -- the analyzer desugars the keyword
    edge into a lambda equivalent to `#(:db/id %)`. `kind` is `::keyword-fn`."
  [form-meta form lexical-env symbolic-edge parameters value
   & {:keys [kind form-resolved form-short]}]
  (finalize (cond-> (merge form-meta {::type ::lambda, ::form form, ::lexical-env lexical-env
                                      ::symbolic-edge symbolic-edge, ::parameters parameters
                                      ::resolved-runtime-value value})
              kind          (assoc ::kind kind)
              form-resolved (assoc ::form-resolved form-resolved)
              form-short    (assoc ::form-short form-short))))

(defn ->call
  "Construct a call node. Represents a function call expression like
  `(user-by-id 42 %)`. The analyzer resolves the var, extracts parameters from
  arglists, and records the provided arguments -- i.e. all values that are
  bound at analysis time (literals, lexical bindings, etc.), as opposed to
  unbound parameters like `%` which are resolved later at runtime."
  [form-meta form lexical-env symbolic-edge value var parameters arguments
   partial form-resolved]
  (finalize (merge form-meta {::type ::call, ::form form, ::lexical-env lexical-env
                              ::symbolic-edge symbolic-edge, ::resolved-runtime-value value, ::var var
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
  `\"json-key\"`. Absent on pull and star nodes (they don't represent a single
  edge)."
  [node] (::symbolic-edge node))

(defn symbolic-edges
  "Return the ordered list of child edge labels for a pull node. Determines
  column order in table views."
  [node] (::symbolic-edges node))

(defn node-value
  "Return the runtime value of this node. For nodes that carry a resolved runtime
  value (symbol, fn, var, lambda, call), returns the resolved value. For nodes
  where the form IS the value (keyword, literal, string), returns the form.
  Returns nil for structural nodes (pull, star, recur, traversal, quoted-var)
  that don't represent a single value."
  [node]
  (if (contains? node ::resolved-runtime-value)
    (::resolved-runtime-value node)
    (case (node-type node)
      (::keyword ::literal ::string) (::form node)
      nil)))

(defn node-var
  "Return the Clojure var backing this node, if any. Present on var, symbol, and
  call nodes when the form resolved to a var. Nil for anonymous functions,
  literals, keywords."
  [node] (::var node))

(defn parameters
  "Return the symbolic parameter list of this node. Present on var, call, and
  lambda nodes. Parameters are symbols like `['db '%]` extracted from the var's
  arglists or the lambda's argument vector. These are names, not values -- see
  `arguments` for runtime values."
  [node] (::parameters node))

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
(defn kw?         "True if `node` is a keyword edge." [node] (= ::keyword (node-type node)))
(defn str-node?   "True if `node` is a string edge." [node] (= ::string (node-type node)))
(defn sym?        "True if `node` is a symbol reference." [node] (= ::symbol (node-type node)))
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
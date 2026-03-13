(ns hyperfiddle.hfql2.ast
  "HFQL AST node API. Specs, constructors, accessors, predicates, and mutators
  for AST nodes. Consumers should access nodes through this API rather than raw
  keyword lookup, so that the map representation remains an implementation detail."
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set]
            #?(:clj  [hyperfiddle.ct.rose-tree :as rt :refer [rose-tree mapHead]]
               :cljs [hyperfiddle.ct.rose-tree :as rt :refer [rose-tree mapHead RoseTree]])
            [hyperfiddle.ct.comonad :refer [extract]]
            [hyperfiddle.ct.cofree :refer [unwrap]]
            [hyperfiddle.hfql2.utils :refer [has-meta?]]
            [hyperfiddle.rcf :refer [tests]])
  #?(:clj (:import [hyperfiddle.ct.rose_tree RoseTree])))

;;;; Specs -- keys

(s/def ::type #{::star ::recur ::literal ::keyword ::string ::symbol
                ::var ::quoted-var ::pull ::runtime-value ::quoted-symbol ::lambda ::call ::traversal})
(s/def ::form any?)            ; the original source form as read by the analyzer
(s/def ::lexical-env (s/map-of (s/or :symbol symbol?       ; runtime CLJ: plain symbol keys
                                     :keyword keyword?     ; CLJS &env: keyword keys (:context, :locals, etc.)
                                     :quoted-form seq?)    ; analysis time CLJ: (quote sym) for code emission
                               any?))
;; ::partial removed — now a derived predicate, see `partial?` defmulti below.
(s/def ::id int?)              ; content-derived hash, see `compute-node-id`
;; ::symbolic-edge removed — now a fully derived accessor, see `symbolic-edge` below.
(s/def ::form-resolved any?)   ; alias-resolved form. May be a qualified keyword (::foo), qualified
                               ; symbol (clojure.core/inc), or a full call form ((my.ns/fn arg1 %)).
                               ; The type depends on the node -- this spec is intentionally open.
(s/def ::form-short any?)      ; shortest unambiguous form for display
(s/def ::resolved-runtime-value any?) ; the value obtained by resolving this node's form (e.g. the function
                                      ; object for a symbol, the var for a var ref). Only present on nodes where
                                      ; resolution produces something distinct from the form itself.
(s/def ::var (s/nilable (s/or :resolved var?    ; runtime: actual var after macro expansion
                              :symbolic seq?))) ; analysis time: code form like (var identity)
(s/def ::parameters sequential?) ; symbolic parameter list (symbols like ['db '%])
(s/def ::arguments sequential?)  ; runtime argument values provided at a call site
;; ::children removed — tree structure lives in RoseTree. Use `unwrap` to access children.
(s/def ::metadata (s/nilable set?))  ; set of keys from reader ^{...} metadata (values in top-level attrs)
(s/def ::options (s/nilable set?))   ; set of keys that came from options syntax (values in top-level attrs)
(s/def ::recursion-limit (s/or :bounded nat-int? :unbounded #{##Inf})) ; remaining recursion depth
(s/def ::multiplicity #{::one ::many}) ; cardinality of the node's result: ::one for single values, ::many for collections
(s/def ::kind #{::star ::keyword-fn}) ; syntactic origin of this node. Refines ::type: where ::type says WHAT a node
                                      ; is structurally (e.g. ::traversal, ::lambda), ::kind says HOW it was generated.
                                      ; ::star (traversals only): from `{* ...}` -- "for all children". The value at
                                      ;   point is expected to be a collection; the continuation applies to each element.
                                      ; ::keyword-fn (lambdas only): from `{:k [...]}` keyword-as-function sugar.
                                      ; Matters for the interpreter which handles star traversals differently.

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

(s/def ::node-attrs (s/multi-spec node-spec ::type))
(s/def ::node (s/and #(instance? RoseTree %)
                     #(s/valid? ::node-attrs (extract %))))

(s/def ::edge ::node) ; for traversals: the inner AST node (itself a full node
                      ; map) that describes the edge function. E.g. in
                      ; `{:user/name [...]}`, the traversal's edge is
                      ; the ::keyword node for `:user/name`. In
                      ; `{(all-users) [...]}`, the edge is a ::call node
                      ; for `(all-users)`.


(defmethod node-spec ::star [_]
  (s/keys :req [::type ::lexical-env ::id ::metadata]))

(defmethod node-spec ::recur [_]
  (s/keys :req [::type ::lexical-env ::id ::recursion-limit ::metadata]))

(defmethod node-spec ::literal [_]
  (s/keys :req [::type ::resolved-runtime-value ::lexical-env ::id ::metadata]))

(defmethod node-spec ::keyword [_]
  (s/keys :req [::type ::form ::lexical-env ::id ::form-short]))

(defmethod node-spec ::string [_]
  (s/and (s/keys :req [::type ::form ::lexical-env ::id])
         #(string? (::form %))))

(defmethod node-spec ::symbol [_]
  (s/keys :req [::type ::form ::lexical-env ::id
                ::resolved-runtime-value ::var ::form-resolved ::form-short]))

(defmethod node-spec ::var [_]
  (s/keys :req [::type ::form ::lexical-env ::id
                ::form-resolved ::form-short]
          :opt [::resolved-runtime-value]))

(defmethod node-spec ::quoted-var [_]
  (s/keys :req [::type ::form ::lexical-env ::id
                ::form-resolved ::form-short]
          :opt [::resolved-runtime-value]))

(defmethod node-spec ::pull [_]
  (s/keys :req [::type ::lexical-env ::id ::metadata]))

(defmethod node-spec ::runtime-value [_]
  (s/keys :req [::type ::form ::lexical-env ::id ::resolved-runtime-value]))

(defmethod node-spec ::quoted-symbol [_]
  (s/keys :req [::type ::form ::lexical-env ::id]))

(defmethod node-spec ::lambda [_]
  (s/keys :req [::type ::form ::lexical-env ::id
                ::parameters]
          :opt [::kind ::form-resolved ::form-short]))

(defmethod node-spec ::call [_]
  (s/keys :req [::type ::form ::lexical-env ::id
                ::resolved-runtime-value ::var ::parameters ::arguments ::form-resolved]))

(defmethod node-spec ::traversal [_]
  (s/keys :req [::type ::lexical-env ::id ::multiplicity ::metadata]
          :opt [::kind ::edge]))

(declare form)

(defmulti ^:private -form
  "Internal: return the raw source form of a node. Use `form` instead."
  (fn [node] (::type (extract node))))

(defmethod -form :default [node] (::form (extract node)))
(defmethod -form ::star [_node] '*)
(defmethod -form ::recur [node]
  (let [a (extract node), limit (::recursion-limit a)]
    (if (= ##Inf limit) '... limit)))
(defmethod -form ::literal [node] (::resolved-runtime-value (extract node)))
(defmethod -form ::quoted-var [node]
  (list 'quote (::form (extract node))))
(defmethod -form ::quoted-symbol [node]
  (list 'quote (::form (extract node))))
(defmethod -form ::pull [node]
  (mapv form (unwrap node)))
(defmethod -form ::traversal [node]
  (let [a (extract node)]
    {(form (::edge a)) (form (first (unwrap node)))}))

(defn form
  "Return the source form of `node`. Restores reader metadata via with-meta and
  wraps in options vector syntax when options are present."
  [node]
  (let [a         (extract node)
        base-form (-form node)
        opts-keys (::options a)
        meta-keys (::metadata a)]
    (if (seq opts-keys)
      ;; Has options: wrap in [form :k1 v1 :k2 v2 ...], reader meta on form
      (let [reader-keys (when meta-keys
                          (not-empty (set/difference meta-keys opts-keys)))
            form-with-meta (if (and reader-keys (has-meta? base-form))
                             (with-meta base-form (select-keys a reader-keys))
                             base-form)]
        (into [form-with-meta] (mapcat (fn [k] [k (get a k)])) opts-keys))
      ;; No options: just reader meta (existing behavior)
      (if (and (seq meta-keys) (has-meta? base-form))
        (with-meta base-form (select-keys a meta-keys))
        base-form))))

(defn compute-node-id
  "Derive a stable identity hash from a RoseTree node's semantic content.
  Two nodes with the same type, form, and display forms will share the same id.
  Used to compare structural equality across independently constructed trees --
  e.g. two cells representing the same column in the same pull will have the
  same id even if they belong to different rows.
  For pull and traversal nodes, `form` is computed from children (via unwrap)."
  [node]
  (let [a (extract node)]
    (hash (assoc (select-keys a #{::type ::form-short ::form-resolved})
                 ::form (-form node)))))

(def ^:private node-defaults
  "Default values merged with lowest precedence by `finalize`. Any key
  explicitly set by a constructor (or via metadata) takes priority."
  {::lexical-env {}, ::metadata nil})

(defn- finalize
  "Build a RoseTree node from attrs and children, compute ::id, and validate.
  Called by every constructor. `node-defaults` is merged underneath so explicit
  values always win. Returns a validated RoseTree."
  [attrs children]
  (let [rt (rose-tree (merge node-defaults attrs) children)
        id (compute-node-id rt)]
    (s/assert ::node (rose-tree (assoc (extract rt) ::id id) children))))

;;;; Constructors
;;
;; Each constructor takes essential fields as positional arguments, followed by
;; optional keyword arguments. `metadata` (metadata from the original source
;; form, defaults to nil) is merged first so its keys become part of the node
;; map. `lexical-env` defaults to `{}` via `node-defaults` in `finalize` when
;; not provided.

(defn ->star
  "Construct a star (`*`) wildcard node. Signals \"include all suggested edges\"
  inside a pull expression. Form is always `'*` (derived)."
  [& {:keys [metadata]}]
  (finalize (merge metadata {::type ::star, ::metadata (some-> metadata keys set)}) ()))

(defn- ->recur [recursion-limit & {:keys [metadata]}]
  (finalize (merge metadata {::type ::recur, ::recursion-limit recursion-limit
                              ::metadata (some-> metadata keys set)}) ()))

(defn ->bounded-recursion
  "Construct a bounded recur node (`... 5`). `limit` is the maximum recursion
  depth (a nat-int)."
  [limit & {:keys [metadata]}]
  (->recur limit :metadata metadata))

(defn ->unbounded-recursion
  "Construct an unbounded recur node (`...`). Recurses until the data is
  exhausted."
  [& {:keys [metadata]}]
  (->recur ##Inf :metadata metadata))

(defn ->literal
  "Construct a literal value node (number, boolean, nil, etc.). The form is
  derived from the value — they are the same thing for self-evaluating forms."
  [value & {:keys [metadata lexical-env]}]
  (finalize (cond-> (merge metadata {::type ::literal, ::resolved-runtime-value value,
                                      ::metadata (some-> metadata keys set)})
              lexical-env (assoc ::lexical-env lexical-env))
            ()))

(defn ->keyword
  "Construct a keyword node. Keywords are the most common edge type in pull
  expressions, e.g. `:user/name` in `[:user/name :user/email]`. Keywords
  auto-resolve at read time, so `form-resolved` and `symbolic-edge` are always
  the form itself -- accessors derive them. Only `form-short` (display alias)
  can genuinely differ."
  [form form-short & {:keys [metadata]}]
  (finalize (merge metadata {::type ::keyword, ::form form, ::form-short form-short, ::lexical-env {}})
            ()))

(defn ->string
  "Construct a string edge node. Strings name edges in map-like structures
  where keys are strings (e.g. JSON-shaped data)."
  [form & {:keys [metadata]}]
  (finalize (merge metadata {::type ::string, ::form form, ::lexical-env {}})
            ()))

(defn ->symbol
  "Construct a symbol node. Represents a bare symbol reference that resolves to a
  value at runtime. Unquoted symbols are always called (never used as map keys)."
  [form value var form-resolved form-short & {:keys [metadata lexical-env]}]
  (finalize (cond-> (merge metadata {::type ::symbol, ::form form
                                      ::resolved-runtime-value value, ::var var
                                      ::form-resolved form-resolved, ::form-short form-short})
              lexical-env (assoc ::lexical-env lexical-env))
            ()))

(defn ->var
  "Construct a var node. Represents a Clojure var reference with known parameters.
  The form is the var form itself (e.g. `#'inc`). `symbolic-edge`, `node-value`,
  and `parameters` are all derived: symbolic-edge and value are the form,
  parameters are always `'(%)`.
  NOTE: `form-resolved` and `form-short` could be derived from the var object's
  metadata (`:ns`, `:name`). This simplification is deferred to the φ-wiring
  phase, when the analyzer calls constructors directly with real var objects
  instead of symbolic `(var sym)` forms."
  [form form-resolved form-short & {:keys [metadata lexical-env resolved-runtime-value]}]
  (finalize (cond-> (merge metadata {::type ::var, ::form form
                                      ::form-resolved form-resolved, ::form-short form-short})
              resolved-runtime-value (assoc ::resolved-runtime-value resolved-runtime-value)
              lexical-env (assoc ::lexical-env lexical-env))
            ()))

(defn ->quoted-var
  "Construct a quoted-var node (`#'foo`). The var is referenced but not called;
  its value is the var object itself. `symbolic-edge` is always the form itself
  (derived, not passed)."
  [form form-resolved form-short & {:keys [metadata lexical-env resolved-runtime-value]}]
  (finalize (cond-> (merge metadata {::type ::quoted-var, ::form form
                                      ::form-resolved form-resolved, ::form-short form-short})
              resolved-runtime-value (assoc ::resolved-runtime-value resolved-runtime-value)
              lexical-env (assoc ::lexical-env lexical-env))
            ()))

(defn ->pull
  "Construct a pull node. A pull collects a set of edges (its children) from a
  map-like value. `children` are the edge AST nodes (RoseTree instances:
  keyword, star, recur, traversal, etc.). `form`, `pull-has-star?`,
  `recursion-point?`, and `recursion-limit` are all derived from children
  via `unwrap` — not passed."
  [children & {:keys [metadata lexical-env]}]
  (finalize (cond-> (merge metadata {::type ::pull, ::metadata (some-> metadata keys set)})
              lexical-env (assoc ::lexical-env lexical-env))
            children))

(defn ->runtime-value
  "Construct a runtime-value node. Represents a function or other runtime value
  passed directly to `build-hfql` (not through the `hfql` macro). The value
  will be called at runtime. `::form` and `::resolved-runtime-value` are both
  the value itself."
  [value & {:keys [metadata lexical-env]}]
  (finalize (cond-> (merge metadata {::type ::runtime-value, ::form value
                                      ::resolved-runtime-value value})
              lexical-env (assoc ::lexical-env lexical-env))
            ()))

(defn ->quoted-symbol
  "Construct a quoted-symbol node. Represents a quoted symbol used as a getter
  key (map lookup). E.g. `'some-key` in `['some-key]` looks up the symbol key
  in the map at point. `form` is the bare symbol (unquoted)."
  [form & {:keys [metadata lexical-env]}]
  (finalize (cond-> (merge metadata {::type ::quoted-symbol, ::form form})
              lexical-env (assoc ::lexical-env lexical-env))
            ()))

(defn ->lambda
  "Construct a lambda node. Represents an inline function expression. Lambda
  nodes arise from two HFQL syntactic forms:
  - Explicit: `[#(:db/id %)]` -- user-written lambda, no `kind`.
  - Keyword-as-function: `{:db/id [...]}` -- the analyzer desugars the keyword
    edge into a lambda equivalent to `#(:db/id %)`. `kind` is `::keyword-fn`.
  `node-value` is derived from the form (the lambda expression itself).
  `symbolic-edge` genuinely differs from form (it's a display summary like
  `(fn name ...)`)."
  [form parameters
   & {:keys [metadata lexical-env kind form-resolved form-short]}]
  (finalize (cond-> (merge metadata {::type ::lambda, ::form form
                                      ::resolved-runtime-value form
                                      ::parameters parameters})
              lexical-env   (assoc ::lexical-env lexical-env)
              kind          (assoc ::kind kind)
              form-resolved (assoc ::form-resolved form-resolved)
              form-short    (assoc ::form-short form-short))
            ()))

(defn ->call
  "Construct a call node. Represents a function call expression like
  `(user-by-id 42 %)`. The analyzer resolves the var, extracts parameters from
  arglists, and records the provided arguments -- i.e. all values that are
  bound at analysis time (literals, lexical bindings, etc.), as opposed to
  unbound parameters like `%` which are resolved later at runtime.
  `symbolic-edge` is derived from the form (the full call form).
  `partial?` is derived from parameters (contains `%`?)."
  [form value var parameters arguments form-resolved
   & {:keys [metadata lexical-env]}]
  (finalize (cond-> (merge metadata {::type ::call, ::form form
                                      ::resolved-runtime-value value, ::var var
                                      ::parameters (or parameters ()) ; zero-arity calls destructure to nil
                                      ::arguments (or arguments ())
                                      ::form-resolved form-resolved})
              lexical-env (assoc ::lexical-env lexical-env))
            ()))

(defn- ->traversal
  [child & {:keys [metadata lexical-env kind multiplicity edge]}]
  (finalize (cond-> (merge metadata {::type ::traversal
                                      ::multiplicity (or multiplicity ::one)
                                      ::metadata (some-> metadata keys set)})
              lexical-env (assoc ::lexical-env lexical-env)
              kind (assoc ::kind kind)
              edge (assoc ::edge edge))
            (list child)))

(defn ->star-traversal
  "Construct a star traversal node. Produced from the pair form `{* ...}`, meaning
  \"for all children\": the value at point is expected to be a collection and
  the continuation is applied to each element. Always `::many` with
  `::kind ::star`. The edge is a `::star` node -- `(form (edge node))` gives
  `'*`, so the `form` accessor reconstructs e.g. `{'* [:a]}` uniformly.
  Not to be confused with pull star (`*` inside a pull like `[:a :b *]`), which
  means \"suggest all edges\" via the Suggestable protocol. E.g. `{* inc}` on
  `(1 2 3)`, `{* [:foo]}` on `({:foo 1} {:foo 2})`.
  `child` is the single continuation AST node."
  [child & {:keys [metadata]}]
  (->traversal child :metadata metadata
    :kind ::star :multiplicity ::many :edge (->star)))

(defn ->edge-traversal
  "Construct an edge traversal node. Represents navigation from a parent value
  to a child value through a specific edge (keyword, symbol, call, etc.). The
  `edge` argument is the inner AST node (a RoseTree) describing the edge
  function -- e.g. a ::keyword node for `:user/name`, a ::call node for
  `(all-users)`. `lexical-env` should be passed explicitly by the caller;
  falls back to reading from the edge for backward compatibility. `child` is
  the single continuation AST node (a RoseTree). `form` is derived from edge
  and child. `partial?` is derived from edge."
  [edge child & {:keys [metadata multiplicity lexical-env]}]
  (->traversal child :metadata metadata :lexical-env (or lexical-env (::lexical-env (extract edge)))
    :edge edge :multiplicity multiplicity))

;;;; Accessors
;;
;; Each function reads from an AST node (a RoseTree). Use `unwrap` directly for
;; tree-structural access.

(defn node-type
  "Return the type of `node`."
  [node] (::type (extract node)))

;; `form` is defined as a wrapper function above. `-form` is the internal defmulti.

(defn form-short
  "Return the shortest unambiguous form for display."
  [node] (or (::form-short (extract node)) (-form node)))

(defn form-resolved
  "Return the alias-resolved form."
  [node] (or (::form-resolved (extract node)) (-form node)))

(defn symbolic-edge
  "Return the edge label for this node — the key under which it appears in its
  parent pull. Fully derived, not stored. Uses -form (raw form without options
  wrapping) so that options never affect the edge label."
  [node]
  (let [a (extract node)]
    (case (::type a)
      ::star nil ; star is syntax, not a named edge
      (::recur ::literal ::keyword ::string ::symbol
       ::var ::runtime-value ::call) (-form node)
      ::pull nil ; pull is a container, not an edge
      (::quoted-var ::quoted-symbol) (::form a)
      ::lambda (if (= ::keyword-fn (::kind a))
                 (or (::form-resolved a) (::form a))
                 (let [[_fn ?name] (::form a)]
                   (if (symbol? ?name) `(~'fn ~?name ~'...) '(fn anonymous ...))))
      ::traversal (symbolic-edge (::edge a)))))

(defn node-value
  "Return the runtime value of this node."
  [node]
  (let [a (extract node)]
    (if (contains? a ::resolved-runtime-value)
      (::resolved-runtime-value a)
      (case (::type a)
        (::keyword ::string ::runtime-value ::var ::lambda ::quoted-var ::quoted-symbol) (::form a)
        nil))))

(defn node-var
  "Return the Clojure var backing this node, if any."
  [node] (::var (extract node)))

(defn parameters
  "Return the symbolic parameter list of this node."
  [node]
  (let [a (extract node)]
    (case (node-type node)
      (::var ::symbol ::runtime-value) '(%)
      (::parameters a))))

(defn arguments
  "Return the runtime argument values provided at a call site."
  [node] (::arguments (extract node)))

(defn lexical-env
  "Return the lexical environment captured at analysis time."
  [node] (::lexical-env (extract node)))

(defn node-id
  "Return the content-derived identity hash of this node."
  [node] (::id (extract node)))

(defn edge
  "Return the inner AST node (a RoseTree) describing a traversal's edge function."
  [node] (::edge (extract node)))

(defn kind
  "Return the syntactic origin tag for traversal and lambda nodes."
  [node] (::kind (extract node)))

(defn multiplicity
  "Return the cardinality of this traversal node's result."
  [node] (::multiplicity (extract node)))

(defn recursion-limit
  "Return the remaining recursion depth. On recur nodes: stored directly.
  On parent nodes (pull, traversal): derived from the recur child via `unwrap`.
  `##Inf` for unlimited, nat-int for bounded, nil when not recursive."
  [node]
  (let [a (extract node)]
    (or (::recursion-limit a)
        (some (fn [c] (when (= ::recur (::type (extract c)))
                        (::recursion-limit (extract c))))
              (unwrap node)))))

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
(defn runtime-value? "True if `node` is a runtime value (e.g. fn passed to build-hfql)." [node] (= ::runtime-value (node-type node)))
(defn quoted-symbol? "True if `node` is a quoted symbol (`'foo`), used as a getter key." [node] (= ::quoted-symbol (node-type node)))
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

(defn pull-has-star?
  "True if this pull node's children include a star wildcard."
  [node] (boolean (some star? (unwrap node))))

(defn recursion-point?
  "True if this node has a recur child (target of `...` recursion). The
  interpreter uses this to find where to loop back when unfolding recursive
  structures. Recurses through star-traversal children so that
  `{edge {* ...}}` patterns are recognized."
  [node] (boolean (or (some recur-node? (unwrap node))
                      (when-let [child (first (unwrap node))]
                        (and (star-traversal? child) (recursion-point? child))))))

(defn bounded-recursion?
  "True if this recur node has a finite recursion limit (e.g. `... 5`)."
  [node] (and (recur-node? node) (not= ##Inf (recursion-limit node))))

(defn unbounded-recursion?
  "True if this recur node has unlimited recursion (`...`)."
  [node] (and (recur-node? node) (= ##Inf (recursion-limit node))))

;; partial? is a multimethod — most nodes are always partial (they expect `%`).
;; Calls derive it from parameters. Traversals derive from their edge.
(defmulti partial?
  "True if this node still expects the `%` argument. Derived from node type
  and content — not stored. Most nodes are always partial. Calls derive from
  their parameters. Traversals derive from their edge."
  node-type)
(defmethod partial? :default [_] true)
(defmethod partial? ::call [node] (boolean (some #{'%} (::parameters (extract node)))))
(defmethod partial? ::traversal [node] (partial? (edge node)))

(defn leaf?
  "True if this node is terminal — either it has no children (via unwrap),
  or its recursion limit has reached 0."
  [node] (boolean (or (empty? (unwrap node))
                      (some-> (recursion-limit node) zero?))))


;;;; Mutators
;;
;; For keys set by the interpreter at runtime, outside of the static AST
;; produced by phi. These are not part of the constructor signatures because
;; they are discovered during tree traversal, not analysis.
;; All mutators work on RoseTree nodes with `mapHead` baked in.

(defn mark-recursion-root
  "Mark this node as the root of a recursive structure. Set by the interpreter
  when it detects a `...` recursion and identifies which ancestor is the
  recursion target."
  [node] (mapHead #(assoc % ::recursion-root true) node))

(defn recursion-root?
  "True if this node has been marked as a recursion root by the interpreter."
  [node] (boolean (::recursion-root (extract node))))

(defn mark-recursive-child
  "Mark this node as a child produced by recursive unfolding. Used by the
  navigator UI to filter out recursive children from column pickers."
  [node] (mapHead #(assoc % ::recursive-child true) node))

(defn recursive-child?
  "True if this node was produced by recursive unfolding."
  [node] (boolean (::recursive-child (extract node))))

(defn dec-recursion-limit
  "Decrement the recursion limit by one, clamping at zero. Called by the
  interpreter each time it unfolds one level of a recursive structure. When the
  limit reaches zero, recursion stops and the node becomes a leaf."
  [node] (mapHead #(update % ::recursion-limit (fn [l] (max 0 (dec l)))) node))

(defn set-metadata
  "Set the metadata on a node. Metadata keys (e.g. :line, :column) are merged
  underneath existing attrs so they never shadow core AST keys. ::metadata
  stores the key set (values live in top-level attrs)."
  [node metadata]
  (mapHead #(merge metadata % {::metadata (some-> metadata keys set)}) node))

(defn set-options
  "Record options-syntax key-value pairs on a node. Values are merged underneath
  existing attrs. ::options records which keys came from options syntax (for form
  reconstruction). Option keys are also added to ::metadata's key set."
  [node options]
  (if (seq options)
    (mapHead (fn [a]
               (merge options a
                 {::metadata (into (or (::metadata a) #{}) (keys options))
                  ::options  (set (keys options))}))
             node)
    node))

(defn set-lexical-env
  "Set the lexical environment on a node."
  [node lexical-env]
  (mapHead #(assoc % ::lexical-env lexical-env) node))

;;;; Tests

;; Helper: all constructed nodes must pass the ::node spec.
(defn- valid-node? [node] (s/valid? ::node node))

(tests
 "->star"
 (let [n (->star)]
   (valid-node? n) := true
   (star? n) := true
   (form n) := '*
   (node-value n) := nil
   (partial? n) := true
   ))

(tests
 "->star with metadata"
 (let [n (->star :metadata {:line 42})]
   (star? n) := true
   (:line (extract n)) := 42))

(tests
 "->unbounded-recursion"
 (let [n (->unbounded-recursion)]
   (valid-node? n) := true
   (recur-node? n) := true
   (form n) := '...
   (unbounded-recursion? n) := true
   (bounded-recursion? n) := false
   (node-value n) := nil
   (recursion-limit n) := ##Inf
   (recursion-limit (dec-recursion-limit n)) := ##Inf
   ))

(tests
  "->bounded-recursion"
  (let [n (->bounded-recursion 3)]
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
 (let [n (->literal 42)]
   (valid-node? n) := true
   (literal? n) := true
   (node-value n) := 42
   (node-value (->literal nil)) := nil ; form IS the value, even when nil
   (symbolic-edge n) := 42
   (partial? n) := true
   ))

(tests
 "->keyword"
 (let [n (->keyword :db/ident :ident)]
   (valid-node? n) := true
   (keyword-node? n) := true
   (form n) := :db/ident
   (symbolic-edge n) := :db/ident
   (node-value n) := :db/ident
   (form-resolved n) := :db/ident
   (form-short n) := :ident
   (partial? n) := true
   ))

(tests
 "->string"
 (let [n (->string "name")]
   (valid-node? n) := true
   (string-node? n) := true
   (form n) := "name"
   (symbolic-edge n) := "name"
   (node-value n) := "name"
   (partial? n) := true
   ))

(tests
 "->symbol (non-fn value)"
 (let [n (->symbol 'x 42 nil 'my.ns/x 'x)]
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
  "->symbol (fn value) stays ::symbol"
  (let [n (->symbol 'inc inc #'inc 'clojure.core/inc 'inc)]
    (valid-node? n) := true
    (symbol-node? n) := true
    (node-type n) := ::symbol
    (node-value n) := inc
    (node-var n) := #'inc
    (form-resolved n) := 'clojure.core/inc
    (form-short n) := 'inc
    (partial? n) := true
    ))

(tests
 "->var"
 (let [n (->var #'inc '(var clojure.core/inc) '(var inc) :resolved-runtime-value #'inc)]
   (valid-node? n) := true
   (var-node? n) := true
   (node-value n) := #'inc
   (parameters n) := '(%)
   (form-resolved n) := '(var clojure.core/inc)
   (form-short n) := '(var inc)
   (partial? n) := true
   ))

(tests
 "->quoted-var"
 (let [n (->quoted-var '#'inc '(var clojure.core/inc) '(var inc) :resolved-runtime-value #'inc)]
   (valid-node? n) := true
   (quoted-var? n) := true
   (symbolic-edge n) := '#'inc
   (node-value n) := #'inc
   (form-resolved n) := '(var clojure.core/inc)
   (form-short n) := '(var inc)
   (partial? n) := true
   ))

(tests
 "->pull"
 (let [a (->keyword :a :a)
       b (->keyword :b :b)
       n (->pull (list a b))]
   (valid-node? n) := true
   (pull? n) := true
   (form n) := [:a :b]
   (symbolic-edge n) := nil
   (unwrap n) := (list a b)
   (leaf? n) := false
   (node-value n) := nil
   (pull-has-star? n) := false
   (recursion-point? n) := false
   (partial? n) := true
   ))

(tests
 "->pull with star child"
 (let [a (->keyword :a :a)
       s (->star)
       n (->pull (list a s))]
   (pull-has-star? n) := true
   (form n) := [:a '*]
   ))

(tests
 "->pull with recursion child"
 (let [a (->keyword :a :a)
       r (->unbounded-recursion)
       n (->pull (list a r))]
   (recursion-point? n) := true
   (recursion-limit n) := ##Inf
   ))

(tests
 "->runtime-value"
 (let [n (->runtime-value identity)]
   (valid-node? n) := true
   (runtime-value? n) := true
   (node-value n) := identity
   (partial? n) := true
   ))

(tests
 "->quoted-symbol"
 (let [n (->quoted-symbol 'foo)]
   (valid-node? n) := true
   (quoted-symbol? n) := true
   (form n) := '(quote foo)
   (node-value n) := 'foo
   (symbolic-edge n) := 'foo
   (partial? n) := true
   ))

(tests
 "->lambda explicit (no kind)"
 (let [n (->lambda '(fn [%] (:db/id %)) '(%))]
   (valid-node? n) := true
   (lambda? n) := true
   (node-value n) := '(fn [%] (:db/id %))
   (parameters n) := '(%)
   (kind n) := nil
   (partial? n) := true
   ))

(tests
 "->lambda keyword-fn"
 (let [n (->lambda '(fn [%] (:db/id %)) '(%)
                   :kind ::keyword-fn :form-resolved :db/ident :form-short :ident)]
   (valid-node? n) := true
   (kind n) := ::keyword-fn
   (form-resolved n) := :db/ident
   (form-short n) := :ident
   ))

(tests
 "->call"
 (let [n (->call '(+ 42 a %) + #'+ '(42 a %) '(42 1 '%) 'clojure.core/+
                 :lexical-env {'a 1})]
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
 (let [n (->call '(inc 42) inc #'inc '(x) '(42) 'clojure.core/inc)]
   (valid-node? n) := true
   (call? n) := true
   (partial? n) := false
   ))

(tests
 "->star-traversal"
 (let [child (->runtime-value 'inc)
       n (->star-traversal child)]
   (valid-node? n) := true
   (traversal? n) := true
   (star-traversal? n) := true
   (form n) := {'* 'inc}
   (kind n) := ::star
   (multiplicity n) := ::many
   (node-value n) := nil
   (star? (edge n)) := true
   (unwrap n) := (list child)
   (leaf? n) := false
   (lexical-env n) := {}
   (partial? n) := true
   ))

(tests
 "->star-traversal with recursion"
 (let [r (->unbounded-recursion)
       n (->star-traversal r)]
   (valid-node? n) := true
   (traversal? n) := true
   (star-traversal? n) := true
   (form n) := {'* '...}
   (kind n) := ::star
   (multiplicity n) := ::many
   (recursion-point? n) := true
   (recursion-limit n) := ##Inf
   (lexical-env n) := {}
   (partial? n) := true
   ))

(tests
 "->edge-traversal"
 (let [kw-edge (->keyword :user/name :name)
       child (->pull (list (->keyword :x :x)))
       n (->edge-traversal kw-edge child)]
   (valid-node? n) := true
   (traversal? n) := true
   (form n) := {:user/name [:x]}
   (multiplicity n) := ::one
   (kind n) := nil
   (edge n) := kw-edge
   (unwrap n) := (list child)
   (leaf? n) := false
   (lexical-env n) := (lexical-env kw-edge)
   (identical? (lexical-env n) (lexical-env kw-edge)) := true
   ))

(tests
 "->edge-traversal with unbounded recursion"
 (let [kw-edge (->keyword :children :children)
       r (->unbounded-recursion)
       n (->edge-traversal kw-edge r)]
   (valid-node? n) := true
   (form n) := {:children '...}
   (recursion-point? n) := true
   (recursion-limit n) := ##Inf))

(tests
 "->edge-traversal with bounded recursion"
 (let [kw-edge (->keyword :children :children)
       r (->bounded-recursion 3)
       n (->edge-traversal kw-edge r)]
   (valid-node? n) := true
   (form n) := {:children 3}
   (recursion-point? n) := true
   (recursion-limit n) := 3))

(tests
 "->edge-traversal with star-traversal continuation containing recursion"
 (let [kw-edge (->keyword :children :children)
       r (->unbounded-recursion)
       star (->star-traversal r)
       n (->edge-traversal kw-edge star :multiplicity ::many)]
   (recursion-point? n) := true))

(tests
 "form-short returns ::form-short when present"
 (form-short (->keyword :db/ident :ident)) := :ident)

(tests
 "form-short falls back to ::form"
 (form-short (->literal 42)) := 42
 (form-short (->star)) := '*
 (form-short (->string "x")) := "x")

(tests
 "form-resolved returns ::form-resolved when present"
 (form-resolved (->symbol 'x 42 nil 'my.ns/x 'x)) := 'my.ns/x)

(tests
 "form-resolved falls back to ::form"
 (form-resolved (->literal 42)) := 42
 (form-resolved (->star)) := '*)

(tests
 "star?"
 (star? (->star)) := true
 (star? (->literal 42)) := false)

(tests
 "leaf? is true for nodes without children"
 (leaf? (->keyword :a :a)) := true
 (leaf? (->star)) := true
 (leaf? (->unbounded-recursion)) := true)

(tests
 "leaf? is false when children present"
 (leaf? (->pull (list (->keyword :x :x)))) := false)

(tests
 "leaf? is true when recursion-limit reaches 0"
 (let [r (->bounded-recursion 0)
       n (->pull (list (->keyword :a :a) r))]
   (recursion-limit n) := 0
   (leaf? n) := true))

(tests
 "mark-recursion-root"
 (let [a (->keyword :a :a)
       n (->pull (list a))
       n' (mark-recursion-root n)]
   (pull? n) := true
   (pull? n') := true
   (form n) := (form n')
   (recursion-root? n) := false
   (recursion-root? n') := true
   ))

(tests
 "mark-recursive-child"
 (let [child (->runtime-value 'inc)
       n (->star-traversal child)
       n' (mark-recursive-child n)]
   (star-traversal? n) := true
   (star-traversal? n') := true
   (= (form n) (form n')) := true
   (recursive-child? n) := false
   (recursive-child? n') := true
   ))

(tests
 "compute-node-id: same content produces same id"
 (let [a (->keyword :db/id :id)
       b (->keyword :db/id :id)]
   (node-id a) := (node-id b)))

(tests
 "compute-node-id: different content produces different id"
 (let [a (->keyword :db/id :id)
       b (->keyword :db/name :name)]
   (not= (node-id a) (node-id b)) := true))

(tests
 "compute-node-id: different types, same form -> different id"
 (let [a (->literal "hello")
       b (->string "hello")]
   (not= (node-id a) (node-id b)) := true))

(tests
 "multiplicity is ::one by default"
 (let [kw (->keyword :a :a)
       child (->pull (list (->keyword :x :x)))]
   (multiplicity (->edge-traversal kw child)) := ::one))

(tests
 "->star-traversal is always ::many"
 (let [child (->runtime-value 'inc)]
   (multiplicity (->star-traversal child)) := ::many))

(tests
 "form carries metadata from metadata"
 (let [meta-map {:line 5 :column 3}
       a (->keyword :a :a)
       n (->pull (list a) :metadata meta-map)]
   (meta (form n)) := meta-map)
 (let [meta-map {:line 10}
       kw (->keyword :a :a)
       child (->keyword :x :x)
       n (->edge-traversal kw child :metadata meta-map)]
   (meta (form n)) := meta-map))


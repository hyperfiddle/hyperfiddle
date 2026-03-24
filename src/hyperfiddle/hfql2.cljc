(ns hyperfiddle.hfql2
  (:refer-clojure :exclude [format])
  (:require
   [clojure.core :as cc]
   [clojure.core.protocols :as ccp]
   [clojure.datafy]
   [clojure.pprint :refer [cl-format]]
   [clojure.set :as set]
   [dustingetz.data :refer [iterator? enumeration? array? to-seq]]
   [dustingetz.str :as strx]
   [hyperfiddle.rcf :refer [tests]]
   [hyperfiddle.ct.monoid :refer [mappend]]
   [hyperfiddle.ct.functor :refer [fmap]]
   [hyperfiddle.ct.function :refer [flip]]
   [hyperfiddle.ct.comonad :refer [extract]]
   [hyperfiddle.ct.monad :refer [bind =<<]]
   [hyperfiddle.ct.cofree :refer [tree-seqT unwrap runCofreeT headF tailF ->CofreeF]]
   [hyperfiddle.ct.rose-tree :refer [rose-tree mapHead]]
   [hyperfiddle.ct.store :as store :refer [pos peeks]]
   [hyperfiddle.ct.experiments.thunk :refer [force-thunk]]
   [hyperfiddle.ct.either :refer [either either? left right unwrap! from-right from-left]]
   [hyperfiddle.hfql2.analyzer :as ana]
   [hyperfiddle.hfql2.ast :as ast]
   [hyperfiddle.hfql2.interpreter :as co :refer [unfoldT hfql-coalgebra hoistHfqlT]]
   [hyperfiddle.hfql2.protocols :as hfqlp :refer [Identifiable Suggestable ComparableRepresentation Datafiable -hfql-resolve -resolve-dispatch
                                                  Searchable -search-matches *search-max-collection-size* *search-max-str-len*]]
   [hyperfiddle.hfql2.utils :refer [has-meta? imap? original-object]]
   #?(:cljs [goog.string :as gstring])
   #?(:cljs [goog.string.format])
   #?(:cljs [cljs.stacktrace :as cljs-stacktrace])
   #?(:cljs [goog.userAgent.product :as user-agent-product]))
  #?(:cljs (:require-macros [hyperfiddle.hfql2 :refer [hfql defpointfree]])))

(defmacro defpointfree
  "Like (def foo (comp a b)) but improve stacktraces by printing `foo` instead of comp_XYZ."
  {:clj-kondo/lint-as 'clojure.core/def}
  [name f]
  `(defn ~name [point#] (~f point#)))

;; ── Human API ────────────────────────────────────────────────────────
;; Functions for HFQL users writing queries, sitemaps, and REPL exploration.
;; Stable contract.
;;
;; Defined here:  *limit*, *default*, seed, nav*, nav, pull*, pull,
;;                next-steps, values, scope, value-at-point, hfql?,
;;                navigable, hfql*, hfql
;; Defined below (load-order dependencies):
;;                filtered, filtered?, union, find-child, find-child-by,
;;                find-cell-by-symbolic-column, hfql-resolve,
;;                identifiable, identify, suggest, comparable

(def ^{:dynamic true, :doc "Collection size cap for fan-out traversals. Bind to an integer to limit how many children HFQL materializes.
  (binding [*limit* 10] (hfql [*] my-big-seq))
"}
  *limit* nil)

(def ^{:dynamic true, :doc "Default value substituted when a key is absent.
  (binding [*default* :n/a] (hfql [:missing-key] {}))
"}
  *default* nil)

(def ^{:doc "Inject data into an HFQL tree by binding scope variables.
  (seed {'% my-data} (hfql [:a :b]))
  (seed {'% root 'page 2} (hfql (list-items % page))
"}
  seed co/seed)

(defpointfree ^{:doc "Navigate one step forward by applying the current HFQL query's step onto the current scope. Return the computed value as an Either (Right value | Left error).
  (nav* (hfql inc 41)) := (Right 42)
  (nav* (hfql inc \"hello\") := (Left #error...)
"}
  nav* (comp force-thunk extract))

(defpointfree ^{:doc "Navigate one step forward by applying the current HFQL query's step onto the current scope. Return the computed value, throwing on error.
  (nav (hfql inc 41)) := 42
  (nav (hfql inc \"hello\") := throws #error

  Not to be confused with clojure.core.protocols/nav. TODO disambiguate
"}
  nav (comp original-object unwrap! nav*))

(defpointfree ^{:doc "Eagerly pull the full tree as an Either.
  (pull* (hfql [:a {:b [:c]}] x)) := (Right {:a 1, :b {:c 2}})
"}
  pull* (comp extract co/pull))

(defpointfree ^{:doc "Eagerly pull the full tree into plain Clojure data, throwing on error.
  (pull (hfql [:a {:b [:c]}] x)) := {:a 1, :b {:c 2}}
"}
  pull (comp force-thunk pull*))

(defpointfree ^{:doc "Return immediate children of an HFQL point as a non-chunked lazy sequence.
  (next-steps (hfql [inc dec] 42)) := (list (hfql inc 42) (hfql dec 42))
  (next-steps (hfql {* inc} (range))) := (list (hfql inc 1) (hfql inc 2) (hfql inc 3) ...) ; infinite
"}
  next-steps unwrap)

(defpointfree ^{:doc "Flat, lazy pre-order traversal of all values in the tree.
  (values (hfql [{:a inc}] {:a 41})) := '({:a 41} 41 42)
  (values (hfql {inc ...} 0)) := '(0 1 2 3 4 ...)
"}
  values (comp (fmap (comp force-thunk nav)) tree-seqT))

(defpointfree ^{:doc "Return the environment map (scope) of an HFQL point.
  (scope (seed (hfql (list-items % page)) {'% my-data, 'page 2})) := {'% my-data, 'page 2}
  (scope (first (next-steps (hfql [:a] {'% my-data, 'page 2})) := {'% my-data, 'page 2}
"}
  scope (comp pos runCofreeT))

(defpointfree ^{:doc "Return the raw value bound to '% at the current point.
  (value-at-point (hfql inc 42)) := (Right 42)
"}
  value-at-point (comp force-thunk '% scope))

(defpointfree ^{:doc "Predicate: is this value an HFQL object?
  (hfql? (hfql identity 1)) := true
"}
  hfql? co/hfql?)

(declare unchunk-seq)
(defn navigable
  "Attach a custom navigation function to a collection via metadata, controlling how HFQL resolves individual items of the collection.
  (navigable (fn [index entity-id] (d/entity *db* entity-id)) '(42 43 44))"
  [nav-fn object]
  (vary-meta (unchunk-seq object) assoc `hyperfiddle.hfql2.protocols/-nav (#?(:clj bound-fn, :cljs fn) [_coll key value] (nav-fn key value))))

(defn hfql*
  "Build an HFQL tree from a pre-analyzed AST. Useful to build or edit HFQL
  expressions at runtime. Prefer the `hfql` macro for normal use."
  [analyzed-hfql] (unfoldT hfql-coalgebra analyzed-hfql))

(defmacro hfql
  "Compile an HFQL query into a navigable object, optionally seeding it with data.
  (hfql inc)                ; can't be navigated yet: missing a seed value.
  (hfql inc 1)              ; ready to navigate
  (seed {'% 1} (hfql inc))  ; equivalent
"
  ([hfql-form] `(hfql* (ana/hfql ~hfql-form)))
  ([hfql-form x] `(seed {~''% (right ~x)} (hfql ~hfql-form))))

;; ── Internal Predicates & Helpers ────────────────────────────────────
;; Structural predicates and framework plumbing used by interpreter.cljc,
;; navigator6, and hfql2 internals. Public but for advanced usage only.

;; AST type predicates (lifted through HFQLT metadata)
(defpointfree pull-has-star?   (comp ast/pull-has-star?   meta))
(defpointfree partial?         (comp ast/partial?         meta))
(defpointfree pull?            (comp ast/pull?            meta))
(defpointfree traversal?       (comp ast/traversal?       meta))
(defpointfree star-traversal?  (comp ast/star-traversal?  meta))
(defpointfree star?            (comp ast/star?            meta))
(defpointfree node-type         (comp ast/node-type         meta))
(defpointfree recursion-point?  (comp ast/recursion-point?  meta))
(defpointfree recursive-child?  (comp ast/recursive-child?  meta))
(defpointfree traversal-only-recursion? (comp co/traversal-only-recursion? meta))

;; Scope helpers
(defpointfree seeded? (comp (flip contains? '%) scope))
(defn update-scope "Like `clojure.core/update` for the current HFQL scope."
  [stepT f & args]
  (seed (apply f (scope stepT) args) stepT))

(defpointfree ^:no-doc static-next-steps (comp ::co/static-children meta)) ; unseeded children, no IO. Structural shape fallback for empty collections.
(defn unbound-params
  "Return the sequence of parameter symbols that are unbound for this HFQL point.
   For call nodes: simple symbols absent from lexical-env.
   For all others: (%) — the universal point-value convention.
   Purely static — derived from the AST, not the scope. Safe to call reactively
   without creating a dependency on seed/scope changes."
  [hfqlT]
  (let [ast (meta hfqlT)
        ;; Traversals delegate partiality to their edge (same as partial?)
        ast (if (ast/traversal? ast) (ast/edge ast) ast)]
    (if (ast/call? ast)
      (let [attributes (extract ast)
            lenv (::ast/lexical-env attributes)]
        (filter (fn [p] (and (simple-symbol? p)
                             (not (or (contains? lenv p)
                                      (contains? lenv (list 'quote p))))))
                (::ast/parameters attributes)))
      '(%))))

(defn form-parameters
  "Ordered parameter descriptors for rendering the parameter form.
   Each entry: {:name sym, :free? bool, :display-value str-or-nil}.
   Call nodes: one per simple-symbol arg, preserving call order.
   Non-call nodes: [{:name '%, :free? true}].
   Bound params include :display-value — the default value stringified.
   Resolved by calling the expr fn with empty scope (falls back to the
   captured default: var value for globals, lexical value for let-bindings)."
  [hfqlT]
  (let [ast (meta hfqlT)
        ;; Traversals delegate partiality to their edge (same as unbound-params)
        ast (if (ast/traversal? ast) (ast/edge ast) ast)]
    (if (ast/call? ast)
      (let [attributes (extract ast)
            lenv (::ast/lexical-env attributes)
            params (::ast/parameters attributes)
            ;; ::arguments are compiled expr fns at runtime (fn [env] → value).
            ;; Never nil for call nodes — each param gets an (expr p) fn from the analyzer.
            args (::ast/arguments attributes)]
        (into []
              (comp (filter (fn [[p _]] (simple-symbol? p)))
                    (map (fn [[p arg-fn]]
                           (let [free? (not (or (contains? lenv p)
                                                (contains? lenv (list 'quote p))))]
                             (cond-> {:name p :free? free?}
                               ;; Bound params: call expr fn with empty scope to get default
                               (not free?)
                               (assoc :display-value
                                 #?(:clj (try (pr-str (arg-fn {}))
                                              (catch Throwable _ (str p)))
                                    :cljs (str p))))))))
              (map vector params args)))
      [{:name '% :free? true}])))

(defn form-defaults
  "Symbolic default parameter values for form initialization.
   Reads ::hfql/form-defaults metadata — a fn {sym nil} → {sym symbolic-value}.
   Returns serializable {sym value} or nil. Caller should ensure DI bindings
   are active (form-defaults fn may access DI vars for computation)."
  [stepT]
  (let [ast (meta stepT)
        ;; Traversals delegate form-defaults to their edge (same as form-parameters)
        ast (if (ast/traversal? ast) (ast/edge ast) ast)
        form-defaults-fn (::form-defaults (extract ast))]
    (when form-defaults-fn
      ;; Pass nil for all params — form starts empty, form-defaults fills in
      ;; symbolic values (e.g. '{:args [*db*]}).
      (let [params  (mapv :name (form-parameters stepT))
            nil-map (into {} (map (fn [sym] [sym nil])) params)]
        (try (form-defaults-fn nil-map)
             (catch #?(:clj Throwable :cljs :default) _ nil))))))

(defn has-form-defaults?
  "True if this HFQL point declares form-defaults metadata.
   Lightweight presence check — does not call the defaults fn."
  [stepT]
  (let [ast (meta stepT)
        ;; Traversals delegate to their edge (same pattern as form-defaults, form-parameters)
        ast (if (ast/traversal? ast) (ast/edge ast) ast)]
    (some? (::form-defaults (extract ast)))))

(defn fullfilled?
  "True if an HFQL point is ready to be navigated — all unbound parameters
   have values in scope. Non-partial steps are always fulfilled."
  [hfqlT]
  (or (not (partial? hfqlT))
      (let [point-scope (scope hfqlT)]
        (every? #(contains? point-scope %) (unbound-params hfqlT)))))

;; Search/filter tagging (filtered, filtered? are Human API — defined here for load order)
(defn- safe-vary-meta [object f & args]
  (if (has-meta? object)
    (apply vary-meta object f args)
    object))

(defn filtered
  "Mark a collection as having been filtered (e.g. by custom userland query).
HFQL will disable automatic in-memory search:
  - to respect user's custom filtering,
  - so not to filter twice (perfs).
  The collection must support metadata.
"
  [coll] (safe-vary-meta coll assoc ::filtered true))

(defn filtered?
  "Predicate: has this collection been marked as filtered?"
  [object] (::filtered (meta object)))

;; HFQL construction (non-macro paths)
(defn build-hfql
  "Advanced use case: build an HFQL object at runtime. Please use the `hfql` macro instead."
  ([hfql-expression] (build-hfql {} hfql-expression))
  ([&env hfql-expression]
   (unfoldT hfql-coalgebra (ana/analyze (or &env {}) hfql-expression))))

(tests
  (set! *print-namespace-maps* false)
  )

;;; tests
(tests
  "Literal getter"
  (values (seed {'% {:a 1, :b 2}} (hfql :a)))
  := '(1)

  (values (seed {'% {:a 1, :b 2, 'c 3}} (hfql 'c)))
  := '(3)

  (values (seed {'% {:a 1, :b 2, 'c 3, "d" 4}} (hfql "d")))
  := '(4))

(tests
  "Quoted symbol always looks up."
  (values (seed {'% {'str 'str, str str, #'str #'str}} (hfql 'str)))
  := '(str)

  "Unquoted symbol always calls (even if fn appears as key in map)."
  (values (seed {'% {'str 'str, str str, #'str #'str}} (hfql str)))
  := (list (str {'str 'str, str str, #'str #'str}))

  "Quoted var is always looked up as a literal."
  (values (seed {'% {'str 'str, str str, #'str #'str}} (eval '(hfql '#'str)))) ; HACK eval is an RCF workaround. RCF chocked on :the-var form.
  := [#'clojure.core/str])

(tests
  "Unquoted symbol always calls — no ambiguity."
  ;; Symbols are always called. To look up, use:
  ;; - 'f for symbol keys
  ;; - #'f for var keys (calling)
  ;; - '#'f for var keys (lookup)
  (values (seed {'% {'str 'str, :str :str #'str #'str}} (hfql str)))
  := '("{str str, :str :str, #'clojure.core/str #'clojure.core/str}"))

(tests
  "Var always mean “apply function”."
  (values (seed {'% {'str 'str, str str, #'str #'str}} (eval '(hfql #'str)))) ; HACK eval is an RCF workaround. RCF chocked on :the-var form.
  := '("{str str, #function[clojure.core/str] #function[clojure.core/str], #'clojure.core/str #'clojure.core/str}")
  )


(tests
  "missing literals"

  (values (seed {'% {str str, #'str #'str}} (hfql 'str)))
  := '(nil)

  (values (seed {'% {'str 'str, str str}} (eval '(hfql '#'str)))) ; HACK eval is an RCF workaround. RCF chocked on :the-var form.
  := '(nil)
  )



(tests
  "manual step-by-step navigation"
  (hfql {(range 3) {* inc}})
  (nav *1)
  := '(0 1 2)
  (next-steps *2)
  (count *1)
  := 3
  (map nav *2)
  := '(1 2 3))

(tests
  "Sequencing pre-order traversal"
  (values (hfql {(range 3) {* inc}}))
  := '((0 1 2) 1 2 3)

  (values (hfql {(range 3) {* {inc inc}}}))
  := '((0 1 2) 1 2 2 3 3 4)
  )


(tests ; FIXME not sure what to do here ATM. Will revisit after Datomic Browser is done, Datomic Browser has higher priority.
  ;; Goal: navigate through object graph at the REPL
  ;; Problem: in a grid UI we want scalars at leaves, not objects
  ;; technical issue: HFQL bypasess `ccp/nav` at expression leaves, causing this test to fail.
  ;; Questions:
  ;; - should this test be changed?
  ;; - should HFQL be changed and use another strategy to navigate/not navigate a leaves?
  (def _attribute  {:db/id 42, :db/ident :attribute/name})
  (def _object (with-meta {:a 42} {`ccp/nav (fn [o k v] (if (= :a k) _attribute (get o k v)))}))
  (nav (seed {'% _object} (hfql :a)))
  := _attribute

  (values (seed {'% _object} (hfql {:a :db/ident})))
  := '({:db/id 42, :db/ident :attribute/name} :attribute/name)

  )


(tests
  "pull syntax"
  (values (seed {'% {:a 1}} (hfql []) )) ; [] ≅ [*]
  := '({:a 1} 1)

  (values (seed {'% {:a 1, :b 2}} (hfql [:a])))
  := '({:a 1} 1)

  (values (seed {'% {:a 1, :b 2}} (hfql [:a :c])))
  := '({:a 1, :c nil} 1 nil)

  (values (seed '{% {:a 1, :b 2}}  (hfql [:a :c str])))
  := '({:a 1, :c nil, str "{:a 1, :b 2}"} 1 nil "{:a 1, :b 2}")

  )

(tests
  "star traversal"
  (values (seed {'% [{:a 1} {:a 2}]} (hfql {identity {* :a}})))
  := '([{:a 1} {:a 2}] 1 2)

  ;; A tree must have a single root. So in this special “top-level *” case, * returns identity and fans-out.
  (values (seed {'% [{:a 1} {:a 2}]} (hfql {* :a})))
   := '([{:a 1} {:a 2}] 1 2)
   )


(tests
  "Manual navigation"
  (let [alice {:db/id 9, :user/name "Alice" :user/email "alice@example.com"}
        step (seed {'% alice} (hfql {identity [:user/name :user/email]}))]

    (nav step)
    := alice

    (force-thunk (nav (first (next-steps step)))) ; FIXME computing next-steps should not require `step` to be seeded.
    := {:user/name "Alice", :user/email "alice@example.com"}

    (map nav (-> step next-steps first next-steps))
    := ["Alice" "alice@example.com"]
    ))


(tests
  (pull (seed {'% {:a 1}} (hfql :a)))
  := 1
  (pull (seed {'% {:a 1, :b 2}} (hfql [:a])))
  := {:a 1}
  (pull (seed {'% {:a 1, :b 2}} (hfql {identity [:a]})))
  := '{identity {:a 1}}
  (pull (seed {'% [{:a 1, :b 2} {:a 3, :c 4}]} (hfql {* [:a]}))) ; This is how we express pull-many.
  := '({:a 1} {:a 3})

  (pull (seed {'% [{:a 1, :b 2} {:a 3, :c 4}]} (hfql {identity {* [:a]}})))
  := '{identity ({:a 1} {:a 3})}

  (pull (seed {'% [{:a 1, :b 2} {:a 3, :c 4}]} (hfql {identity {* {identity [:a]}}})))
  := '{identity ({identity {:a 1}} {identity {:a 3}})}

  (defn _ns-publics [ns] (take 2 (ns-publics ns)))
  (pull  (hfql {(_ns-publics 'clojure.core) {* {val symbol}}}))
  := '{(_ns-publics clojure.core) ({val clojure.core/primitives-classnames} {val clojure.core/+'})}

  (pull (hfql {(_ns-publics 'clojure.core) {* {val [symbol]}}}))
  := '{(_ns-publics clojure.core) ({val {symbol clojure.core/primitives-classnames}}
                                   {val {symbol clojure.core/+'}})}

  )

(tests
  (pull (seed '{% {:a {:db/id 1 :db/ident :foo}}} (hfql [:a {:a :db/ident}])))
  := {:a :foo}
  )




(def ^:private _alice_    {:db/id 9, :user/email "alice@example.com", :user/name "Alice" :user/address {:address/number 42}})
(def ^:private _bob_      {:db/id 10, :user/email "bob@example.com", :user/name "Bob"})
(def ^:private _charlie_  {:db/id 11, :user/email "chalie@example.com", :user/name "Charlie"})
(def ^:private _db_       ^:symbolic {9 _alice_, 10 _bob_, 11 _charlie_})
(def ^:private _d-entity_ (fn [db eid] (get db eid)))
(defn- _people_ []
  (->> (list 9 10 11)
    (navigable (fn [_index value] (_d-entity_ _db_ value)) )))


(tests
  (let [user-summary #(str (:user/name %) " – " (:user/email %))
        selected 9
        expr (hfql {(_d-entity_ _db_ selected) [:db/id :user/email user-summary keys clojure.core/keys cc/keys]})]
    #_(values expr)
    (pull expr))
  := {'(_d-entity_ _db_ 9)
      {:db/id 9,
       :user/email "alice@example.com",
       'user-summary "Alice – alice@example.com",
       'keys '(:db/id :user/email :user/name :user/address),
       'clojure.core/keys '(:db/id :user/email :user/name :user/address)
       'cc/keys '(:db/id :user/email :user/name :user/address)}}
  )

(tests
  (let [selected 9]
    (pull (hfql {(_d-entity_ _db_ selected) [:user/name {:user/address [:address/number]}]})))
  := {'(_d-entity_ _db_ 9)
      {:user/name "Alice", :user/address {:address/number 42}}}
  )

(tests
  (values (seed {'% {:a 1}} (hfql *)))
  := '({:a 1})

  (pull (seed {'% _alice_} (hfql *)))
  := _alice_

  (pull (seed {'% _alice_} (hfql {identity [:db/id *]})))
  := {'identity _alice_})


(tests
  "Traversing calls into ccp/nav"
  (values (hfql {(_people_) {* [:user/name]}}))
   := '((9 10 11) {:user/name "Alice"} "Alice" {:user/name "Bob"} "Bob" {:user/name "Charlie"} "Charlie")

  (pull (hfql {(_people_) {* [:user/name]}}))
  := {'(_people_) [{:user/name "Alice"}
                   {:user/name "Bob"}
                   {:user/name "Charlie"}]})


;;; Metas

(tests
  "keyword aliases"

  (ast/form-short (meta (hfql :test)))
  := :test

  (ast/form (meta (hfql ::test)))
  := :hyperfiddle.hfql2/test

  (ast/form-short (meta (hfql ::test)))
  := "::test"

  )

(tests
  "No need to seed to get static children"
  (->> (hfql {identity inc})
    (next-steps)
    (first)
    (meta)
    (ast/form))
   := 'inc
  )

(tests
  "children metas"
  ;; G: I added this test case because hoistCofreeT can wrap metadatas like
  ;; matryoshkas. Causing every point to contain a stack of metas from root to
  ;; point. There may be a future use case for "looking up parent metas", but
  ;; for now we only want metas "at point" for every point.
  ;; This test case ensures no matryoshkas on metadata.
  (->> (hfql {identity inc})
    (next-steps)
    (first)
    (meta)
    (ast/form))
  := 'inc
  )

(comment
  (pull (hfql {(all-ns) {* ns-name}}))
  )

#?(:clj
   (tests
     "Clojure 1.12 method functions"

     (values (seed {'% "  "} (hfql [String/.isBlank java.lang.String/.isEmpty .length])))
     := '({String/.isBlank true, java.lang.String/.isEmpty false, .length 2} true false 2)
     ))

;; ── Navigator API ────────────────────────────────────────────────────
;; Functions consumed by navigator6. Public but not part of the human-facing API.
;; Stable enough for navigator to depend on.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sitemap / Dynamic HFQL expression manipulation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:no-doc wrap-in-pull "Turn (hfql inc) into (hfql [inc]). Builds a new instance. Loses eventual cached IO."
  [stepT]
  (seed (scope stepT)
    (hfql* (ana/wrap-in-pull (meta stepT)))))

(tests
  (pull (seed {'% 1} (hfql inc)))
  := 2
  (pull (wrap-in-pull (seed {'% 1} (hfql inc))))
  := '{inc 2}
  )

(defn- merge-pull-head-attrs [rose-a rose-b]
  (let [form-a (ana/unquote* (ast/form rose-a))
        merged-form (vec (distinct (into (vec form-a) (ana/unquote* (ast/form rose-b)))))]
    (assoc (extract rose-a)
      ::ast/lexical-env {} ::ast/form merged-form)))

(tests
  (merge-pull-head-attrs
    (ana/analyze {'% 1} [:a :b])
    (ana/analyze {'% 2} [:a :c]))
  := {:hyperfiddle.hfql2.ast/id _
      :hyperfiddle.hfql2.ast/type :hyperfiddle.hfql2.ast/pull,
      :hyperfiddle.hfql2.ast/form [:a :b :c],
      :hyperfiddle.hfql2.ast/lexical-env {},
      :hyperfiddle.hfql2.ast/metadata nil}
  )

(declare describe) ; forward ref: symbolic-edge* delegates to describe for runtime-value sources

(defn symbolic-edge* [attrs]
  (or (::as attrs) ; user-explicit from options syntax or metadata
      (::label attrs) ; auto-generated labels (e.g. type-column). Set via ^{::hfql/label ...} metadata on forms.
      (let [type (::ast/type attrs)
            edge (::ast/edge attrs)]
        (case type
          ::ast/traversal (ast/symbolic-edge edge)
          ::ast/star nil ; star is syntax, not a named edge
          (::ast/recur ::ast/keyword ::ast/string ::ast/symbol
           ::ast/var ::ast/call) (::ast/form attrs)
          ;; Delegate to `describe` on the source object (e.g. java.lang.reflect.Method)
          ;; for the display name. Falls back to the fn object for untagged runtime values.
          ::ast/runtime-value (or (:name (some-> (::hfqlp/obj attrs) describe))
                                  (::ast/form attrs))
          ::ast/literal (::ast/resolved-runtime-value attrs)
          ::ast/pull nil
          (::ast/quoted-var ::ast/quoted-symbol) (::ast/form attrs)
          ::ast/lambda (if (= ::ast/keyword-fn (::ast/kind attrs))
                         (or (::ast/form-resolved attrs) (::ast/form attrs))
                         (let [[_fn ?name] (::ast/form attrs)]
                           (if (symbol? ?name) `(~'fn ~?name ~'...) '(fn anonymous ...))))))))

(defn symbolic-edge
  "Return a semantic symbolic representation of an HFQL point.
  (symbolic-edge (hfql :user/name)) := :user/name
  (symbolic-edge (hfql {:user/address [:address/street]})) := :user/address
  (symbolic-edge (hfql {(clojure.java.io/file \".\") [.getName]})) := '(clojure.java.io/file \".\")
  (symbolic-edge (hfql ^{::as 'add-1} inc)) := 'add-1
  "
  [hfqlT]
  (when hfqlT
    (symbolic-edge* (extract (meta hfqlT)))))

(defn display-edge
  "Strip the (quote ...) wrapper from runtime-discovered map keys so the UI
   shows e.g. (count ?x) instead of (quote (count ?x)). Inverse of
   disambiguate-from-hfql-syntax (interpreter.cljc) which quotes list/number/star
   keys to prevent HFQL from misinterpreting them as syntax."
  [edge]
  (if (and (seq? edge) (= 'quote (first edge)))
    (second edge)
    edge))

(defn- resolved-symbolic-edge*
  "Like symbolic-edge* but uses form-resolved for symbol/var/call nodes.
  Takes a RoseTree AST node. Used for union deduplication so that
  jar-filename and dustingetz.nav-jar/jar-filename merge as one column."
  [node]
  (let [attrs (extract node)]
    (case (::ast/type attrs)
      (::ast/symbol ::ast/var ::ast/call)
      (let [fr (::ast/form-resolved attrs)]
        (if (or (symbol? fr) (nil? fr))
          (or fr (::ast/form attrs))
          (::ast/form attrs))) ; form-resolved is not a symbol (e.g. fn from stale cache) — fall back
      (symbolic-edge* attrs))))

(defn- resolved-symbolic-edge
  "Like `symbolic-edge`, but resolves symbols for the environment.
  (def alice 42)
  (resolved-symbolic-edge (hfql (user-profile alice))) := '(user-profile 42)
  "
  [hfqlT]
  (when hfqlT
    (resolved-symbolic-edge* (meta hfqlT))))

;; PERF: Eager — `into #{}` forces children-a for the dedup set, then `into (vec ...)`
;; forces both seqs again. A lazy alternative (concat + memoized edge-set) would defer
;; materialization.
(defn- merge-hfqlt-children
  "Merge two sequences of HFQLT children by resolved-symbolic-edge. Left-biased."
  [children-a children-b]
  (let [a-edges (into #{} (map resolved-symbolic-edge) children-a)]
    (into (vec children-a)
          (remove (comp a-edges resolved-symbolic-edge))
          children-b)))

;; PERF: Same eager pattern as merge-hfqlt-children, but AST-level (pure computation, no IO).
(defn- merge-pull-children [children-A children-B]
  (let [a-edges (into #{} (map resolved-symbolic-edge* children-A))]
    (into children-A (remove (comp a-edges resolved-symbolic-edge*) children-B))))

(defn- combine-pull [rose-a rose-b]
  (let [metas (merge-pull-head-attrs rose-a rose-b)]
    (fmap (fn [kv] (assoc kv ::ast/lexical-env (::ast/lexical-env metas)))
      (rose-tree
        metas
        (merge-pull-children
          (unwrap rose-a) (unwrap rose-b))))))

(defn- union-pull
  "Merge two pull HFQLT instances at the CofreeT level, preserving interpreter
  metadata (::static-children, ::scope, etc.) and cached Store computations."
  [a b]
  (let [rose-a        (meta a)
        rose-b        (meta b)
        merged-attrs  (merge-pull-head-attrs rose-a rose-b)
        merged-ast-children (merge-pull-children (unwrap rose-a) (unwrap rose-b))
        ;; PERF: Union is eager end-to-end — merge-hfqlt-children and merge-pull-children
        ;; above both force all children immediately. When called from augment-with-suggestions!,
        ;; this materializes all suggested columns per row even though IndexRing only renders
        ;; `limitH` visible columns.
        merged-static (merge-hfqlt-children
                        (or (::co/static-children rose-a) [])
                        (or (::co/static-children rose-b) []))
        ;; TODO ::ast/id should become a pure accessor instead of a stored value —
        ;; it's a pure function of node content. See also ast/compute-node-id and ast/node-id.
        merged-rose   (let [rt (rose-tree merged-attrs merged-ast-children)
                            base (assoc (mapHead #(assoc % ::ast/id (ast/compute-node-id
                                                                      (rose-tree % merged-ast-children))) rt)
                                        ::co/static-children merged-static)]
                        ;; Carry forward record extras from rose-a (e.g. ::positional-index).
                        ;; rose-tree and mapHead create fresh RoseTrees without extras,
                        ;; and merge-pull-head-attrs uses (extract) which only returns AST attrs.
                        ;; Skip keys already set above (::co/static-children has its own merge).
                        (reduce-kv (fn [r k v] (if (contains? r k) r (assoc r k v)))
                                   base
                                   (dissoc rose-a :a :children)))
        store-a       (runCofreeT a)
        store-b       (runCofreeT b)
        merged-scope  (merge (pos store-b) (pos store-a))  ; left-biased
        merged-store  (store/store
                        (fn [scope]
                          (let [cf-a (store/peek scope store-a)
                                cf-b (store/peek scope store-b)]
                            (->CofreeF (headF cf-a)
                                       (merge-hfqlt-children
                                         (tailF cf-a) (tailF cf-b)))))
                        merged-scope)]
    (co/->hfqlT (assoc merged-rose ::co/scope merged-scope)
                merged-store)))

(defn union                             ; For advanced use cases (expert level).
  "Monoidal union of HFQL expressions at the CofreeT level.
  Merges children by symbolic-edge (left-biased).
  Preserves all interpreter metadata and cached Store computations."
  ([hfqlT] hfqlT)
  ([hfqlT-a hfqlT-b]
   (cond
     (nil? hfqlT-b) hfqlT-a
     (nil? hfqlT-a) hfqlT-b
     :else
     (let [type-a (node-type hfqlT-a)]
       (assert (= type-a (node-type hfqlT-b)))
       (case type-a
         ::ast/pull (union-pull hfqlT-a hfqlT-b)
         (throw (ex-info (str "Union not supported for " type-a) {:type type-a}))))))
  ([hfqlT-a hfqlT-b & hfqlTs]
   (reduce union (union hfqlT-a hfqlT-b) (remove nil? hfqlTs))))

(tests
  (->
    (union
      (let [a 1]
        (hfql [:a :b]))
      (let [b 2]
        (hfql [:a :c])))
    (meta)
    (extract))
  := {:hyperfiddle.hfql2.ast/id _
      :hyperfiddle.hfql2.ast/type :hyperfiddle.hfql2.ast/pull,
      :hyperfiddle.hfql2.ast/form [:a :b :c],
      :hyperfiddle.hfql2.ast/lexical-env {},
      :hyperfiddle.hfql2.ast/metadata nil
      })

(tests
  "union preserves static-children"
  (let [u (union (seed {'% {:a 1 :b 2}} (hfql [:a :b]))
                 (seed {'% {:a 1 :c 3}} (hfql [:a :c])))]
    (count (::co/static-children (meta u))))
  := 3

  "union produces correct values"
  (pull (seed {'% {:a 1 :b 2 :c 3}}
    (union (hfql [:a :b]) (hfql [:a :c]))))
  := {:a 1 :b 2 :c 3}
  )

(defn- normalize-edge [edge]
  (cond (ident? edge) `(~edge ~'%)
        :else edge))

(declare identify)
(defn symbolic-edge-computed [hfqlT]
  (or (seq
        (map (fn [part] (or (identify part) part))
          (normalize-edge
            (co/symbolic-edge hfqlT ::co/point-free false))))
    (either (comp symbol pr-str type) identify (value-at-point hfqlT))))

(defn- call-matches? [edgeA edgeB]
  (when (and (seq? edgeA) (seq? edgeB)) ; call form
    (and (= (first edgeA) (first edgeB)) ; same fn
      (= (count edgeA) (count edgeB)) ; same argc
      )))

(defn- child-matches? [symbolic-edge childT]
  (let [child-edge (hyperfiddle.hfql2/symbolic-edge childT)]
    (or (= symbolic-edge child-edge) ; direct match
      (call-matches? symbolic-edge child-edge)
      (call-matches? symbolic-edge (normalize-edge child-edge))
      )))

;; TODO find-child-by, find-child, find-cell-by.* should all be refactored into an `index` accessor of a representable functors.

(defn- find-child-by [pred hfqlT]                               ; Human API
  (->> (unwrap hfqlT)
    (eduction
      #?(:clj (halt-when (fn [_] (.isInterrupted (Thread/currentThread))))
         :cljs (map identity))
      (filter pred))
    (first)))

(defn- find-child [symbolic-edge hfqlT] (find-child-by (partial child-matches? symbolic-edge) hfqlT)) ; Human API

(def id (comp ast/node-id meta))

(defn find-cell-by-column* "Lookup a cell in a set of cells."
  ([columnT] (partial find-cell-by-column* columnT))
  ([columnT cellTs]
   (when columnT
     (let [identity (id columnT)]
       (first (filter #(= identity (id %)) cellTs))))))

(defn find-cell-by-column "Lookup a column in a row."
  ([columnT] (partial find-cell-by-column columnT))
  ([columnT rowT]
   (find-cell-by-column* columnT (unwrap rowT))))

(defn find-cell-by-symbolic-column "Lookup a column in a row by its symbolic form." ; Human API — TODO isn't that somewhat a duplicate of find-child?
  ([symbolic-column] (partial find-cell-by-symbolic-column symbolic-column))
  ([symbolic-column rowT]
   (when symbolic-column
     (first (filter #(= symbolic-column (symbolic-edge %)) (unwrap rowT))))))

(comment
  (def _rows (unwrap (seed {'% (clojure.java.io/file ".")} (dustingetz.file-explorer/sitemap `dustingetz.file-explorer/dir-list))))
  (def _columnT (first (unwrap (first _rows))))
  (find-cell-by-column _columnT (second _rows))
  )

;;;;;;;;;;;;;;;
;; Protocols ;;
;;;;;;;;;;;;;;;

;; TODO disambiguate – following "nav" should be exposed here but name is already taken by `nav` above.
#_(defn nav [object k v] (hyperfiddle.hfql2.protocols/nav object k v))

(def ^{:dynamic true, :doc "HFQL will bind this var to the current collection being traversed in the context where `hfql-resolve` is called."} *collection-at-point*)
(def ^{:dynamic true, :doc "HFQL will bind this var to the current value in the context where `hfql-resolve` is called."} *value-at-point*)
(defn hfql-resolve* [symbolic-object-constructor]
  (try
    (let [resolved (-hfql-resolve symbolic-object-constructor)]
      (if (either? resolved)
        resolved
        (right resolved)))
    (catch #?(:clj Throwable, :cljs :default) t
      (left t))))

(defn hfql-resolve
  "Dual of `identify`. Resolve a symbolic constructor produced by `identify` to its original runtime object, throwing on failure.
  E.g. (hfql-resolve '(clojure.java.io/file \".\")) := #java.io.File[\".\" 0x12345]."
  [symbolic-object-constructor] (unwrap! (hfql-resolve* symbolic-object-constructor))) ; Human API

(defn identifiable
  "Teaches HFQL how to identify an object by attaching a custom identification function via metadata.
  (identifiable (fn [file] (list 'clojure.java.io/file (.getAbsolutePath file))) my-file).
  "
  [identify-fn object]                                          ; Human API
  (vary-meta object assoc `hyperfiddle.hfql2.protocols/-identify (#?(:clj bound-fn*, :cljs identity) identify-fn))) ; #?(:clj bound-fn* :cljs identity)

#?(:clj
   (defn- extends-protocol? [proto atype]
     (and atype
       (or (extends? proto atype)
         (boolean (some #(extends-protocol? proto %) (bases atype)))))))

(defn identifiable? "State if HFQL knows how to identify a given object." [object]
  (or (and (has-meta? object) (contains? (meta object) `hyperfiddle.hfql2.protocols/-identify))
    #?(:clj (extends-protocol? Identifiable (#?(:clj class, :cljs type) object))
       :cljs (and (not (nil? object))
               (object? object)
               (satisfies? Identifiable object)))))

(defn identify                                                  ; Human API
"Produces a symbolic identity for `object`, or nil if `object` does not
explicitly implement `Identifiable`. A symbolic identity value should be
serializable and uniquely resolvable to the extent possible. All values being
self-identical, `(or (identify x) x)` always yields a valid identifier, though
it might not be serializable."
  [object]
  (when (identifiable? object)
    (hyperfiddle.hfql2.protocols/-identify object)))

#?(:clj
   (tests
     (identify (Object.)) := nil
     (identify nil) := nil
     (identify (identifiable (constantly `hash-map) {})) := `hash-map)

   :cljs
   (tests
     (identify (js-obj)) := nil
     (identify nil) := nil
     (identify (identifiable (constantly `hash-map) {})) := `hash-map)
   )

(extend-protocol Identifiable
  #?(:clj java.util.Map$Entry, :cljs MapEntry)
  (-identify [entry]
    (let [key (key entry)
          identity (or
                     (identify key)
                     (and (ana/literal? key)
                       key)
                     (and (ident? key)
                       key)
                     nil)]
      (or
        (and (some? identity)
          `(find ~identity))
        nil)))
  )

(defmethod -hfql-resolve 'clojure.core/find [[_ key]]
  (bind *value-at-point*
    (fn [value]
      (fmap (fn [resolved-key] (clojure.core/find (original-object value) resolved-key))
        (or (hfql-resolve key) (right key)) ; Hum, isn't it the wrong place to resolve the inner value?
        ))))
(defmethod -hfql-resolve `current-point [[_]] *value-at-point*)
(defmethod -hfql-resolve 'hyperfiddle.hfql2/nth [[_ index]]
  (try (fmap #(nth % index nil) *collection-at-point*)
       (catch #?(:clj Throwable, :cljs :default) t
         (println "Failed to resolve by positional index running (nth % " index ") in collection of type:" (type (either identity identity *collection-at-point*)) "." t)
         nil)))

(def ^:private DEFAULT-SUGGESTION (hfql ^::default [identity identify type]))
(def ^:private LEAF-SUGGESTION (hfql ^::default [identity]))

(extend-protocol Suggestable
  #?(:clj java.util.Map$Entry, :cljs MapEntry)
  (-suggest [_] (hfql [key val]))
  #?(:clj Object, :cljs default)
  (-suggest [_] LEAF-SUGGESTION)
  nil (-suggest [_]))

#?(:clj (extend-type Class
          Identifiable (-identify [klass] `(java.lang.Class/forName ~(.getCanonicalName klass)))
          Suggestable (-suggest [_] (hfql [Class/.getCanonicalName Class/.getMethods Class/.getFields]))))

#?(:clj
   (defmethod -hfql-resolve 'java.lang.Class/forName [[_ class-canonical-name]]
     ;; FIXME This should be in userland because there are security concerns/trade-offs here.
     ;; TODO remove this for HFQL core. This is safe to our use case, But may not be acceptable to some users.
     ;; I keeping this here for now because of demos.
     (try
       (java.lang.Class/forName class-canonical-name
         false ; don't run static initializers, they aren't pure.
         ;; scope resolution to the Thread Context Class Loader (TCCL).
         ;; In a clojure app the TCCL is *usually* a clojure.lang.DynamicClassLoader and *may* default to clojure.lang.RT/baseLoader
         ;; (type (clojure.lang.RT/baseLoader)) := clojure.lang.DynamicClassLoader
         ;; The TCCL is often set/configured by the app server, container or managed environment setup.
         ;; I picked the TCCL here because:
         ;; - in our demos the TCCL is Clojure's DynamicClassLoader – this is safe to our use case.
         ;; - users should be able to customize the class loader. An idiomatic way to do so is to set the TCCL with Thread/.setContextClassLoader.
         (.getContextClassLoader (Thread/currentThread))
         )
       (catch ClassNotFoundException _
         nil))))


;; Exceptions
#?(:cljs
   (defn- get-stack-trace [ex]
     (when-let [stack (.-stack ^js/Error ex)]
       (cljs-stacktrace/parse-stacktrace {} stack
         (cond
           user-agent-product/CHROME  :chrome
           user-agent-product/FIREFOX :firefox
           user-agent-product/SAFARI  :safari
           user-agent-product/IE      :ie
           :else        :unknown)
         nil))))

(extend-protocol Suggestable
  #?(:clj Throwable, :cljs js/Error)
  (-suggest [_] (hfql [type ex-message ex-data {ex-cause ...} #?(:clj .getStackTrace :cljs get-stack-trace)])))

#?(:clj
   (extend-type java.lang.StackTraceElement
     Suggestable
     (-suggest [_] (hfql [.getClassName .getMethodName .getFileName .getLineNumber .isNativeMethod .getModuleName .getModuleVersion .getClassLoaderName]))))

(defmulti -suggest-dispatch type)
(defmethod -suggest-dispatch :default [type] type)
(defmulti -suggest -suggest-dispatch)

(defn suggest* [object]
  (try (right (-suggest (original-object object)))
       (catch #?(:clj Throwable, :cljs :default) err
         (left err))))

(defn suggest                         ; Human API
  "Return an HFQL instance describing possible navigation onto the given `object`.
  Used by UIs to draw columns for runtime objects.

  Extensible on any object by implementing `-suggest` of the
  `hyperfiddle.hfql2.protocols/Suggestable` protocol or extending the
  `hyperfiddle.hfql2/-suggest` multimethod. Protocol implementation should be
  favored, unless finer grained dispatch is needed: the multimethod has
  precedence over the protocol.

  `-suggest` must return an HFQL instance."
  [object] (unwrap! (suggest* object)))

(defn- suggest-map [object]
  ;; TODO render the type in UI if the map-like object is of an interesting type (e.g. IRecords)
  ;; See edge case in `suggest-record` for records with no static fields.
  (hfql [*] object))

(defn- suggest-record [record]
  (let [basis #?(:clj
                 (-> (class record)
                   (.getMethod "getBasis" nil)
                   (.invoke nil nil))
                 :cljs
                 (.getBasis (type record)))
        record-entries (map keyword basis)]
    (if (empty? basis)
      ;; Edge case of a record with no static fields but potentially some extra keys associated to it.
      ;; Real-world example: datomic.fulltext_index.PersistentFulltext accessible under d/db > :memidx > :fulltext
      (suggest-map record)
      (->> (keys record)
        (map (fn [key] (cond (number? key) (list 'quote key) ; raw numbers are interpreted as recursion, not colums.
                             :else key)))
        (remove (set record-entries)) ; won't clash with numbers mapped to quoted numbers – record fields must be symbols.
        (concat record-entries)
        (vec)
        (ana/analyze {})
        (hfql*)))))

(defn- multiplicity-many-value? [object] ; TODO promote to HFQL
  ;; `coll?` is not enough. E.g. datomic.api.EntityMap is `coll?` but isn't `map?`.
  ;; Also we don't want to treat records as coll
  (and (not (record? object))
    (or (sequential? object)
      (set? object)
      (enumeration? object)
      (iterator? object)
      #?(:clj (array? object))
      #?(:clj (instance? java.util.Collection object)))))

(defn- suggest-collection [coll] (hfql {* [*]} (to-seq coll)))

#?(:clj
   (def ^:private discover-methods-for-class
     "Memoized by class so that the same fn objects are returned for the same class.
      This ensures stable HFQL IDs (compute-id hashes ::form which contains the fn)
      across ColumnPicker (samples first item) and MatrixRow (per-row augmentation). "
     (memoize
       (fn [^Class clazz]
         (let [obj-methods (set (map #(.getName %) (.getMethods Object)))
               ;; Prefer Method objects from public interfaces over the concrete class.
               ;; .getMethods on the impl class returns Methods bound to the declaring class;
               ;; on modular JDK classes (Java 9+), internal classes like
               ;; sun.management.BaseOperatingSystemImpl are not exported, so .invoke throws
               ;; IllegalAccessException even after .setAccessible. Methods resolved from
               ;; public interfaces are always accessible.
               interface-methods (->> (.getInterfaces clazz)
                                   (mapcat #(.getMethods ^Class %))
                                   (into {} (map (fn [^java.lang.reflect.Method m] [(.getName m) m]))))
               all-methods (->> (.getMethods clazz)
                             (into {} (map (fn [^java.lang.reflect.Method m] [(.getName m) m]))))]
           (->> (merge all-methods interface-methods) ; interface methods win
             (vals)
             (filter (fn [^java.lang.reflect.Method m]
                       (and (zero? (.getParameterCount m))
                            (not= Void/TYPE (.getReturnType m))
                            (not (obj-methods (.getName m))))))
             (mapv (fn [^java.lang.reflect.Method m]
                     (try (.setAccessible m true) (catch Exception _))
                     (with-meta
                       (fn [obj]
                         ;; Invoke on a separate thread to avoid deadlocks with framework
                         ;; internals (e.g. Jetty holds request-handling locks; a reflected
                         ;; method that acquires the same locks will deadlock on this thread
                         ;; but succeed on a fresh thread that doesn't hold those locks).
                         ;; Uses Clojure's cached daemon thread pool; timeout prevents
                         ;; permanent hangs if a true cross-thread deadlock occurs.
                         ;; Timeout returns nil; real exceptions propagate for UI display.
                         (let [result (deref (future (.invoke m obj (object-array 0))) 50 ::timeout)]
                           (when-not (= ::timeout result) result)))
                       ;; Store the source Method — `describe` dispatches on its type
                       ;; to produce :name/:arglists/:doc lazily at describe-time.
                       {::hfqlp/obj m})))))))))
#?(:clj
   (defn- discover-methods
     "Discover zero-arg public methods on a Java object, returning callable accessor
      functions with the source Method as ::hfqlp/obj metadata. `describe` dispatches
      on the Method type for column display and tooltips."
     [object]
     (discover-methods-for-class (class object))))

#?(:clj
   (defn- suggest-java-object
     "Build a dynamic HFQL suggestion from discovered zero-arg methods."
     [object]
     (let [methods (discover-methods object)]
       (if (seq methods)
         (union (hfql [type]) (build-hfql (vec methods)))
         DEFAULT-SUGGESTION))))

#?(:clj
   (defn- java-object?
     "True if obj is a Java object (not a Clojure type) that may have discoverable methods.
     Excludes nil and some scalar types — nil is not an instance of anything, so the IObj check alone
     would wrongly classify it as a Java object, leading to spurious column
     discovery (e.g. 'type' / '.getBasis' on nil cells)."
     [object]
     (and (some? object)
          (not (instance? clojure.lang.IObj object))
          (not (string? object))
          (not (keyword? object)) ; Keyword doesn't implement IObj but is a Clojure scalar, not a Java object
          (not (number? object))
          (not (boolean? object))
          (not (char? object)))))

(defmethod -suggest :default [object]
  (let [user-suggestion (hyperfiddle.hfql2.protocols/-suggest object)]
    (if (or (nil? user-suggestion) (::default (extract (meta user-suggestion))))
      (cond
        (record? object) (suggest-record object)
        (multiplicity-many-value? object) (suggest-collection object)
        (imap? object) (suggest-map object)
        #?@(:clj [(java-object? object) (suggest-java-object object)])
        :else user-suggestion)
      user-suggestion)))

(defn- suggest-at-point! [hfqlT]
  (->> (value-at-point hfqlT)
    (=<< suggest*)
    (either
      (fn [err] (println "Failed to suggest columns:" (ex-message err)) nil)
      (fn [suggestedT]
        (if (some? suggestedT)
          (hoistHfqlT #(assoc % ::suggested true)
            suggestedT) ; recursively map over HFQLT metadata - lazily
          hfqlT)))))

(declare parent set-parent)

;; ── Call sites (7) ──────────────────────────────────────────────────
;; matrix.cljc:171   — per-row rendering (PERF: redundant, same schema per row)
;; matrix.cljc:274   — ColumnPicker (discovers all columns for picker UI)
;; matrix.cljc:317   — associative views (once at RenderMatrix level)
;; sorting.cljc:105  — per-row sort (TODO: only augment sorted column(s))
;; search.cljc:151   — per-row search (discovers searchable cells)
;; selection2.cljc:438 — cell resolution (resolve column from seeded point)
;; selection2.cljc:454 — cell resolution fallback (resolve from row point)
;;
;; PERF: Per-row eagerness — each call forces: (1) value-at-point IO to discover schema,
;; (2) children-ids diff (forces ::co/static-children), (3) collection sampling via `(first coll)`
;; for [*] pulls, (4) eager union of all children. Collection views pay this per visible row
;; (all rows rediscover the same schema). Associative views pay it once at RenderMatrix level.
;; Lazy alternative: compute suggestions once at collection level, share across rows.
;; Blocked on heterogeneous collections (varying attrs per entity).
(defn- resolve-star-pull
  "When the static tree is [*] (pull-all), discover concrete columns from the
  runtime value by sampling the first item. Returns a concrete pull HFQLT or nil.
  Wrapped in try-catch: sampling can fail on objects with side-effectful
  methods (e.g. Jetty connectors deadlock when reflected during request handling)."
  [hfqlT]
  (try
    (let [coll (from-right nil (value-at-point hfqlT))]
      (cond
        (multiplicity-many-value? coll)
        (when-let [sample (first coll)]
          (let [s (from-right nil (suggest* sample))]
            (cond
              ;; Suggestion is a concrete pull (not [*] itself) — use it directly.
              ;; E.g. Java objects → [type .method1 .method2 ...], records → [:field1 :field2 ...]
              ;; Maps return [*] from suggest-map, which would no-op in union — rejected here.
              (and s (pull? s) (not (pull-has-star? s))) s
              ;; Map-like sample — materialize keys as explicit columns.
              (imap? sample) (build-hfql (vec (keys sample))))))
        ;; Value is already a single map (e.g. {*} traversal fanned out
        ;; a collection of maps, [*] sees one map directly)
        (imap? coll)
        (build-hfql (vec (keys coll)))))
    (catch #?(:clj Throwable, :cljs :default) _ nil)))

(defn- mark-suggested
  "Mark each node in hfqlT with ::suggested based on whether its id is in suggested-ids."
  [suggested-ids hfqlT]
  (hoistHfqlT (fn [ast] (assoc ast ::suggested (boolean (suggested-ids (ast/node-id ast)))))
    hfqlT))

(let [children-ids (comp set (fmap id) ::co/static-children meta)]
  (defn augment-with-suggestions! [hfqlT]
    (when hfqlT
      (-> (if-let [suggestedT (suggest-at-point! hfqlT)]
            (if (-> (::default (extract (meta suggestedT)))
                  (and (pull-has-star? hfqlT))
                  (and (> 1 (count (children-ids hfqlT)))))
              hfqlT
              (let [suggested? (set/difference (children-ids suggestedT) (children-ids hfqlT))]
                (cond
                  (empty? suggested?) hfqlT

                  ;; Both are pulls — merge children, mark new ones as suggested
                  (and (pull? hfqlT) (pull? suggestedT))
                  (mark-suggested suggested? (union hfqlT suggestedT))

                  ;; [*] pull but suggestion isn't a pull — resolve [*] from runtime value
                  (and (pull? hfqlT) (= '[*] (ast/form (meta hfqlT))))
                  (if-let [resolved (resolve-star-pull hfqlT)]
                    (let [suggested? (set/difference (children-ids resolved) (children-ids hfqlT))]
                      (mark-suggested suggested? (union hfqlT resolved)))
                    (update-scope LEAF-SUGGESTION (constantly (scope hfqlT))))

                  ;; Static is an explicit pull (not [*]) — user specified columns, keep them
                  (pull? hfqlT) hfqlT

                  ;; Static isn't a pull, suggestion is — adopt suggestion's structure with static's scope
                  (pull? suggestedT) (update-scope suggestedT (constantly (scope hfqlT)))

                  :else hfqlT)))
            hfqlT)
        (set-parent (parent hfqlT))))))

(comment
  (import [java.io File])
  (augment-with-suggestions!
    (seed {'% (clojure.java.io/file ".")}
      (hfql [File/.getName {File/.listFiles ...}])))
  (unwrap *1)
  )

(defmethod -resolve-dispatch :default [object]
  (if (seq? object)
    (first object)
    object))

;; Safe comparison protocol - prevents blowup on infinite lazy seqs and uncomparable objects.
;; Collections return [type-name, count-or-max] for grouping by type + safe ordering by size.
;; Unknown objects return type name only (never the raw object).
(defn comparable
  "Return a sort-friendly representation of an object (string, number, or [type count] for collections).
  Extensible on any object by implementing `hyperfiddle.hfql2.protocols/ComparableRepresentation`."
  [object] (hyperfiddle.hfql2.protocols/-comparable object))    ; Human API

(extend-protocol ComparableRepresentation
  nil (-comparable [_] nil)

  ;; Safe primitives - direct comparison
  #?(:clj String, :cljs string) (-comparable [this] this)
  #?(:clj Number, :cljs number) (-comparable [this] this)
  #?(:clj Boolean, :cljs boolean) (-comparable [this] this)
  #?(:clj clojure.lang.Symbol, :cljs cljs.core/Symbol) (-comparable [this] (str this))
  #?(:clj clojure.lang.Keyword, :cljs cljs.core/Keyword) (-comparable [this] (str (namespace this) "/" (name this)))

  #?@(:clj [java.lang.Class (-comparable [^Class this] (.getName this))
            java.util.UUID (-comparable [this] (str this))
            java.util.Date (-comparable [this] (.getTime ^java.util.Date this))])

  #?@(:cljs [cljs.core/UUID (-comparable [this] (str this))])

  ;; Either - [group, value] to keep Rights and Lefts separate
  ;; Rights sort first (prefixed with 0), Lefts sort last (prefixed with 1)
  #?@(:clj [hyperfiddle.ct.either.Right
            (-comparable [this] ["0:right" (comparable (from-right nil this))])
            hyperfiddle.ct.either.Left
            (-comparable [this] ["1:left" (comparable (from-left nil this))])]

      :cljs [hyperfiddle.ct.either/Right
             (-comparable [this] ["0:right" (comparable (from-right nil this))])
             hyperfiddle.ct.either/Left
             (-comparable [this] ["1:left" (comparable (from-left nil this))])])

  ;; Collections - [type-name, count-or-max] for grouping + safe ordering
  ;; Counted collections sort by count within type group, uncounted sort last
  #?@(:clj [clojure.lang.Seqable
            (-comparable [coll]
              [(.getName (class coll))
               (if (counted? coll) (count coll) Long/MAX_VALUE)])]

      :cljs [cljs.core/PersistentVector
             (-comparable [coll] [(pr-str (type coll)) (count coll)])
             cljs.core/PersistentHashMap
             (-comparable [coll] [(pr-str (type coll)) (count coll)])
             cljs.core/PersistentArrayMap
             (-comparable [coll] [(pr-str (type coll)) (count coll)])
             cljs.core/PersistentHashSet
             (-comparable [coll] [(pr-str (type coll)) (count coll)])
             cljs.core/List
             (-comparable [coll] [(pr-str (type coll)) (count coll)])
             cljs.core/LazySeq
             (-comparable [coll] [(pr-str (type coll)) js/Number.MAX_SAFE_INTEGER])])

  ;; Default: type name only - safe, groups by type, never inspects object
  ;; Counted objects and arrays get [type-name, count] like collections
  #?(:clj Object, :cljs default)
  (-comparable [this]
    #?(:clj (let [clazz (class this)
                  type-name (.getName clazz)]
              (cond
                (counted? this) [type-name (count this)]
                (.isArray clazz) [type-name (alength this)]
                :else type-name))
       :cljs (let [type-name (pr-str (type this))]
               (cond
                 (counted? this) [type-name (count this)]
                 (array? this) [type-name (alength this)]
                 :else type-name)))))

(defn- describe-dispatch-fn [object]
  (cond (satisfies? hyperfiddle.ct.cofree/ComonadCofree object)
        (node-type object)
        (map? object) (or (::ast/type object) (type object))
        :else (type object)))

;; TODO missing docstring
(defmulti describe describe-dispatch-fn)
(defmethod describe :default [_] nil)
(defmethod describe ::ast/symbol         [stepT] (extract (meta stepT)))
(defmethod describe ::ast/quoted-symbol  [stepT] (extract (meta stepT)))
;; Delegate to (describe source) when a source object is attached,
;; so extensible description (e.g. Method) flows through one multimethod.
(defmethod describe ::ast/runtime-value [stepT]
  (let [attrs (extract (meta stepT))]
    (if-some [source (::hfqlp/obj attrs)]
      (merge attrs (describe source))
      attrs)))
(defmethod describe ::ast/keyword        [stepT] (extract (meta stepT)))
(defmethod describe ::ast/string         [stepT] (extract (meta stepT)))
(defn- describe-call [attrs] (assoc attrs ::symbolic-edge (symbolic-edge* attrs)))
(defmethod describe ::ast/call   [stepT] (describe-call (extract (meta stepT))))
(defmethod describe ::ast/traversal [stepT] (describe-call (extract (ast/edge (meta stepT)))))

;; Extension: describe a reflected Java method for column display and tooltips.
;; Returns standard var-like metadata keys (:name, :arglists, :doc).
#?(:clj
   (defmethod describe java.lang.reflect.Method [^java.lang.reflect.Method m]
     {:name     (symbol (str "." (.getName m)))
      :arglists (list (mapv #(symbol (.getSimpleName ^Class %))
                        (.getParameterTypes m)))
      :doc      (str (.getName (.getDeclaringClass m))
                     "/." (.getName m)
                     " \u2192 " (.getSimpleName (.getReturnType m)))}))

(comment
  (describe nil)
  (describe "")
  (describe (hfql ns-name))
  )

(defn- format [fmt & args]
  #?(:clj (apply clojure.core/format fmt args)
     :cljs (apply gstring/format fmt args)))

;; TODO missing docstring
(defmulti describe-formatted describe-dispatch-fn)
(defmethod describe-formatted :default [_] "")

(defmethod describe-formatted ::ast/symbol [hfqlT]
  (let [desc (describe hfqlT)
        form (::ast/form desc)]
    ;; If the form carries metadata (e.g. d/query result reshaping attaches
    ;; the backing Datomic attribute), show it as the tooltip.
    ;; Otherwise fall through to the standard resolved-form tooltip.
    (if-let [m (not-empty (meta form))]
      (pr-str m)
      (let [form-resolved (or (::ast/form-resolved desc) form)
            var (::ast/var desc)]
        (cl-format nil "~a~%~{~a~^~%~}~@[~%~a~]" form-resolved (some-> var meta :arglists) (some-> var meta :doc))))))

;; Call nodes (e.g. `(d/query query-map)`) — without this, bare calls in
;; sitemaps fall through to :default → "" → no tooltip.
(defmethod describe-formatted ::ast/call [hfqlT]
  (let [desc (describe hfqlT)
        {::keys [symbolic-edge]} desc
        var (::ast/var desc)]
    (cl-format nil "~a~%~{~a~^~%~}~@[~%~a~]" symbolic-edge (some-> var meta :arglists) (some-> var meta :doc))))

(defmethod describe-formatted ::ast/keyword [hfqlT] (::ast/form (describe hfqlT)))
(defmethod describe-formatted ::ast/string  [hfqlT] (::ast/form (describe hfqlT)))
(defmethod describe-formatted ::ast/lambda  [hfqlT] (symbolic-edge hfqlT))
;; Delegate to describe-formatted on the source when available,
;; so Method (or any future foreign type) controls its own tooltip rendering.
(defmethod describe-formatted ::ast/runtime-value [hfqlT]
  (let [attrs (extract (meta hfqlT))]
    (if-some [source (::hfqlp/obj attrs)]
      (describe-formatted source)
      (str (symbolic-edge hfqlT)))))

(defmethod describe-formatted ::ast/traversal [hfqlT]
  ;; Bare calls like `'query (hfql (d/query query-map))` have no traversal
  ;; structure. SuggestAndRender wraps their result in a synthetic star-traversal
  ;; for table display. The star edge is structural — delegate to parent (the
  ;; original call HFQLT) so the title tooltip shows the call's docstring.
  (if (and (star-traversal? hfqlT) (parent hfqlT))
    (describe-formatted (parent hfqlT))
    (let [desc (describe hfqlT)
          {::keys [symbolic-edge]} desc
          var (::ast/var desc)]
      (cl-format nil "~a~%~{~a~^~%~}~@[~%~a~]" symbolic-edge (some-> var meta :arglists) (some-> var meta :doc)))))

;; Tooltip text for a reflected Java method: FQN doc + arglists.
#?(:clj
   (defmethod describe-formatted java.lang.reflect.Method [^java.lang.reflect.Method m]
     (let [{:keys [doc arglists]} (describe m)]
       (cl-format nil "~a~@[~%~{~a~^~%~}~]" doc arglists))))

(comment
  (describe (hfql :db/ident))
  (describe-formatted (hfql :db/ident))
  (describe (get dustingetz.namespace-explorer/sitemap `all-ns))
  (describe-formatted (get dustingetz.namespace-explorer/sitemap `all-ns))
  )

(defpointfree ^{:doc "Given an HFQL object, return it's parent, if any.

(def q (hfql [:a])))
(= q (parent (first (next-steps q)))) := true
"}
  parent (comp ::parent meta))

(defn ^:no-doc set-parent [childT parentT] (vary-meta childT assoc ::parent parentT))
(defpointfree multiplicity-many? (comp ast/multiplicity-many? meta))

(defn wrap-traversal-in-pull
  "Wrap an HFQLT traversal in a synthetic pull to provide a selectable row.
  Used by the navigator when a traversal-only recursive form has no natural pull.
  Unlike `wrap-in-pull` (which re-analyzes a leaf), this wraps the existing AST
  node directly — preserving the recursion structure."
  [hfqlT]
  ;; seed takes (env, t) — env first, so we can't use -> threading for it.
  (-> (seed (scope hfqlT) (unfoldT hfql-coalgebra (ast/->pull [(meta hfqlT)])))
    (set-parent (parent hfqlT))))

(defn- lookup
  "Given an HFQL step object, lookup for a symbolic point in the reachable HFQL scope.
  The reachable HFQL scope is made of:
  - the current step (passed in),
  - ancestor steps, up to the root,
  - multiplicity-1 children steps of ancestors steps.

  Example, in the following HFQL expression:
  ```
  {(shirt-orders) {* [:person/email
                      {[:person/shirt-size ::hfql/options (shirt-sizes :person/gender)] [:db/ident]}
                      {:person/gender [[:db/ident ::as 'gender-ident]]}]}}
  ```
  - `(shirt-sizes ...)` can refer to `:person/gender`, but only under the same multiplicity branch.
  - Refering to `:db/ident` from `:person/shirt-size` would unambiguously resolve to the `:db/ident`
    under `:person/gender`. Because it is the sole candidate at the same level as `:person/shirt-size`
    or part of the multiplicity-1 ancestry of `:person/shirt-size`.
  - Refering to `:db/ident` under `:person/shirt-size` would be ambiguous because `:db/ident` is reachable
    through both `:person/shirt-size` and `:person/gender`. In such case, one can refer to the `:as` label,
    assuming the label is unique in the reachable scope.

  Image:
                                                          (shirt-orders) Ⓐ
                                                                │
                                                                * fan out
                        ┌───────────────────────────────────────┴──────────────────────────┬──────   ─   ─   ─
┌───────────────────────┼────────────────────────────────────────┐ ┌───────────────────────┼────────────────────────────────────────┐
│        ┌──────────────┼────────────────────────────────┐       │ │        ┌──────────────┼────────────────────────────────┐       │
│        │              │                                │       │ │        │              │                                │       │
│        │              │          (shirt-sizes x )      │       │ │        │              │          (shirt-sizes x )      │       │
│        │              │             ▲         │        │       │ │        │              │             ▲         │        │       │
│        │              │             │         │        │       │ │        │              │             │         │        │       │
│ :person/email  :person/shirt-size ──┘         └▶:person/gender │ │ :person/email  :person/shirt-size ──┘         └▶:person/gender │
│                       │                                │       │ │                       │                                │       │
│                       │                                │       │ │                       │                                │       │
│                    :db/ident                        :db/ident  │ │                    :db/ident                        :db/ident  │
│                                                                │ │                                                                │
└────────────────────────────────────────────────────────────────┘ └────────────────────────────────────────────────────────────────┘
                        Ⓑ                                                                  Ⓒ

  - Ⓐ produces a collection and `*` fans-out, creating multiple branches.
  - Ⓑ and Ⓒ are branches, isolated from each others.
  - All nodes in Ⓑ can reference Ⓐ.
  - All nodes in Ⓒ can reference Ⓐ.
  - Nodes in Ⓑ cannot reference nodes in Ⓒ, and vice-versa.
  - `:person/shirt-size` references `shirt-sizes`, in turn referencing `:person/gender`.
  - `shirt-sizes` cannot reference `:person/shirt-size`
  - `:person/gender` cannot reference `shirt-sizes`.
  "
  ([hfqlT symbolic-edge]
   (lookup hyperfiddle.hfql2/symbolic-edge* hfqlT symbolic-edge))
  ([keyfn hfqlT symbolic-edge]
   (lookup keyfn hfqlT hfqlT symbolic-edge))
  ([keyfn previously-visitedT currentT symbolic-edge]
   (cond
     ;; check if current step has a matching `:as` label.
     ;; `:as` has priority.
     (= symbolic-edge (::as (extract (meta currentT))))
     currentT

     ;; check if current step has the same symbolic-edge
     (= symbolic-edge (keyfn (extract (meta currentT))))
     currentT

     ;; IF current step hasn't been visited already
     ;; AND current step doesn't match neither by symbolic edge nor by :as label
     ;; AND current step is of multiplicity many
     ;; THEN search amongst children
     ;; EXCLUDING previously visited step – to prevent a parent -> child -> parent loop
     (and (not= previously-visitedT currentT)
       (not (multiplicity-many? currentT))
       ;; (not (= :map (::value-type (extract (meta currentT))))) ; FIXME should not apply anymore. Verify FIXME looks like a wart – why do we need this special case? Maps are reshaped as a seq of kvs, and should probably be flagged as multiplicity many. But are they really?
       )
     (->> (unwrap currentT) ; children
       (remove #{previously-visitedT})
       (some #(lookup keyfn previously-visitedT % symbolic-edge)))

     ;; No match and no matching child. Look up the parent scope.
     :else
     (when-let [parentT (parent currentT)]
       (lookup keyfn currentT parentT symbolic-edge)))))


(tests
  (defn _f [x] (prn "IO – f") x)
  (defn _g [x] (prn "IO – g") x)
  (defn _h [x] (prn "IO – h") x)
  (def _hfql (hfql {_f [:a {_g :b} {_h ^{:as 'c} #(:c %)}]} {:a 1, :b 2, :c 3}))
  (def _a (-> _hfql unwrap first unwrap first))               ; no expected IO – check the REPL for a print.
  (def _c (-> _hfql unwrap first unwrap (nth 2) unwrap first)) ; no expected IO – check the REPL for a print.
  (symbolic-edge _a)
  := :a
  (symbolic-edge _c)
  := :c
  (lookup _a :a)
  := _a
  (lookup _a :c)                        ; no expected IO
  := _c
  (nav _a)                              ; will run _f
  (nav (lookup _a :c))                  ; will leverage cached _f and will run _h. _g isn't supposed to run.
  (nav _c)                              ; no expected IO, already cached by `(nav (lookup _a :c))`
  := 3
  )

(tests
  (def john {:person/first-name "John", :person/last-name "Doe"})
  (def query (hfql {john [:person/first-name]}))
  (values query)
  := '({:person/first-name "John", :person/last-name "Doe"}
       {:person/first-name "John"}
       "John")

  (pull query)
  := {'john {:person/first-name "John"}} ; note "john" isn't quoted
  (get-in *1 ['john :person/first-name])
  := "John"
  )


;; ── ::as option tests ─────────────────────────────────────────────────────────

(tests
  "::as option overrides symbolic-edge"
  (defn _as-f [x] x)
  (def _as-hfql (hfql {_as-f [:a [:b ::as 'aliased]]} {:a 1, :b 2}))
  (def _as-a (-> _as-hfql unwrap first unwrap first))
  (def _as-b (-> _as-hfql unwrap first unwrap second))
  (symbolic-edge _as-a) := :a
  (symbolic-edge _as-b) := 'aliased)

(tests
  "::as option — lookup finds by alias"
  (defn _as-g [x] x)
  (def _as-hfql2 (hfql {_as-g [:a [:b ::as 'renamed]]} {:a 1, :b 2}))
  (def _as-a2 (-> _as-hfql2 unwrap first unwrap first))
  (def _as-b2 (-> _as-hfql2 unwrap first unwrap second))
  (lookup _as-a2 'renamed) := _as-b2)

;;; Misc

(defn- unchunk-seq
  "Turn a clojure seqable into a one-step-at-a-time seq. Preserves eventual
  metadata. Must be called on the original chunked seq. Calling `unchunk-seq` on
  any descendent seq won't have any effect. E.g. `(unchunk-seq (range 3))` –
  turns a chunked LongRange into a proper lazy-seq. But `(unchunk-seq (map
  prn (range 3)))` will still print 0 1 2 because `map` eagerly preprocesses the
  first chunk.

  Use case: prevent undesired side effects while building the cofree tree. We
  don't want to realize any child value when computing fan-out cardinality (e.g. '*).
  Example: In {range {'* inc}}, given `range` receives 3, `range` will produce a
  chunked LongRange. Any downstream `map`, `filter`, `for` etc. will pre-process
  the first chunk and eventually call `inc`. This is not acceptable because in
  the general case `inc` may not be a pure function.

  Note range is a good example because: `range` may or may not return a chunked seq:
  - (type (range)) := clojure.lang.Iterate – not chucked, fully lazy
  - (type (range 1)) or (range 1 10 1) := LongRange – chunked
  - (type (range 1 1)) := PersistentList$EmptyList

  Also see `realize`.
  "
  [aseq]
  (if (or (chunked-seq? aseq) (vector? aseq)) ; calling seq on a vector produces a chunked-seq
    (with-meta
      (lazy-seq
        (when-let [s (seq aseq)]
          (cons (first s) (unchunk-seq (rest s)))))
      (meta aseq))
    aseq))

#?(:clj
   (tests
     "Static method syntax support"
     ;; Original failing attempt, now passing
     (pull (hfql {(java.lang.management.ManagementFactory/getOperatingSystemMXBean)
                  [.getArch .getAvailableProcessors]}))
     := {'(java.lang.management.ManagementFactory/getOperatingSystemMXBean)
          {'.getArch _, '.getAvailableProcessors _}}


     ;; another made up (but valid) case, 2026-01-08 G: AFAIKT no user attempt this one so far.
     (nav (hfql [identity java.util.Date/new] 1767883504555))
     := '{identity 1767883504555,
          java.util.Date/new #inst "2026-01-08T14:45:04.555-00:00"}

     ))


(defn recursion-end?! ; ! because this runs IO
  "State if the given point is a recursion point and if recursion should stop.
  Recursion stops when the recursion limit is exhausted or on nil values.
  At the HFQL tree-seqing level, recuring forever on nil is allowed because `nil` is not treated as a special value.
  But we haven't found a valid use case to recur on nil in a UI."
  [hfqlT]
  (let [rose (meta hfqlT)]
    (or (and (ast/recursion-point? rose) (empty? (next-steps hfqlT))) ; limit exhausted — interpreter produced no children (pure, no IO)
      (and (ast/recursion-root? rose)
        (= (right nil) (value-at-point hfqlT)))
      (and (ast/recursion-point? rose)
        (= (right nil) (nav* hfqlT))))))

(comment

  (pull (hfql [ex-message {ex-cause ...}] (ex-info "err" {})))

  )

(defn- supports-nth? [obj]
  (and (some? obj)
    #?(:clj ;; from clojure.lang.RT/nth and nthFrom
       (or (instance? clojure.lang.Indexed obj)
         (instance? java.lang.CharSequence obj)
         (.isArray (.getClass obj))
         (instance? java.util.RandomAccess obj)
         (instance? java.util.regex.Matcher obj)
         (map-entry? obj)
         (sequential? obj))
       :cljs ;; from https://cljs.github.io/api/cljs.core/nth
       (or (satisfies? IIndexed obj)
         (string? obj)
         (array? obj)
         (or (implements? ISeq obj)
           (implements? ISequential obj))
         (native-satisfies? IIndexed obj)))))

(defmethod hyperfiddle.hfql2.protocols/--datafy :default [obj]
  (let [dispatched (hyperfiddle.hfql2.protocols/-datafy obj)] ; protocol dispatch has priority
    (if (identical? obj dispatched)
      (cond
        (and (seqable? obj) (not (supports-nth? obj)) (not (associative? obj)))
        (try
          ;; `seqable?`'s contract says: "return true if `seq` is supported".
          ;; `seqable?` is true for all libpython-clj's objects. But seq may still fail on some of them.
          ;; Libpython-clj breaks the contract by defining all objects as Iterable.
          ;; Other types may break the contract. We decide the safest thing to
          ;; do is to return the object as-is and let users decide.
          (let [result (seq obj)]
            ;; Tag seqs auto-converted from Java Maps so the suggest dispatch
            ;; can route them to associative rendering instead of collection
            ;; traversal. Only this :default branch is tagged — user -datafy
            ;; protocol extensions short-circuit at line 1629, and user --datafy
            ;; multimethod extensions dispatch before :default.
            (if #?(:clj (and result (imap? obj))
                   :cljs false)
              (vary-meta result assoc ::map-entries true)
              result))
          (catch #?(:clj Throwable, :cljs :default) _
            obj))
        ;; TODO what about records?
        :else obj)
      dispatched)))

#?(:clj (defmethod hyperfiddle.hfql2.protocols/--datafy java.util.Iterator [obj] (iterator-seq obj)))
#?(:clj (defmethod hyperfiddle.hfql2.protocols/--datafy java.util.Enumeration [obj] (enumeration-seq obj)))
#?(:clj (prefer-method hyperfiddle.hfql2.protocols/--datafy java.util.Iterator java.util.Enumeration)) ;; ZipFile$ZipEntryIterator is both an Iterator and Enumeration.

#?(:clj
   (extend-protocol Datafiable
     java.util.Spliterator
     (-datafy [sp] (iterator-seq (java.util.Spliterators/iterator sp)))

     java.util.stream.BaseStream
     ;; streams are Closable, who's in charge of closing them? when?
     (-datafy [s] (iterator-seq (.iterator s))) ; clojure 1.12 has `stream-seq!` but we chose retrocompat.
     ))

#?(:cljs
   (extend-protocol Datafiable
     ES6Iterator
     (-datafy [o] (es6-iterator-seq o))))

;;; Safe in-memory search

(defn search-matches "State if a given object matches a given `needle` object, usually a string. E.g.:
  (search-matches \"hello\" \"ll\") := true
  (search-matches :hello \"ll\") := true
  (search-matches 'hello \"ll\") := true
  (search-matches true \"ru\") := true

  Extensible on any object by implementing `hyperfiddle.hfql2.protocols/Searchable`.

  Default behavior:
  Will scan collections in O(n) with the following constraints:
  - only lookup of for a maximum of `*search-max-collection-size*` items – default 100.
  - only search `*search-max-depth*` levels deep – default 3.

  Will only match on the first `*search-max-str-len*` characters of a string – default 200.
  All `hyperfiddle.hfql2.protocols/*search-max-...` parameters are customizable with `binding`.

  Implementations must respect thread interruption.
  "
  [object needle] (hyperfiddle.hfql2.protocols/search-matches object needle))

(defn- safe-str-match
  "Match a string against needle, truncating to MAX-STR-LEN."
  [s needle]
  (strx/includes-str? (subs s 0 (min (count s) *search-max-str-len*)) needle))

(defn- some-interruptible
  "Like `some`, but checks for thread interruption between items on JVM."
  [pred coll]
  #?(:clj (loop [s (seq coll)]
            (when (and s (not (Thread/interrupted)))
              (or (pred (first s))
                  (recur (next s)))))
     :cljs (some pred coll)))

(defn- search-matches-map? [m needle]
  (and (counted? m)
    (boolean
      (or ; scan keyset first then scan values – reasoning is keys are quick to match. Values less so.
        (some-interruptible #(search-matches % needle) (take *search-max-collection-size* (keys m)))
        (some-interruptible #(search-matches % needle) (take *search-max-collection-size* (vals m))) ; same order as keys is guaranteed.
        ))))

(defn- search-matches-collection? [coll needle]
  (and
    (counted? coll)
    (seqable? coll)
    (boolean (some-interruptible #(search-matches % needle) (take *search-max-collection-size* coll)))))

(extend-protocol Searchable
  nil
  (-search-matches [_ _needle] #_(= "nil" needle) false) ; Enable if searching for nil is desirable.

  #?(:clj Throwable :cljs js/Error)
  (-search-matches [ex needle]
    (boolean
      (or (-search-matches (ex-message ex) needle)
          (search-matches (ex-data ex) needle)
          (search-matches (ex-cause ex) needle))))

  #?(:clj String :cljs string)
  (-search-matches [s needle] (safe-str-match s needle))

  #?(:clj clojure.lang.Keyword :cljs cljs.core/Keyword)
  (-search-matches [k needle] (-search-matches (str k) needle))

  #?(:clj clojure.lang.Symbol :cljs cljs.core/Symbol)
  (-search-matches [s needle] (-search-matches (str s) needle))

  #?(:clj Number :cljs number)
  (-search-matches [n needle] (-search-matches (str n) needle))

  #?(:clj Boolean :cljs boolean)
  (-search-matches [b needle] (-search-matches (str b) needle))

  ;; Maps: recurse into keys and values
  #?@(:clj [clojure.lang.IPersistentMap
            (-search-matches [m needle] (search-matches-map? m needle))])

  #?@(:cljs [cljs.core/PersistentHashMap
             (-search-matches [m needle] (search-matches-map? m needle))])
  #?@(:cljs [cljs.core/PersistentArrayMap
             (-search-matches [m needle] (search-matches-map? m needle))])

  #?@(:clj [java.util.UUID
            (-search-matches [u needle] (-search-matches (str u) needle))

            java.util.Date
            (-search-matches [d needle] (-search-matches (str d) needle))

            Class
            (-search-matches [c needle] (-search-matches (.getCanonicalName c) needle))

            ;; Other collections: recurse into elements
            clojure.lang.IPersistentCollection
            (-search-matches [coll needle] (search-matches-collection? coll needle))

            ;; Default: match against type name only (never prints object)
            Object
            (-search-matches [o needle] (-search-matches (class o) needle))]

      :cljs [cljs.core/UUID
             (-search-matches [u needle] (-search-matches (str u) needle))

             js/Date
             (-search-matches [d needle] (-search-matches (str d) needle))

             ;; Vectors
             cljs.core/PersistentVector
             (-search-matches [coll needle] (search-matches-collection? coll needle))

             ;; Lists
             cljs.core/List
             (-search-matches [coll needle] (search-matches-collection? coll needle))

             ;; Sets
             cljs.core/PersistentHashSet
             (-search-matches [coll needle] (search-matches-collection? coll needle))

             ;; Default: match against type name only (never prints object)
             default
             (-search-matches [o needle] (-search-matches (pr-str (type o)) needle))]))
(ns hyperfiddle.hfql2.interpreter
  "
  Defines how to interpret AST nodes with corresponding effects. Implement all of these effects.
  Defines the HFQLT type, a Cofree Transformer (aka. CofreeT) over the HFQL AST.
  Provides:
  - how to transform and HFQL AST into an HFQL navigator,
  - how to `seed` the resulting navigator instance with a value,
  - how to traverse (navigate) the resulting tree, either:
    - step by step (lazily),
    - eagerly by pulling, as in Datomic Pull.

  An HFQL \"navigator\" is a Cofree Transformer instance. It is traversable, and shaped as a rose tree:
  - starting with a value (the seed), and a HFQL query (AST), one can ask for the computed value at point, and for eventual children.
  - the value at point and children at point are thunked. Both are realized on-demand.
  - children, if any, are of the same uniform HFQLT type – this respects the Cofree type.
  - depending of the query, asking for the value at point, or for children at point may run IO.
  - IO is cached, as in Clojure's lazy-seq, and behaves the same wrt GC.

  The HFQLT type is equivalent in shape as `(CofreeT (Pair Metadata, (Store focus Env)))`, where:
  - `Env` is a map of bindings from symbols to values, as in a dynamic scope.
  - `Store focus Env` is a Store comonad instance running the `focus` function on `Env`, on demand.
  - `focus :: Env -> (Pair value childrenTs)` reads `Env`, may run IO, and produce the value at point + next CofreeT steps.
  - `childrenTs :: [HFQLT]` is a lazy sequence where elements are of the same CofreeT type as the root, recursively up to leaves.
  - `(Pair Metadata Store)` is a static layer containing metadata describing the current CofreeT computation step, held in Store
    (thunked). Metadata contains the current point's original AST and various annotations describing what value running the Store instance
    will produce. A use case is to draw a UI skeleton ASAP, of the correct shape, while the value is being computed in the
    background. HFQLT implements IMeta/IObj — use `(meta stepT)` to access static metadata and `(vary-meta stepT f args)` to update it.

  Seeding an HFQLT is optional and depends on the HFQL query:
  - An HFQL query expecting arguments (e.g. {(user-by-id %) [:user/email]}) must be seeded. This query is said to be \"partial\".
  - An fully-provided HFQL query (non-partial) can be traversed without a seed. E.g. {(range 10) inc}.
  Non-partial HFQL queries are rare, often teaching materials, if not toys. UIs being about bidirectional IO with the user, most
  real-world HFQL queries expects some arguments to be injected.

  Traversing an HFQLT instance is done through the the Cofree and Comonad APIs. For a given `stepT`:
  - the value for this step is: `(extract stepT)`
  - children for this step are: `(unwrap stepT)`

  IO behavior

  Some HFQL queries can be lazily traversed up to leaves without running any IO:
  ```
  {(current-logged-in-user) [:user/email :user/role]}
  ```
  In this query, one can traverse the CofreeT tree up to e.g. :user/role without running IO.
  But as soon as a value is `extract`ed out of :user/role, an upward IO cascade will happen:
  - computing :user/role requires knowing the result of `(current-logged-in-user)`.
  - running `(current-logged-in-user)` eventually causes IO. The result is cached.
  - :user/email won't be computed, as it is not required to compute :user/role.
  - if :user/email is then extracted too (on user request), no IO will happen, as `current-logged-in-user` as been cached.

  However, the following query will run IO on traversal:
  ```
  {(all-users) {* [:user/email :user/role]}}
  ```
  In this query, the multiplicity of `(all-users)` is known to be \"many\", but the actual count is unknown.
  In order to navigate to any eventual user in the resulting list and ask for its :user/role, HFQL must run `(all-users)`.
  `(all-users)` is expected to return a collection of zero, one or many values, but HFQL cannot guess how many will be returned
  without running the `all-users` function. `all-users` is non-partial, but if it was partial, this may also cause an upward IO cascade.

  Laziness

  To the extent possible, the list of children is lazily computed, their
  multiplicity may be infinite. Lazy-seq chunking is prevented as much as
  possible, such that children are realized one by one, not by chunks of 32.
  Children's respective metadata, however, is stored at the Cofree level and
  doesn't require evaluation.

  Re-seeding a already seeded tree resets caching. Re-seeding s a valid use case, and is used in practice.

"
  (:refer-clojure :exclude [extend resolve meta bound-fn*])
  (:require
   [contrib.data :as cd :refer [group-by-pred]]
   #?(:clj [clojure.tools.logging :as log])
   [hyperfiddle.ct.functor :refer [Functor fmap]]
   [hyperfiddle.ct.pair :refer [pair]]
   [hyperfiddle.ct.monoid :refer [mappend]]
   [hyperfiddle.ct.comonad :refer [Comonad extract duplicate extend -extract]]
   [hyperfiddle.ct.cofree :refer [ComonadCofree unwrap ICofreeT runCofreeT ->CofreeF ->CofreeT headF tailF tree-seqT]]
   [hyperfiddle.ct.rose-tree :refer [mapHead]]
   [hyperfiddle.ct.store :refer [store seek pos #?(:cljs Store)]]
   [hyperfiddle.ct.monad-reader :refer [reader local runReader memoize-1]]
   [hyperfiddle.ct.monad :refer [=<<]]
   [hyperfiddle.ct.either :as either :refer [either? left?]]
   [hyperfiddle.hfql2 :as-alias hfql]
   [hyperfiddle.hfql2.protocols :as hfqlp]
   [hyperfiddle.rcf :refer [tests tap %]]
   [hyperfiddle.hfql2.ast :as ast]
   [hyperfiddle.hfql2.analyzer :as ana]
   [hyperfiddle.hfql2.utils :refer [has-meta? original-object]]
   [hyperfiddle.ct.experiments.thunk :as thunks :refer [thunk thunk* thunk? force-thunk]])
  #?(:clj (:import [hyperfiddle.ct.store Store]))
  #?(:cljs (:require-macros [hyperfiddle.hfql2.interpreter :refer [-hfql]])))

(defn safe-ccp-nav [object key value]
  (when (some? object) ; nil check – nav is not defined on nil, on purpose
    (hfqlp/nav object key value)))

(def ^:dynamic *navigate* safe-ccp-nav) ; default is to navigate objects, except in UIs where, at leaves, where we want to see the symbolic values

;; Deferred var ref for hfql2 dynvar — hfql2 loads interpreter (not vice-versa),
;; so this var exists by the time the delay is forced at runtime.
#?(:clj (def ^:private limit-var   (delay (find-var 'hyperfiddle.hfql2/*limit*))))
#?(:clj (def ^:private default-var (delay (find-var 'hyperfiddle.hfql2/*default*))))

(defn- meta [object]
  (when (has-meta? object)
    (clojure.core/meta object)))

(defn- resolve
  "Lookup `symbol` into `env` and return associated value. Throw if symbol cannot be found in `env`."
  ([symbol] (partial resolve symbol))
  ([symbol env]
   (if-let [[_symbol value] (find env symbol)]
     value
     (throw (ex-info (str "Unable to resolve `" symbol "` from environment.") {:symbol symbol, :env env}))))
  ([symbol env not-found]
   (try (resolve symbol env)
        (catch #?(:clj Throwable, :cljs :default) _ex
          not-found))))

(defn thunked "thunked :: (a -> b) -> a -> Thunk b
  For non-::none strategies, captures the current binding frame so thunk bodies
  see dynamic bindings that were active when thunked-f was applied (e.g. from
  inject-dynamic-bindings)."
  ([f] (thunked ::thunks/none f))
  ([strategy f]
   (fn thunked-f [a]
     (let [f #?(:clj (if (= ::thunks/none strategy) f (clojure.core/bound-fn* f))
                :cljs f)]
       (thunk* strategy (fn [] (force-thunk (f (force-thunk a)))))))))

(tests

  ;; deref ∘ thunked f = f
  ((thunked inc) 1)
  (thunk? *1) := true
  (deref *2) := 2

  ;; thunked with explicit strategy
  ((thunked ::thunks/weak inc) 1)
  (thunk? *1) := true
  (deref *2) := 2

  ;; thunked ∘ thunked = thunked
  ((thunked (thunked inc)) 1)
  (thunk? *1) := true
  (deref *2) := 2
  )

(defn- ensure-either [%]
  (cond
    (thunk? %) (fmap ensure-either %) ; either is already inside
    (either/either? %) %
    :else (either/right %)))

(def ^:private focus-% (local (thunked (comp ensure-either (resolve '%)))))

(defn resolve-arguments
  ([ast-node] (partial resolve-arguments ast-node))
  ([ast-node env]
   (try
     (let [args (mapv (fn [parameter argument]
                        (if (symbol? parameter)
                          (let [resolved (try (either/right (ensure-either (resolve parameter env)))
                                              (catch #?(:clj Throwable, :cljs :default) err
                                                (either/left err)) )]
                            (either/either
                              either/left
                              (fn [b] (if (= '% parameter) (force-thunk b) b))
                              resolved))
                          (if (= ::unbound argument)
                            (either/left (ex-info "Unbound function argument" {:parameter parameter}))
                            (either/right argument))))
                  (ast/parameters ast-node) (concat (ast/arguments ast-node) (repeat ::unbound)))]
       (either/sequenceEither args))
     (catch #?(:clj Throwable, :cljs :default) err
       (either/left err)))))

(def ^:private focus-arguments (comp local thunked resolve-arguments))

(defn- unwrap-either [e] (either/either identity identity e))

(defn- template-symbolic-edge [env ast-node & [point-free?]]
  (let [edge (ast/edge ast-node)
        form (ast/form edge)
        parameters (ast/parameters edge)]
    (case (ast/node-type edge)
      ::ast/star nil ; star is syntax, not a named edge
      ::ast/call (cons (first form)
                   (map (fn [parameter argument]
                          (if (or (:symbolic (meta parameter)) (:symbolic (meta argument)))
                            parameter
                            argument))
                     parameters (unwrap-either (resolve-arguments edge env))))
      ::ast/symbol (cond
                       point-free? form
                       :else (list form (unwrap-either (force-thunk (resolve '% env '%))))) ; introduce a point
      form)))

(declare hfql?)

(defn- symbolic-edge-static [ast-node]
  (ast/symbolic-edge
   (case (ast/node-type ast-node)
     ::ast/traversal (ast/edge ast-node)
     ast-node)))

(defn symbolic-edge ; FIXME uniformize
  [point & {::keys [point-free] :or {point-free true}}]
  (let [[ast-node scope] (if (hfql? point)
                            [(meta point) (pos (runCofreeT point))]
                            [point (::scope point)])]
    (case (ast/node-type ast-node)
      ::ast/traversal (template-symbolic-edge scope ast-node point-free)
      ;; ::ast/call (template-symbolic-edge (::scope ast-node) ast-node point-free) ; TODO
      (symbolic-edge-static ast-node))))

(def ^:private mem-reader "Like `monad-reader/reader` but the read function only reruns if the environment has changed." (comp reader memoize-1))

(defn- fmap-original-objects [f objs]
  (let [ret (f (map original-object objs))
        recovered (reduce (fn [r obj]
                            (if (identical? ret (original-object obj))
                              (reduced obj)
                              r))
                    ::not-found
                    objs)]
    (if (identical? ::not-found recovered)
      (hfqlp/datafy ret)
      recovered)))

(defn- fmap-original-object [f obj]
  (fmap-original-objects (comp f first) [obj]))

#?(:clj
   (tests
     (let [obj (hfqlp/datafy (.iterator (java.util.Collections/singleton 1)))]
       (fmap-original-objects (fn [args] (apply identity args)) [obj])
       := obj)

     (let [obj (hfqlp/datafy (.iterator (java.util.Collections/singleton 1)))]
       (fmap-original-object identity obj)
       := obj)
     ))

(defn- getter-lookup-key
  "Return the key to use for map lookup, dispatching on AST node type.
  Uses `node-value` which returns the raw runtime value without options wrapping."
  [ast-node]
  (case (ast/node-type ast-node)
    ::ast/quoted-var (ast/node-value ast-node)
    ::ast/symbol (ast/form ast-node)            ; macro symbols routed here — use the raw symbol
    (ana/unquote* (ast/node-value ast-node))))

(defn- io-operation
  "Shared IO-wrapping skeleton for getter and call. `f` receives the resolved
  value(s) and returns the computed result. The wrapper handles memoized reading,
  weak caching, thunking, and Either error handling.
  `xform` (any -> any) is applied to the result after `f`, inside the try-catch
  (exceptions become Left). Defaults to `identity`."
  ([f] (io-operation f identity))
  ([f xform]
   (mem-reader
     (thunked ::thunks/weak
       (=<<
         (fn [v]
           (try
             (either/right (xform (f v)))
             (catch #?(:clj Throwable, :cljs :default) err
               (either/left err)))))))))

(defn- getter [ast-node]
  (let [key         (getter-lookup-key ast-node)
        xform       (or (::hfql/xform (extract ast-node)) identity)
        default-val (::hfql/default (extract ast-node))]
    (io-operation
      (fn [%]
        #?(:clj (log/debug "[IO]" `getter key))
        (fmap-original-object
          (fn [%] (*navigate* % key (get % key default-val)))
          %))
      xform)))

(defn- call [ast-node]
  (let [call! (if (= ::ast/keyword-fn (ast/kind ast-node))
                (let [key         (ast/form-resolved ast-node)
                      default-val (::hfql/default (extract ast-node))] ; honor ::hfql/default for keyword-fn dispatch
                  #(*navigate* %1 key (get %1 key default-val)))
                (let [f (ast/node-value ast-node)] ; Meaning :db/id and #(:db/id %) will navigate, but #(get % :db/id) won't.
                  #(apply f %&)))
        xform (or (::hfql/xform (extract ast-node)) identity)]
    (io-operation
      (fn [args]
        #?(:clj (log/debug "[IO]" `call (ast/symbolic-edge ast-node)))
        (fmap-original-objects (fn [args] (apply call! args)) args))
      xform)))

;; star-keys handles error detection via type guard, so touch is just either/right.
(def ^:private star (mem-reader (thunked (=<< either/right))))

(defn- bound-fn*
  ([f] #?(:clj (clojure.core/bound-fn* f)
          :cljs (throw (ex-info "Unsupported platform" {}))))
  ([bindings f]
   #?(:clj
      (with-bindings* bindings clojure.core/bound-fn* f)
      :cljs f ; TODO
      )))

(defn- leaf
  ([ast-node] (partial leaf ast-node))
  ([ast-node Ra]
   (->> (reader (fn [env] ((bound-fn* {} (runReader Ra)) env)))
     (fmap (fn [v] (pair v ()))))))

(declare seed)

;; Quote forms that collide with HFQL syntax when used as runtime-discovered pull keys.
;; Numbers look like recursion limits, `*` looks like star-pull, `...` looks like recursion.
;; Macros and special forms are handled in the coalgebra via macro-or-special-symbol?.
(defn- disambiguate-from-hfql-syntax [form]
  (cond
    (nat-int? form)    (list 'quote form)
    (#{'* '...} form)  (list 'quote form)
    :else              form))
(defn- undisambiguate-from-hfql-syntax [form]
  (if (and (ana/quoted? form)
           (let [inner (second form)]
             (or (nat-int? inner) (#{'* '...} inner))))
    (ana/unquote* form)
    form))

(defn- pull-peek-at-immediate-kids! [env kidsT]
  ;; Unless I'm mistaken, this is only a test convenience utility.
  ;; A proper UI wouldn't ask for a pull value at point but
  ;; would "just" render a key-value layout and iterate on
  ;; children. Static keys are already available in metas. Dynamic
  ;; keys are available on children metas. So in a UI there's no
  ;; need to build a map here, unless there's a use case to render
  ;; the raw value at point (unpulled), e.g. for an EDN <pre>,
  ;; which may be no different from of test utility?
  (try
    (either/right
      (into {} (map pair (fmap (comp undisambiguate-from-hfql-syntax symbolic-edge meta) kidsT)
                 (->> kidsT
                   (fmap (seed env))
                   (fmap runCofreeT)
                   (fmap extract) ; store
                   (fmap headF)   ; inner layer
                   (fmap force-thunk)
                   (fmap (either/either identity identity))
                   ))))
    (catch #?(:clj Throwable, :cljs :default) err
      (either/left err))  ))

(declare unfoldT)

(defn- recursion-point? [hfql]
  (or (ast/recur-node? hfql)
    (ast/recursion-point? hfql)))

(defn- direct-recursion-point? [hfql]
  (and (recursion-point? hfql)
    (or (ast/recur-node? hfql)
      (ast/star-traversal? hfql))))

(defn- star-keys
  "Attempt to get dynamic pull keys for `[*]` on a supposedly map-like object.
Does not call into datafy as we want datafy calls to be explicit from userland.
Pulling on non-maps (non associative) is perfectly fine.
  E.g.: in [File/.getName *], file is not a map so star will produce no keys.
  The alternative would be to throw. We prefer to let the user datafy beforehand and then call `*`.
  "
  [object]
  (->> (force-thunk (runReader star object)) ; expects an IPersistentMap or a sequences of MapEntries
    (either/either
      (constantly ())
      (fn [associative]
        (try
          (key (first associative)) ; type guard: fail fast if not map entries
          (->> associative
            (map key) ; do not use `keys` here. `doall` doesn't work as expected on it. See following RCF.
            (map disambiguate-from-hfql-syntax))
          (catch #?(:clj Throwable, :cljs :default) _ ()))))))

(tests
  "clojure.core/keys is a sneaky dodger" ; `vals` too and potentially others.
  ;; based case – nothing unexpected
  (keys {}) := nil
  (keys {:a 1}) := '(:a)
  ;; Less known but valid usage: keys accepts sequences of map entries – see keys' docstring.
  (keys (seq {})) := nil
  (keys (seq {:a 1})) := '(:a)
  (keys (list (first {:a 1}))) := '(:a)
  ;; keys return a lazy-seq
  (seq? (keys {:a 1})) := true
  (list? (keys {:a 1})) := false

  ;; Problem: doesn't fail early when passed a sequence of non-map entries
  ;; This is understandable given `keys` produce a lazy-seq.
  (count (keys '(x))) := 1
  ;; BUT! `doall` doesn't force it as we would expect!
  (count (doall (keys '(x)))) := 1
  (count (doall (keys '(x y)))) := 2 ; proof that doall realized the first slot without causing a crash.
  ;; But peeking at the first element does throw. Why?
  ;; Note (do ... nil) is to bypass REPL printing, as it would force the value.
  ;; This proves that `first` throws, but neither `seq`, `next`, `rest` nor `count` does.
  (do (first (keys '(x y))) nil) :throws ClassCastException

  ;; Only `into` will eagely realize the collection
  (do (into [] (keys '(x))) nil)
  :throws ClassCastException

  ;; Explanation:
  ;; TL;DR:
  ;;  - `keys` is NOT `(map key (seq m))`
  ;;  - `keys` is an iterator transformer. Interpreting its input seq values on `first`, not on `seq` nor `next`.
  ;;
  ;; `keys` return an `APersistentMap$KeySeq` (https://github.com/clojure/clojure/blob/6a4ba6aedc8575768b2fff6d9c9c7e6503a0a93a/src/jvm/clojure/lang/APersistentMap.java#L140)
  ;; KeySeq is a lazy-seq over SUPPOSED map entries, casting to MapEntry on call to `.first()`
  ;; So `.next().next().next()...` only walk over the input seq, without interpreting values as map entries.
  ;; We can say that `KeySeq` separates the spine from values.
  ;; Calling first runs this code: `((Map.Entry) seq.first()).getKey();`, causing a ClassCastException if the input was not a map or a seq of MapEntry objects.

  ;; Less surprising implementation, but not strictly equivalent:
  (do (doall (map key '(x))) nil)
  :throws ClassCastException

  nil)

(defn- inject-dynamic-bindings
  "Transform a Reader (env -> a) such that when run, the value associated to
  `hyperfiddle.hfql2/*bindings*` in `env` is installed in dynamic scope. The `*bindings*` value is
  expected to be compatible with `clojure.core/with-bindings*`. I.e. a map of
  `{#'var value, ...}`.
  When `ast-node` has :limit or :default (from options syntax or metadata),
  their values are bound to the corresponding hfql2 dynvars."
  [ast-node Ra]
  (mem-reader ; no need to memoize/delay here, all possible IO is in `Ra`, let Ra deal with it.
    (fn [env]
      (let [bindings (get env `hfql/*bindings* {})
            #?@(:clj [a        (extract ast-node)
                       bindings (cond-> bindings
                                  (:hyperfiddle.hfql2/limit a)
                                  (assoc @limit-var (:hyperfiddle.hfql2/limit a))
                                  (:hyperfiddle.hfql2/default a)
                                  (assoc @default-var (:hyperfiddle.hfql2/default a)))])]
        ((bound-fn* bindings (runReader Ra)) env)))))

(defn- insert-at [index collA collB] (concat (take index collA) collB (drop index collA)))

(defn- find-recursion-point [hfql]
  (let [ancestors (->> (iterate ::parent hfql)
                    (take-while some?)
                    (take-while ast/partial?))
        nearest-root (some #(when (ast/recursion-root? %) %) ancestors)]
    (ast/mark-recursion-root (or nearest-root (last ancestors)))))

(defn- recurse-or-stop
  "Given an hfql node and a child node that may carry a recursion limit, find
  the recursion root and return it with a decremented limit. Returns nil if the
  limit is exhausted."
  [hfql limit-source]
  (let [recursion-root (find-recursion-point hfql)
        limit (or (ast/recursion-limit recursion-root)
                  (ast/recursion-limit limit-source)
                  0)]
    (when (pos? limit)
      (mapHead #(assoc % ::ast/recursion-limit (dec limit)) recursion-root))))

(defn- recursion-children
  "Check recursion limit on `limit-source` and either produce [wrapped-root] or [].
  `wrap-fn` transforms the root before wrapping (e.g. ast/mark-recursive-child)."
  [ast-node limit-source wrap-fn]
  (if-let [root (recurse-or-stop ast-node limit-source)]
    [(wrap-fn root)]
    []))

(defn- link-parent
  "Annotate `child` with a parent reference to `parent-node`."
  ([parent-node] (partial link-parent parent-node))
  ([parent-node child] (assoc child ::parent parent-node)))

(defn- resolve-pull-children
  "Given pull's children AST nodes and the current % value, compute ordered
  static+dynamic children and an optional recursion point. Returns
  {:keys [ordered-kids recur-point]}.
  Empty children default to star behavior (`[] ≅ [*]`), favoring REPL-driven
  exploratory DX. See monoidal rules in the body comment."
  [children percent]
  (let [[[star?] kids] (if (empty? children)
                         ;; `[]` defaults to `[*]` for DX — see monoidal rules:
                         ;; `[] ≅ [*]`, `[] ≠ [*]`, `[:a] ⊗ [] = [:a] ≇ [:a *]`.
                         [[true] ()]
                         (group-by-pred ast/star? children))
        star-position (if (empty? children)
                        0
                        (or (ffirst (filter (comp ast/star? second) (map-indexed vector children)))
                          ##Inf))
        dynamic-keys (when star? (star-keys (ensure-either percent)))
        star-kids (let [static-keys (set (map symbolic-edge-static kids))
                        remaining-star-keys (remove static-keys dynamic-keys)]
                    ;; Analyze each key individually instead of as a pull vector.
                    ;; Preserves laziness — no vec, no pull wrapping/unwrapping.
                    (map (ana/analyze {}) remaining-star-keys))
        recur-point (first (filter direct-recursion-point? (concat kids star-kids)))
        kids (remove direct-recursion-point? kids)
        star-kids (remove direct-recursion-point? star-kids)]
    {:ordered-kids (insert-at star-position kids star-kids)
     :recur-point recur-point}))

(defn- traversal-continuation
  "Given a traversal ast-node and its direct child (already parent-linked),
  compute the children vector. Handles star-unwrapping and recursion."
  [ast-node direct-child]
  (if (ast/star-traversal? direct-child)
    ;; Unwrap star children first, then handle recursion here
    ;; (not downstream) so ::recur nodes stay invisible to pull_algebra.
    (let [unwrapped (first (unwrap direct-child))
          unwrapped (link-parent ast-node unwrapped)]
      (if (ast/recur-node? unwrapped)
        (recursion-children ast-node unwrapped identity)
        [unwrapped]))
    ;; Non-star child: handle recursion at traversal level if needed.
    (if (recursion-point? ast-node)
      (recursion-children ast-node direct-child identity)
      [direct-child])))

(defn hfql-coalgebra
  "A coalgebra transforming one HFQL AST node (a RoseTree) into an effectful Reader.
  When run, this Reader yields (Pair value children), where:
  - value is the computed result for this node (often a thunk).
  - children are the AST nodes to unfold next. There may be none (leaf nodes).

  The `unfoldT` function is in charge of:
  - running the Reader with a seeded environment (scope),
  - recursively applying hfql-coalgebra onto returned child nodes,
  - wrapping the result into an HFQLT (CofreeT-backed navigator).

  Note: pull and traversal branches pre-unfold some children internally
  (e.g. peek-at-immediate-kids, edge traversals) before returning them
  alongside the remaining AST children for unfoldT to process."
  [ast-node]
  (let [children (unwrap ast-node)
        transform
        (case (ast/node-type ast-node)
          ::ast/symbol        (if (ana/macro-or-special-symbol? ast-node)
                                (comp (leaf ast-node) focus-% getter)
                                (comp (leaf ast-node) (focus-arguments ast-node) call))
          ::ast/quoted-symbol (comp (leaf ast-node) focus-% getter)
          ::ast/keyword       (comp (leaf ast-node) focus-% getter)
          ::ast/string        (comp (leaf ast-node) focus-% getter)
          ::ast/literal       (comp (leaf ast-node) focus-% getter)
          ::ast/runtime-value (comp (leaf ast-node) (focus-arguments ast-node) call)
          ::ast/var           (comp (leaf ast-node) (focus-arguments ast-node) call)
          ::ast/quoted-var    (comp (leaf ast-node) focus-% getter)
          ::ast/call          (comp (leaf ast-node) (focus-arguments ast-node) call)
          ::ast/lambda        (comp (leaf ast-node) (focus-arguments ast-node) call)
          ::ast/star    (comp (leaf ast-node) focus-% (constantly star))
          ;; Resolve % from scope and wrap in Thunk(Either), matching the contract
          ;; of every other node type. Previously stored raw `env` here, which broke
          ;; nav*'s invariant that it must always return an Either.
          ::ast/recur   (constantly
                          (mem-reader
                            (fn [env]
                              (pair (thunk (try (ensure-either (resolve '% env))
                                               (catch #?(:clj Throwable :cljs :default) err
                                                 (either/left err))))
                                    [(find-recursion-point ast-node)]))))
          ::ast/pull
          (constantly
            (mem-reader
              (fn [env]
                (let [{:keys [ordered-kids recur-point]} (resolve-pull-children children (get env '%))]
                  (pair (thunk
                          (->> ordered-kids
                            (map (unfoldT hfql-coalgebra))
                            (pull-peek-at-immediate-kids! env)))
                    (->>
                      (mappend
                        ordered-kids
                        (if recur-point
                          (recursion-children ast-node recur-point ast/mark-recursive-child)
                          []))
                      ;; map (not mapv) — lazy children for large pull-star collections
                      (map (link-parent ast-node))))))))
          ::ast/traversal
          (let [lexical-symbol? (and (ast/symbol-node? (ast/edge ast-node))
                                     (not (fn? (ast/node-value (ast/edge ast-node)))))
                lexical-value (ast/node-value (ast/edge ast-node))
                edgeT (unfoldT hfql-coalgebra (cond (ast/star-traversal? ast-node) (ana/hfql identity)
                                                    lexical-symbol? (ana/hfql identity)
                                                    :else (ast/edge ast-node)))]
            (constantly
              (mem-reader
                (fn [env]
                  (let [env (force-thunk env)
                        env' (if lexical-symbol?
                              (assoc env '% lexical-value)
                              env)]
                    (pair (extract (seed env' edgeT))
                      (traversal-continuation ast-node (link-parent ast-node (first children))))))))))]
    (inject-dynamic-bindings ast-node (transform ast-node))))

(declare hfqlt-equals hfqlt-hash)

(deftype HFQLT [ast CofreeT]
  ICofreeT
  (-runCofreeT [_] (runCofreeT CofreeT))
  Functor
  (-fmap [fa f] (HFQLT. ast (fmap f CofreeT)))
  Comonad
  (-extract [t] (extract CofreeT))
  (-duplicate [this] (extend identity this))
  (-extend [this f]
    (let [inner-store (runCofreeT CofreeT)]
      (HFQLT. ast
        (->CofreeT
          (store
            (fn [env']
              (let [focused-store (seek env' inner-store)
                    cofreeF (extract focused-store)
                    focused-hfqlt (HFQLT. ast (->CofreeT focused-store))
                    result (f focused-hfqlt)
                    children (tailF cofreeF)
                    extended-children (fmap #(extend f %) children)]
                (->CofreeF result extended-children)))
            (pos inner-store))))))
  ComonadCofree
  (-unwrap [this]
    (fmap #(vary-meta % assoc ::hfql/parent this)
      (unwrap CofreeT)))

  #?(:clj clojure.lang.IMeta)
  #?(:clj (meta [_] ast))
  #?(:clj clojure.lang.IObj)
  #?(:clj (withMeta [_ m] (HFQLT. m CofreeT)))

  Object
  #?(:clj (equals [this other] (hfqlt-equals this other)))
  #?(:clj (hashCode [this] (hfqlt-hash this)))
  (toString [this]
    (let [scope (runCofreeT CofreeT)
          ast-node ast
          % (get (pos scope) '%)
          seeded? (some? %)
          realized? (or (and (thunk? %) (realized? %)) seeded?)
          error? (cond
                   (and (thunk? %) (clojure.core/realized? %)) (left? (force-thunk %))
                   (either? %) (left? %)
                   :else :unknown)]
      (str "#hfql/" (name (or (ast/node-type ast-node) :hfql/hfql)) " "
        {:form (ast/form ast-node)
         :seeded? seeded?
         :realized? realized?
         :error? error?})))

  #?(:cljs cljs.core/IEquiv)
  #?(:cljs (-equiv [this other] (hfqlt-equals this other)))
  #?(:cljs cljs.core/IHash)
  #?(:cljs (-hash [this] (hfqlt-hash this)))
  #?(:cljs cljs.core/IMeta)
  #?(:cljs (-meta [_] ast))
  #?(:cljs cljs.core/IWithMeta)
  #?(:cljs (-with-meta [_ m] (HFQLT. m CofreeT)))
  )

#?(:clj
   (defmethod print-method HFQLT [v ^java.io.Writer w]
     (.write w (.toString v))))

(defn ->hfqlT [ast fwa] (->HFQLT ast (->CofreeT fwa)))
(defn hfql? [obj] (instance? HFQLT obj))

(defn- hfqlt-equals [^HFQLT this ^HFQLT other]
  (or (identical? this other)
    (and (instance? HFQLT this) (instance? HFQLT other)
      (= (ast/node-id (.-ast this))
        (ast/node-id (.-ast other)))
      (= (pos (runCofreeT this))
        (pos (runCofreeT other))))))

(defn- hfqlt-hash [^HFQLT hfqlt] (hash (ast/node-id (.-ast hfqlt))))

(defn hoistHfqlT
  "Apply f to metadata at every node in the HFQLT tree. Lazy — children are only transformed when accessed."
  [f t]
  (let [inner-store (runCofreeT t)
        store' (fmap (fn [cofreeF]
                       (->CofreeF (headF cofreeF)
                                  (fmap #(hoistHfqlT f %) (tailF cofreeF))))
                     inner-store)]
    (->HFQLT (f (meta t)) (->CofreeT store'))))


(defn- limit-collection
  "Truncate coll to at most limit items, preserving metadata.
  When limit is ##Inf, returns the seqed collection as-is."
  [limit coll]
  (let [s (cd/to-seq coll)]
    (if (infinite? limit)
      s
      (with-meta (take limit s) (clojure.core/meta coll)))))

(defn unfoldT
  "Build an HFQLT navigator from an AST (rose tree) using a coalgebra.
  The coalgebra transforms each AST node into a Reader Env (Pair value children).
  The result is an HFQLT instance wrapping a Store comonad — seed it with an
  environment to traverse."
  ([coalgebra] (partial unfoldT coalgebra))
  ([coalgebra ast-node]
   (let [step (coalgebra ast-node)] ; TODO should this move inside of the store's function below?
     (->hfqlT (assoc ast-node ::static-children
                (map (comp (unfoldT coalgebra) #(assoc % ::parent ast-node))
                  (unwrap ast-node)))
       (store
           (fn peek [scope]
             ;; IO issue: scope can change due to dynamic bindings (e.g. search
             ;; needle). Even if `step` is memoized, a new ->CofreeF will be
             ;; built. And under * traversal IO will be forced to produce a new
             ;; list of children. FIXME, memoize the CofreeF instance and
             ;; rebuild it only if `value` or `children` did change.
             ;; #?(:clj (log/debug "Unwrapping" (ast/symbolic-edge (or (ast/edge ast-node) ast-node))))
             (let [[value children] (runReader step scope)]
               (->CofreeF value
                 (let [value (thunk ::thunks/hard (extract value))] ; cache branch values, discarded when no leaves are referencing it, as per lazy-seq semantics, recursively up to the root.
                   (if-not (ast/multiplicity-many? ast-node) ; if multiplicity one
                     ;; don't use fmap under here - we want to enforce lazy children.
                     (map ; for each child, unfold it and seed it with next scope.
                       (comp (seed (case (ast/node-type ast-node) ; decide what's next scope.
                                     ::ast/pull scope ; a "pull" point doesn't alter '% – it doesn't run anything. Only collects kvs.
                                     (assoc scope '% value)))
                         (unfoldT coalgebra))
                       children)
                     (if (empty? children) ; if recursion is bounded and over, * won't have more children
                       ()
                       ;; Unfold next child once, then fan-out by seeding it multiple times in parallel, once for each value. Put each value in scope as '%.
                       (let [childT (unfoldT coalgebra (first children))] ; There can only be one child, because fan-outs happen on traversals only and a traversal only has one continuation. No IO — builds template from AST.
                         (lazy-seq ; defer IO until children are actually iterated
                           (let [forced (do #?(:clj (log/trace "Running IO for * traversal." (ast/symbolic-edge (or (ast/edge ast-node) ast-node))))
                                            (either/either
                                              (constantly ()) ; stop navigation at first error
                                              identity
                                              (force-thunk value) ; forces effects up to the root to compute children multiplicity
                                              ))
                                 limit  (::hfql/limit (extract ast-node) ##Inf)
                                 forced (limit-collection limit forced)]
                             (map-indexed
                               (fn [positional-index value_i]
                                 (let [% (thunk ::thunks/weak
                                           (try
                                             (either/right (safe-ccp-nav forced nil value_i))
                                             (catch #?(:clj Throwable, :cljs :default) err
                                               (either/left err))))]
                                   (-> (seed (assoc scope '% %) childT)
                                     (vary-meta assoc ::hfql/positional-index positional-index))))
                               (cd/to-seq forced)))))))))))
           (ast/lexical-env ast-node))))))

(defn seed
  "Sets the environment of an HFQLT instance. Any point of the tree can be re-seeded. Re-seeding busts cached IO."
  ([env] (partial seed env))
  ([env t]
   (->hfqlT (assoc (meta t) ::scope env)
            (seek env (runCofreeT t)))))

(defmacro -hfql [hfql] `(unfoldT hfql-coalgebra (ana/hfql ~hfql)))

(defn- -values [T] (map (either/either identity identity) (map force-thunk (map extract (tree-seqT T)))))

(defn- _inc [x] (prn 'IO! '_inc x) (inc x))

(tests
  (let [% 1]
    (-values (-hfql (_inc %))))
  := '(2)

  (-values (seed {'% 1} (-hfql _inc))) 
  := '(2)

  (let [% 1]
    (-values (-hfql _inc) ))
  := '(2)

  (-values (seed {'% {:a 1}} (-hfql :a))) 
  := '(1)

  (-values (seed {'% {:a 1, :b 2}} (-hfql [:a]) )) 
  := '({:a 1} 1)
 

  (-values (seed {'% {:a 1, :b 2}} (-hfql [:a *]) )) 
  := '({:a 1, :b 2} 1 2)

  (-values (seed {'% {1 :a, 2 :b}} (-hfql ['1 *])))
  )

(tests
  "Column order is respected in presence of star"
  (-values (seed {'% {:a 1, :b 2, :c 3}} (-hfql [:a *])))
  := '({:a 1, :b 2, :c 3} 1 2 3)

  (-values (seed {'% {:a 1, :b 2, :c 3}} (-hfql [* :a])))
  := '({:a 1, :b 2, :c 3} 2 3 1)

  (-values (seed {'% {:a 1, :b 2, :c 3}} (-hfql [:a * :c])))
  := '({:a 1, :b 2, :c 3} 1 2 3)

  (-values (seed {'% {:a 1, :b 2, :c 3, :d 4}} (-hfql [:a :b * :d])))
  := '({:a 1, :b 2, :c 3, :d 4} 1 2 3 4)

  (-values (seed {'% {:a 1, :b 2, :c 3, :d 4}} (-hfql [:a * :c :d])))
  := '({:a 1, :b 2, :c 3, :d 4} 1 2 3 4)
  )

(defrecord _Box [x])

(tests
  (->_Box :a) := (->_Box :a)
  )

;; (-values (seed {'% {:a 1, 2 3}} (-hfql [:a 2])))


#?(:clj
   (tests
     (-values (seed {'% {:a 1, (->_Box :b) 2}} (-hfql [:a *]) ))
     := (list {:a 1, (->_Box :b) 2} 1 2)))

(tests
  (-values (seed {'% {:a 1, :b 3}} (-hfql {:a inc}) )) 
  := '(1 2)

  (-values (seed {'% {:a 1, :b 3}} (-hfql [{:a inc}])))
  := '({:a 1} 1 2)

  (-values (seed {'% 1} (-hfql {inc inc})))
  := '(2 3)

  (-values (seed {'% 1} (-hfql {_inc {_inc {_inc _inc}}})))
  := '(2 3 4 5)

  (let [fork (fn [x] (list x x))]
    (-values (seed {'% '.} (-hfql {identity {fork {fork fork}}}))))
  := '(              .
                   (. .)
               ((. .) (. .))
       (((. .) (. .)) ((. .) (. .))))  ; 8 elements

  (let [fork (fn [x] (list x x))]
    (-values (seed {'% '.} (-hfql {identity {fork {* {fork {* fork}}}}})))) 
  := '(. (. .) (. .) (. .) (. .) (. .) (. .) (. .)) ; 8 elements
  ;; * doesn't flatten, it fans-out, `-values` is responsible for the apparent flattening. Values arbitrarily traverses in pre-order.

  )

(tests
  "calling fn in pull"
  (-values (seed {'% {:a 1, :b 2}} (-hfql [identity :a])))
  := [{'identity {:a 1, :b 2}, :a 1} {:a 1, :b 2} 1]
  )

(tests
  "traversal in pull"
  (-values (seed {'% {:a 1, :b "b"}} (-hfql [{:a inc} *])))
  := '({:a 1, :b "b"} 1 2 "b")
  )

(defn- extract-right-value-or-keep-left-error [x]
  (either/either
    either/left
    identity
    x))

(defn- pull-algebra [W children]
  (let [ast-node (meta W)]
    (case (ast/node-type ast-node)
      ::ast/pull
      (thunk
        (into {}
          (map (fn [W value]
                 (let [ast-node (meta W)]
                   (case (ast/node-type ast-node)
                     ::ast/traversal (force-thunk value)
                     (pair (symbolic-edge W ::point-free true) (original-object (force-thunk value))))))
            (unwrap W)
            children)))
      ::ast/traversal
      (thunk
        (let [key (symbolic-edge W ::point-free true)
              val (if (seq children) ; regular traversal always have children, but bounded recursion final point doesn't.
                    (if (ast/multiplicity-many? ast-node)
                      (map (comp original-object force-thunk) children)
                      (original-object (force-thunk (first children))))
                    ;; bounded recursion case, no more children, return value as-is.
                    (original-object (extract-right-value-or-keep-left-error (force-thunk (extract W)))))]
          (if (ast/star-traversal? ast-node) ; e.g. without this check {* [ns-name]} on (all-ns) would produce {nil (…)} because `*` is purely syntactic and is not a named point.
            val
            {key val})))
      (fmap extract-right-value-or-keep-left-error (extract W)))))

;; Compare with cofree/fold-postorder which recurses with plain `map`:
;;   (defn fold-postorder [φ w]
;;     (φ (extract w) (map (fold-postorder φ) (unwrap w))))
;; The plain version creates lazy seqs with no binding capture — safe when
;; consumed synchronously, but dynamic bindings are lost when the result
;; is iterated later outside the original `binding` context.
(defn- fold-postorder-bound
  "Like cofree/fold-postorder but captures the JVM thread binding frame at each
   level, so lazy child sequences restore it when iterated. Prevents dynamic
   binding loss when pull's lazy results are consumed outside the original
   binding context. On CLJS, falls back to plain recursion."
  ([φ] (partial fold-postorder-bound φ))
  ([φ w]
   (let [recurse #?(:clj (clojure.core/bound-fn* (fold-postorder-bound φ))
                    :cljs (fold-postorder-bound φ))]
     (φ (extract w) (map recurse (unwrap w))))))

(defn- synthesize-bound
  "Like cofree/synthesize but uses fold-postorder-bound to capture the JVM thread
   binding frame. This ensures dynamic bindings active at synthesis time are
   preserved when lazy children in the result are iterated later, outside the
   original binding context."
  ([φ] (partial synthesize-bound φ))
  ([φ w] (extend (fold-postorder-bound φ) w)))

(defn pull "Equivalent of `datomic.api/pull`. Reshape an HFQL tree `W` into a post-order version of the same tree.
  Lazy in theory, eager in practice.
  `(pull W)` is O(1) and its result can be navigated just like `W`. The type is
  W a -> W a. But because the tree returned by pull is now in post-order,
  navigating onto the first value will eagerly traverse the entire tree.
  Further navigation will still be lazy, but will be produced from an already
  fully realized tree.

  Asking for all values of (pull W) means traversing the eagerly projected
  expression. This can be seen as a \"renavigation\" onto a precomputed tree.
  "
  [W] ; could be written point-free but adding a point produces better stacktraces
  ;; Note `pull` is defined in term of comonadic operations. `pull-algebra` is the
  ;; interpreter "zipping" values up the tree. The sole purpose of `pull-algebra` is to re-attach
  ;; keys onto projected values, producing a result of the same shape as the original symbolic
  ;; query.
  ((synthesize-bound pull-algebra) (duplicate W)))


(tests
  (force-thunk (extract (pull (seed {'% 1} (-hfql identity))))) 
  := 1

  (force-thunk (extract (pull (seed {'% {:a 1}} (-hfql :a)))))
  := 1

  (force-thunk (extract (pull (seed {'% {:a 1, :b 2}} (-hfql [:a])))))
  := {:a 1}

  (force-thunk (extract (pull (seed {'% {:a 1}} (-hfql [:a :b])))))
  := {:a 1, :b nil}

  (force-thunk (extract (pull (seed {'% {:a 1, :b 2}} (-hfql [:a :b])))))
  := {:a 1, :b 2}

  (force-thunk (extract (pull (seed {'% {:a 1}} (-hfql [:a identity])))))
  := {:a 1, 'identity {:a 1}}

  (force-thunk (extract (pull (seed {'% {:a 1}} (-hfql [{:a inc}]))))) 
  := {:a 2}

  (force-thunk (extract (pull (seed {'% {:a 1}} (-hfql [{:a {inc inc}}])))))
  := {:a {'inc 3}}

  (force-thunk (extract (pull (seed {'% {:a 1}} (-hfql {identity :a})))))
  := {'identity 1}

  (force-thunk (extract (pull (seed {'% {:a 1}} (-hfql {identity [:a]})))))
  := {'identity {:a 1}}

  (force-thunk (extract (pull (-hfql (range 3)))))
  := '(0 1 2)

  (-values (-hfql {(range 2) identity}))
  := '((0 1) (0 1))

  (-values (-hfql {(range 2) {* identity}}))
  := '((0 1) 0 1)

  (force-thunk (extract (pull (-hfql {(range 2) {* inc}})))) 
  := '{(range 2) (1 2)}

  (force-thunk (extract (pull (-hfql {(range 2) {* {inc inc}}}))))
  := '{(range 2) ({inc 2} {inc 3})}
  )

#?(:clj
   (tests
     (force-thunk (extract (pull (-hfql {(ns-publics *ns*) {* val}}) ))) 
     := {(list 'ns-publics *ns*) (vals (ns-publics *ns*))}
     ))

#?(:clj
   (tests
     (force-thunk (extract (pull (-hfql {(ns-publics *ns*) {* {val symbol}}}))))
     := {(list 'ns-publics *ns*) (map #(hash-map 'val %) (map symbol (vals (ns-publics *ns*))))}
     ))

#?(:clj
   (tests
     (force-thunk (extract (pull (-hfql {(ns-publics *ns*) {vals {* symbol}}}))))
     := {(list 'ns-publics *ns*) {'vals (map symbol (vals (ns-publics *ns*)))}}
     ))

#?(:clj
   (tests
     "Arguements tagged ^:symbolic do not template in resulting pull keys."
     (force-thunk (extract (pull (-hfql {(ns-publics ^:symbolic *ns*) {* val}}))))
     := {'(ns-publics *ns*) (vals (ns-publics *ns*))}
     ))


#?(:clj
   (tests
     "Multiplicity many traversals stays lazy, even through pull."
     (->> (-hfql {(ns-publics *ns*) {* (fn [%] (tap "IO!") (val %))}})
       pull ; CofreeT
       extract ; pulled value {ns-publics (...)}
       force-thunk
       first ; map entry
       val ; (...)
       (take 2)
       (doall))

     % := "IO!" ; we only see two "IO!" because of (take 2)
     % := "IO!"
     % := :hyperfiddle.rcf/timeout
     )
   )

(tests
  (unwrap-either (force-thunk (extract (seed {'% {:a 1}} (-hfql *))))) 
  := {:a 1}

  (unwrap-either (force-thunk (extract (seed {'% {:a 1}} (-hfql :a)))))
  := 1

  (unwrap-either (force-thunk (extract (seed {'% {:a 1}} (-hfql identity)))))
  := {:a 1}

  (unwrap-either (force-thunk (extract (seed {'% {identity 'identity}} (-hfql identity)))))
  := {identity 'identity} ; identity is called (not looked up), returns the map
  )

(tests
  (-values (seed {'% {:a 1, :b 2, :c 3}} (-hfql [:a :b])))
  := '({:a 1, :b 2} 1 2)
  )


(defn- -retraverse [pulledT] (map force-thunk (map extract (tree-seqT pulledT))))

(tests
  (-retraverse (pull (seed {'% {:a 1, :b 2, :c 3}} (-hfql :a))))
  := '(1)

  (-retraverse (pull (seed {'% {:a 1, :b 2, :c 3}} (-hfql [:a :b]))))
  := '({:a 1, :b 2} 1 2)
  )

(tests
  (-retraverse (pull (seed {'% 1} (-hfql {identity identity}))))
  := [{'identity 1} 1]

  (-retraverse (pull (seed {'% 1} (-hfql {identity inc}))))
  := [{'identity 2} 2]

  (-retraverse (pull (seed {'% 1} (-hfql {inc inc}))))
  :=
  [{'inc 3}
   3]

  (-retraverse (pull (seed {'%  1} (-hfql {inc {inc {inc inc}}}))))
  := '[{inc {inc {inc 5}}}
       {inc {inc 5}}
       {inc 5}
       5]

  (-values (seed {'% {:a 1, :b 2}} (-hfql {identity [:a]})))
  := '({:a 1, :b 2} {:a 1} 1)

  (-retraverse (pull (seed {'% {:a 1, :b 2}} (-hfql {identity [:a]}))))
  := '({identity {:a 1}} {:a 1} 1)
  )

(tests
  (-values (-hfql {(range 3) {* inc}}))
  := '((0 1 2) 1 2 3)

  (-retraverse (pull (-hfql {(range 3) {* inc}})))
  := '({(range 3) (1 2 3)} 1 2 3)
 )

(tests
  "recursive nav – unbounded"
  (take 12 (-values (seed '{% 1} (-hfql {inc [str {identity ...}]}))))
  := '(2 {str "2", identity 2} "2" 2 3 {str "3", identity 3} "3" 3 4 {str "4", identity 4} "4" 4)
  )

(tests
  "recursive nav – bounded"
  (-values (seed '{% 1} (-hfql {inc [str {identity 0}]})))
  := '(2 {str "2", identity 2} "2" 2)

  (-values (seed '{% 1} (-hfql {inc [str {identity 1}]})))
  := '(2 {str "2", identity 2} "2" 2 3 {str "3", identity 3} "3" 3)

  (-values (seed '{% 1} (-hfql {inc [str {identity 2}]})))
  := '(2 {str "2", identity 2} "2" 2 3 {str "3", identity 3} "3" 3 4 {str "4", identity 4} "4" 4)
  )

(tests
  "recursive nav through pull – unbounded"
  (take 12 (-values (seed '{% 1} (-hfql {inc [str ...]}))))
  := '(2 {str "2"} "2" 3 {str "3"} "3" 4 {str "4"} "4" 5 {str "5"} "5")
  )

(tests
  "recursive pull" ; datomic pull does detect cycles and return only :db/id at leaves.
  (force-thunk (extract (pull (seed '{% 1} (-hfql {inc [str {identity 1}]}))))) 
  := '{inc {str "2", identity {inc {str "3", identity 3}}}})


#?(:clj
   (tests
     "Unbounded pull will overflow"
     (force-thunk (extract (pull (seed {'% nil} (-hfql {identity ...})))))
     :throws java.lang.StackOverflowError
     ))

(tests
  "recursive pull across star traversal"
  (force-thunk (extract (pull (seed {'% (range 3)} (-hfql {identity {* 1}})))))
  := '{identity ({identity 0} {identity 1} {identity 2})}
  )

(tests
  "pull re-navigation on recursive pull"
  (-retraverse (pull (seed '{% 1} (-hfql {inc [str {identity 1}]})))) 
  := '({inc {str "2", identity {inc {str "3", identity 3}}}}
       {str "2", identity {inc {str "3", identity 3}}}
       "2"
       {identity {inc {str "3", identity 3}}}
       {inc {str "3", identity 3}}
       {str "3", identity 3}
       "3"
       {identity 3}))

(tests
  "bounded recursion"
  (-values (seed {'% 1} (-hfql {inc [str 2]})))
  := '(2 {str "2"} "2" 3 {str "3"} "3" 4 {str "4"} "4")

  "unbounded recursion"
  (take 9 (-values (seed {'% 1} (-hfql {inc [str ...]}))))
  := '(2 {str "2"} "2" 3 {str "3"} "3" 4 {str "4"} "4")

  "bounded pull"
  (-retraverse (pull (seed {'% 1} (-hfql {inc [str 2]}))))
  := '({inc {str "2", inc {str "3", inc {str "4"}}}}
       {str "2", inc {str "3", inc {str "4"}}}
       "2"
       {inc {str "3", inc {str "4"}}}
       {str "3", inc {str "4"}}
       "3"
       {inc {str "4"}}
       {str "4"}
       "4"))

#?(:clj
   (tests
     "unbounded pull" ; crashes on first value asked – because pull is a post-order traversal.
     (pull (seed {'% 1} (-hfql {inc [str ...]})))
     := _ ; no crash

     (take 0 (-retraverse (pull (seed {'% 1} (-hfql {inc [str ...]})))))
     := _ ; same query, no crash

     (first (-retraverse (pull (seed {'% 1} (-hfql {inc [str ...]})))))
     :throws java.lang.StackOverflowError ; same query, asking for the first crashes (first is the root, accumulated in post-order)

     nil))

(tests
  "Reseeding – seeding twice"
  (-values (seed {'% {:a 10}} (seed {'% {:a 1}} (-hfql [:a])))) ; (seed … (seed … query)
  := '({:a 10} 10))

(tests
  "Reseeding – seeding children while traversing"
  (let [w (seed {'% 1} (-hfql {inc {inc {inc inc}}}) )]
    (unwrap-either (force-thunk (extract w))) := 2
    (let [w2 (first (unwrap w))]
      [(-values w2)
       (-values (seed {'% 10} w2))])

    := '[(3 4 5) (11 12 13)]
    ))

(tests
  "dynamic scope injection"
  (def ^:dynamic *_x* 1)
  (def _plus-x (fn [y] (+ *_x* y)))
  (let [% 2]
    (-values (-hfql (_plus-x %))))
  := '(3)

  (binding [*_x* 8]
    (let [% 2]
      (-values
        (seed {`hfql/*bindings* {#'*_x* *_x*}
               '% 2}
          (-hfql (_plus-x %))))))
  := '(10))

;; ── :limit option runtime tests ──────────────────────────────────────────────

(tests
  ":limit — truncates collection"
  (let [T (seed {'% (range 100)} (-hfql [[{* identity} ::hfql/limit 3]]))]
    (count (unwrap (first (unwrap T)))) := 3))

(tests
  ":limit — limit is available on traversal HFQLT metadata"
  (let [T (seed {'% (range 100)} (-hfql [[{* identity} ::hfql/limit 3]]))
        traversalT (first (unwrap T))]
    (::hfql/limit (extract (meta traversalT))) := 3))

(tests
  ":limit — truncation derivable: bounded-count > limit when truncated"
  (let [T (seed {'% (range 100)} (-hfql [[{* identity} ::hfql/limit 3]]))
        traversalT (first (unwrap T))
        limit (::hfql/limit (extract (meta traversalT)))
        children (unwrap traversalT)]
    (> (bounded-count (inc limit) children) limit) := false ; limit-collection already truncated, so bounded-count = limit
    (count children) := 3))

(tests
  ":limit — exact count: not truncated"
  (let [T (seed {'% (range 3)} (-hfql [[{* identity} ::hfql/limit 3]]))
        traversalT (first (unwrap T))
        limit (::hfql/limit (extract (meta traversalT)))
        children (unwrap traversalT)]
    (count children) := 3))

;; ── :xform option runtime tests ──────────────────────────────────────────────

(tests
  ":xform — transforms leaf value"
  (-values (seed {'% {:a 1}} (-hfql [[:a ::hfql/xform inc]])))
  := '({:a 2} 2))

(tests
  ":xform — exception in xform becomes Left"
  (let [T (seed {'% {:a "not-a-number"}} (-hfql [[:a ::hfql/xform inc]]))]
    (either/left? (force-thunk (extract (first (unwrap T))))))
  := true)

;; ── :default option runtime tests ────────────────────────────────────────────

(tests
  ":default — substitutes when key absent"
  (-values (seed {'% {:a 1}} (-hfql [[:b ::hfql/default 42]])))
  := '({:b 42} 42))

(tests
  ":default — present nil is NOT substituted"
  (-values (seed {'% {:b nil}} (-hfql [[:b ::hfql/default 42]])))
  := '({:b nil} nil))

(tests
  ":default — present value is NOT substituted"
  (-values (seed {'% {:b 7}} (-hfql [[:b ::hfql/default 42]])))
  := '({:b 7} 7))
(ns hyperfiddle.hfql2.interpreter
  "
  Defines the HFQLT type, a Cofree Transformer (aka. CofreeT) over the HFQL AST.
  Defines how to interpret AST nodes with corresponding effects. Implement all of these effects.
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
   [hyperfiddle.ct.function :refer [flip]]
   [hyperfiddle.ct.functor :refer [Functor fmap]]
   [hyperfiddle.ct.bifunctor :as bf]
   [hyperfiddle.ct.pair :refer [pair]]
   [hyperfiddle.ct.monoid :refer [mappend]]
   [hyperfiddle.ct.comonad :refer [Comonad extract duplicate extend -extract]]
   [hyperfiddle.ct.cofree :refer [ComonadCofree synthesize unwrap ICofreeT runCofreeT ->CofreeF ->CofreeT headF tailF tree-seqT]]
   [hyperfiddle.ct.rose-tree :refer [mapHead mapTail]]
   [hyperfiddle.ct.store :refer [store seek pos #?(:cljs Store)]]
   [hyperfiddle.ct.monad-reader :refer [reader local runReader memoize-1]]
   [hyperfiddle.ct.monad :refer [bind =<<]]
   [hyperfiddle.ct.either :as either :refer [either? left?]]
   [hyperfiddle.hfql2 :as-alias hfql]
   [hyperfiddle.hfql2.protocols :as hfqlp]
   [hyperfiddle.rcf :refer [tests tap %]]
   [hyperfiddle.hfql2.ast :as ast]
   [hyperfiddle.hfql2.analyzer :as ana]
   [hyperfiddle.hfql2.utils :refer [has-meta? original-object]]
   [hyperfiddle.ct.experiments.thunk :as thunks :refer [thunk thunk? force-thunk]])
  #?(:clj (:import [hyperfiddle.ct.store Store]))
  #?(:cljs (:require-macros [hyperfiddle.hfql2.interpreter :refer [-hfql]])))

(defn safe-ccp-nav [object key value]
  (when (some? object) ; nil check – nav is not defined on nil, on purpose
    (hfqlp/nav object key value)))

(def ^:dynamic *navigate* safe-ccp-nav) ; default is to navigate objects, except in UIs where, at leaves, where we want to see the symbolic values

(defn- meta [object]
  (when (has-meta? object)
    (clojure.core/meta object)))

(def star? ast/star?)
(def star-traversal? ast/star-traversal?)
(def multiplicity-many? ast/multiplicity-many?)
(def projection-type ast/projection-type)

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

(defn thunked "thunked :: (a -> b) -> a -> Thunk b"
  ([f] (#?(:clj bound-fn :cljs fn) thunked-f [a] (thunked f a)))
  ([f a] (let [f (#?(:clj clojure.core/bound-fn*, :cljs identity) f)]
           ;; Note this is almost monadic bind for Thunks, except it accepts both thunks and non-thunks as initial value.
           (thunk (force-thunk (f (force-thunk a)))))))

(tests

  ;; deref ∘ thunked f = f
  (thunked inc 1)
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

(def focus-% (local (thunked (comp ensure-either (resolve '%)))))

(defn resolve-arguments
  ([point] (partial resolve-arguments point))
  ([point env]
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
                  (::ana/parameters point) (concat (::ana/arguments point) (repeat ::unbound)))]
       (either/sequenceEither args))
     (catch #?(:clj Throwable, :cljs :default) err
       (either/left err)))))

(def focus-arguments (comp local thunked resolve-arguments))

(defn- from-either [e] (either/either identity identity e))

(defn template-symbolic-edge [env point & [point-free?]]
  (let [edge (::ana/edge point)
        {::ana/keys [form parameters]} edge]
    (case (::ana/type edge)
      ::ana/call (cons (first form)
                   (map (fn [parameter argument]
                          (if (or (:symbolic (meta parameter)) (:symbolic (meta argument)))
                            parameter
                            argument))
                     parameters (from-either (resolve-arguments edge env))))
      ::ana/symbol (cond
                       point-free? form
                       :else (list form (from-either (force-thunk (resolve '% env '%))))) ; introduce a point
      form)))

(declare hfql?)

(defn symbolic-edge-static [point]
  (::ana/symbolic-edge
   (case (::ana/type point)
     ::ana/traversal (::ana/edge point)
     point)))

(defn symbolic-edge ; FIXME uniformize
  [point & {::keys [point-free] :or {point-free true}}]
  (let [[point scope] (if (hfql? point)
                        [(meta point) (pos (runCofreeT point))]
                        [point (::scope point)])]
    (case (projection-type point)
      ::ana/traversal (template-symbolic-edge scope point point-free)
      ;; ::ana/call (template-symbolic-edge (::scope point) point point-free) ; TODO
      (symbolic-edge-static point))))

(def mem-reader "Like `monad-reader/reader` but the read function only reruns if the environment has changed." (comp reader memoize-1))

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

(defn- getter [point]
  (let [key (ast/unquote* (get point ::ana/form))]
    (mem-reader ;; reader is algebraically equivalent to a plain function (fn [env] ...). But we keep an
      ;; explicit reader for clarity. Also mem-reader is optimized
      (binding [thunks/*cache-strategy* ::thunks/weak]
        (thunked
          (=<<
            (fn [%]
              (try
                #?(:clj (log/debug "[IO]" `getter key))
                (either/right
                  (fmap-original-object (fn [%] (*navigate* % key (get % key))) %))
                (catch #?(:clj Throwable, :cljs :default) err
                  (either/left err))))))))))

(defn- get-or-call [point]
  (mem-reader ; explicit reader for clarity
    (binding [thunks/*cache-strategy* ::thunks/weak]
      (thunked
        (=<<
          (fn [%]
            (try
              (either/right
                (fmap-original-object (fn [%]
                                        (let [ifn (::ana/value point)]
                                          (if (try (contains? % ifn) ; FIXME find a better check
                                                   (catch #?(:clj Throwable, :cljs :default) _
                                                     false))
                                            (do #?(:clj (log/debug "[IO]" 'get-or-call ">" '*navigate* (::ana/form point)))
                                                (*navigate* % ifn (get % ifn)))
                                            (do #?(:clj (log/debug "[IO]" 'get-or-call ">" 'apply (::ana/form point)))
                                                (apply ifn [%])))))
                  %))
              (catch #?(:clj Throwable, :cljs :default) err
                (either/left err)))))))))

(defn- call [point]
  (let [call! (if (= ::ana/keyword-fn (::ana/kind point))
                (let [key (::ana/form-resolved point)]
                  #(*navigate* %1 key (get %1 key)))
                (let [f (::ana/value point)] ; Meaning :db/id and #(:db/id %) will navigate, but #(get % :db/id) won't.
                  #(apply f %&)))]
    (mem-reader
      (binding [thunks/*cache-strategy* ::thunks/weak]
        (thunked
          (=<<
            (fn [args]
              (try
                #?(:clj (log/debug "[IO]" `call (::ana/symbolic-edge point)))
                (either/right
                  (fmap-original-objects (fn [args] (apply call! args)) args))
                (catch #?(:clj Throwable, :cljs :default) err
                  (either/left err))))))))))

(defn- touch [object]
  (try
    (either/right
      (if (seqable? object)
        (doall object)
        object))
    (catch #?(:clj Throwable, :cljs :default) err
      (either/left err))))

(def ^:private star (mem-reader (thunked (=<< touch))))

(defn- left "Variadic opposite of `{}` aka discard." [a & _] a) ; almost the K combinator
(defn- right "Variadic `{}` aka discard." [& args] (last args)) ; almost (K I)

(defn- bound-fn*
  ([f] #?(:clj (clojure.core/bound-fn* f)
          :cljs (throw (ex-info "Unsupported platform" {}))))
  ([bindings f]
   #?(:clj
      (with-bindings* bindings clojure.core/bound-fn* f)
      :cljs f ; TODO
      )))

(defn leaf
  ([point] (partial leaf point))
  ([point Ra]
   (->> (if (= false (::leaf point)) ; FIXME wart – should always navigate unless the renderer disables it.
          Ra
          (reader (fn [env] ((bound-fn* {} (runReader Ra)) env))))
     (fmap (fn [v] (pair v ()))))))

(declare seed)

(defn- disambiguate-nums-from-recursion [form] (ast/quote-nat-ints form))
(defn- undisambiguate-nums-from-recursion [form] (if (and (ast/quoted? form) (nat-int? (second form)))
                                                   (ast/unquote* form)
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
      (into {} (map pair (fmap (comp undisambiguate-nums-from-recursion symbolic-edge meta) kidsT)
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
  (or (= ::ana/recur (projection-type (extract hfql)))
    (::ana/recursion-point (extract hfql))
    (when-let [child (first (unwrap hfql))]
      (and (star-traversal? (extract child))
        (recursion-point? child)))))

(defn- direct-recursion-point? [hfql]
  (and (recursion-point? hfql)
    (or (= ::ana/recur (projection-type (extract hfql)))
      (star-traversal? (extract hfql)))))

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
          (->> associative
            (map key) ; do not use `keys` here. `doall` doesn't work as expected on it. See following RCF.
            (map disambiguate-nums-from-recursion)
            (doall))
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
  `{#'var value, ...}`."
  [Ra]
  (mem-reader ; no need to memoize/delay here, all possible IO is in `Ra`, let Ra deal with it.
    (fn [env]
      ((bound-fn* (get env `hfql/*bindings* {}) (runReader Ra)) env))))

(defn- insert-at [index collA collB] (concat (take index collA) collB (drop index collA)))

(defn- find-recursion-point [hfql]
  (->> (iterate #(some->> % extract ::parent) hfql)
    (take-while some?)
    (take-while (fn [%] (contains? (::env (extract %)) '%)))
    (last)
    (mapHead #(assoc % ::ana/recursion-root true))
    ))

(defn- recurse-or-stop
  "Given an hfql node, its current env, and a child node that may carry a
  recursion limit, find the recursion root and return it with a decremented
  limit. Returns nil if the limit is exhausted."
  [hfql env limit-source]
  (let [recursion-root (find-recursion-point (mapHead #(assoc % ::env env) hfql))
        limit (or (::ana/recursion-limit (extract limit-source))
                  (::ana/recursion-limit (extract recursion-root))
                  0)]
    (when (pos? limit)
      (fmap #(assoc % ::ana/recursion-limit (dec limit)) recursion-root))))

(defn hfqlT
  "A coalgebra transforming one HFQL AST node (a Cofree point) into an effect point.
  This effect point is of type (Reader Env (Pair value children)), where:
  - Env is the seeded environment.
  - value is the value computed from Env when Reader is run.
  - children are the next AST nodes. There may be none.

  The `unfoldT` function is in charge of:
  - running the Reader,
  - recursively apply hfqlT onto AST child nodes,
  - wrapping them into an HFQLT type.
  "
  [hfql]
  (let [point (extract hfql)
        children (unwrap hfql)
        transform
        (case (projection-type point)
          ::ana/symbol  (comp (leaf point) focus-% getter)
          ::ana/keyword (comp (leaf point) focus-% getter)
          ::ana/string  (comp (leaf point) focus-% getter)
          ::ana/literal (comp (leaf point) focus-% getter)
          ::ana/fn      (comp (leaf point) focus-% get-or-call)
          ::ana/var     (comp (leaf point) (focus-arguments point) call)
          ::ana/quoted-var (comp (leaf point) focus-% getter)
          ::ana/call    (comp (leaf point) (focus-arguments point) call)
          ::ana/lambda  (comp (leaf point) (focus-arguments point) call)
          ::ana/star    (comp (leaf point) focus-% (constantly star))
          ::ana/recur   (constantly (mem-reader (fn [env] (pair env [(find-recursion-point (mapHead #(assoc % ::env env) hfql)) ; TODO extract helper for parent-aware tree walk
                                                                     ]))))
          ::ana/pull
          (constantly
            (mem-reader
              (fn [env]
                (let [[[star?] kids] (if (empty? children)
                                       ;; This conditional makes it impossible to express a pull expression that "selects nothing" aka "pulls nothing".
                                       ;; We haven't found a use case for `[]` in UIs. It produces empty tables.
                                       ;; We don't know if there's a use case for `[]` at the REPL.
                                       ;; Therefore, we define `[]` to behave as `[*]` so to improve DX by favoring a
                                       ;; REPL-like, hot-code-reload driven, exploratory workflow. Even though `[]` vs
                                       ;; `[*]` are not defined as algebraically equivalent.
                                       ;; Monoïdal rules are:
                                       ;; - although `[] ≅ [*]`, `[] ≠ [*]`,
                                       ;; - therefore `[:a] ⊗ [] = [:a] ≇ [:a *]`,
                                       ;; - commutatively `[] ⊗ [:a] = [:a] ≇ [* :a]` – Note this would mess table column order up,
                                       ;; - and `[] ⊗ [] = [] ≅ [*]`.
                                       [[true] ()]
                                       (group-by-pred (comp star? extract) children))
                      star-position (if (empty? children)
                                      0
                                      (or (ffirst (filter (comp hyperfiddle.hfql2.interpreter/star? extract second) (map-indexed vector children)))
                                        ##Inf))
                      dynamic-keys (when star? (star-keys (ensure-either (get env '%))))
                      star-kids (let [static-keys (set (map (comp symbolic-edge extract) kids))
                                      remaining-star-keys (remove static-keys dynamic-keys)]
                                  (unwrap (ana/analyze {} (vec remaining-star-keys))))
                      recur-point (first (filter direct-recursion-point? (concat kids star-kids)))
                      kids (remove direct-recursion-point? kids)
                      star-kids (remove direct-recursion-point? star-kids)]
                  (pair (thunk
                          (->> (concat kids star-kids) ; star kids cannot overwrite static kids – they are already filtered at this point
                            (map (unfoldT hfqlT))
                            (pull-peek-at-immediate-kids! env)))
                    (->>
                      (mappend
                        (insert-at star-position kids star-kids)
                        (if recur-point
                          (if-let [root (recurse-or-stop hfql env recur-point)]
                            [(bf/first #(assoc % ::ana/recursive-child true) root)] ; only used in matrix.cljc. TODO confirm use case.
                            [])
                          []))
                      (mapv (fn [child] (mapHead #(assoc % ::parent (mapHead (fn [%] (assoc % ::env env)) hfql)) child))) ; TODO extract helper for parent-aware tree walk
                      ))))))
          ::ana/traversal
          (let [lexical-symbol? (= ::ana/symbol (projection-type (::ana/edge point)))  ; not a function
                lexical-value (::ana/value (::ana/edge point))
                edgeT (unfoldT hfqlT (cond (star-traversal? point) (ana/hfql identity)
                                           lexical-symbol? (ana/hfql identity)
                                           :else (ana/hydrate [(::ana/edge point) ()]) ; ::leaf false is set by the analyzer
                                           ))]
            (constantly
              (mem-reader
                (fn [env]
                  (let [env (force-thunk env)
                        env' (if lexical-symbol?
                              (assoc env '% lexical-value)
                              env)]
                    (pair (extract (seed env' edgeT))
                      (let [direct-child (first children)
                            direct-child (mapHead #(assoc % ::parent (mapHead (fn [%] (assoc % ::env env)) hfql)) direct-child)] ; TODO extract helper for parent-aware tree walk
                        (if (star-traversal? (extract direct-child))
                          ;; Unwrap star children first, then handle recursion here
                          ;; (not downstream) so ::recur nodes stay invisible to pull_algebra.
                          (let [unwrapped (first (unwrap direct-child))
                                unwrapped (mapHead #(assoc % ::parent (mapHead (fn [%] (assoc % ::env env)) hfql)) unwrapped)]
                            (if (= ::ana/recur (projection-type (extract unwrapped)))
                              (if-let [root (recurse-or-stop hfql env unwrapped)]
                                [root]
                                [])
                              [unwrapped]))
                          ;; Non-star child: handle recursion at traversal level if needed.
                          (if (recursion-point? hfql)
                            (if-let [root (recurse-or-stop hfql env direct-child)]
                              [root]
                              [])
                            [direct-child]))))))))))]
    (inject-dynamic-bindings (transform point))))

(declare hfqlt-equals hfqlt-hash)

(deftype HFQLT [metadata CofreeT]
  ICofreeT
  (-runCofreeT [_] (runCofreeT CofreeT))
  Functor
  (-fmap [fa f] (HFQLT. metadata (fmap f CofreeT)))
  Comonad
  (-extract [t] (extract CofreeT))
  (-duplicate [this] (extend identity this))
  (-extend [this f]
    (let [inner-store (runCofreeT CofreeT)]
      (HFQLT. metadata
        (->CofreeT
          (store
            (fn [env']
              (let [focused-store (seek env' inner-store)
                    cofreeF (extract focused-store)
                    focused-hfqlt (HFQLT. metadata (->CofreeT focused-store))
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
  #?(:clj (meta [_] metadata))
  #?(:clj clojure.lang.IObj)
  #?(:clj (withMeta [_ m] (HFQLT. m CofreeT)))

  Object
  #?(:clj (equals [this other] (hfqlt-equals this other)))
  #?(:clj (hashCode [this] (hfqlt-hash this)))
  (toString [this]
    (let [scope (runCofreeT CofreeT)
          % (get (pos scope) '%)
          seeded? (some? %)
          realized? (or (and (thunk? %) (realized? %)) seeded?)
          error? (cond
                   (and (thunk? %) (clojure.core/realized? %)) (left? (force-thunk %))
                   (either? %) (left? %)
                   :else :unknown)]
      (str "#hfql/" (name (::ana/type metadata :hfql/hfql)) " "
        {:form (::ana/form metadata)
         :seeded? seeded?
         :realized? realized?
         :error? error?})))

  #?(:cljs cljs.core/IEquiv)
  #?(:cljs (-equiv [this other] (hfqlt-equals this other)))
  #?(:cljs cljs.core/IHash)
  #?(:cljs (-hash [this] (hfqlt-hash this)))
  #?(:cljs cljs.core/IMeta)
  #?(:cljs (-meta [_] metadata))
  #?(:cljs cljs.core/IWithMeta)
  #?(:cljs (-with-meta [_ m] (HFQLT. m CofreeT)))
  )

#?(:clj
   (defmethod print-method HFQLT [v ^java.io.Writer w]
     (.write w (.toString v))))

(defn ->hfqlT [metadata fwa] (->HFQLT metadata (->CofreeT fwa)))
(defn hfql? [obj] (instance? HFQLT obj))

(defn- hfqlt-equals [^HFQLT this ^HFQLT other]
  (or (identical? this other)
    (and (instance? HFQLT this) (instance? HFQLT other)
      (= (::ana/id (.-metadata this))
        (::ana/id (.-metadata other)))
      (= (pos (runCofreeT this))
        (pos (runCofreeT other))))))

(defn- hfqlt-hash [^HFQLT hfqlt] (hash (::ana/id (.-metadata hfqlt))))

(defn hoistHfqlT
  "Apply f to metadata at every node in the HFQLT tree. Lazy — children are only transformed when accessed."
  [f t]
  (let [inner-store (runCofreeT t)
        store' (fmap (fn [cofreeF]
                       (->CofreeF (headF cofreeF)
                                  (fmap #(hoistHfqlT f %) (tailF cofreeF))))
                     inner-store)]
    (->HFQLT (f (meta t)) (->CofreeT store'))))

(defn vary-static-meta
  "Deprecated: use vary-meta instead."
  [t f & args] (apply vary-meta t f args))

(defn- lexical-env [point]
  (::ana/lexical-env point))

(def iterator? cd/iterator?)
(def enumeration? cd/enumeration?)
(def to-seq cd/to-seq)

(defn unfoldT
  "Build an HFQLT navigator from an AST (rose tree) using a coalgebra.
  The coalgebra transforms each AST node into a Reader Env (Pair value children).
  The result is an HFQLT instance wrapping a Store comonad — seed it with an
  environment to traverse."
  ([coalgebra] (partial unfoldT coalgebra))
  ([coalgebra AST]
   (let [point (extract AST)
         step (coalgebra AST)]
     (->hfqlT (assoc point ::AST AST ::static-children (map (unfoldT coalgebra) (unwrap AST)))
       (store
           (fn peek [scope]
             ;; IO issue: scope can change due to dynamic bindings (e.g. search
             ;; needle). Even if `step` is memoized, a new ->CofreeF will be
             ;; built. And under * traversal IO will be forced to produce a new
             ;; list of children. FIXME, memoize the CofreeF instance and
             ;; rebuild it only if `value` or `children` did change.
             ;; #?(:clj (log/debug "Unwrapping" (::ana/symbolic-edge (or (::ana/edge metas) metas))))
             (let [[value children] (runReader step scope)]
               (->CofreeF value
                 (let [value (binding [thunks/*cache-strategy* ::thunks/hard] ; cache branch values, discarded when no leaves are referencing it, as per lazy-seq semantics, recursively up to the root.
                               (thunk (extract value)))]
                   (if-not (multiplicity-many? point) ; if multiplicity one
                     ;; don't use fmap under here - we want to enforce lazy children.
                     (map ; for each child, unfold it and seed it with next scope.
                       (comp (seed (case (projection-type point) ; decide what's next scope.
                                     ::ana/pull scope ; a "pull" point doesn't alter '% – it doesn't run anything. Only collects kvs.
                                     (assoc scope '% value)))
                         (unfoldT coalgebra))
                       children)
                     (if (empty? children) ; if recursion is bounded and over, * won't have more children
                       ()
                       ;; Isn't there a way here to produce a lazy-seq producing unseeded children upto multiplicity?
                       ;; It seems no because to know the children count we must run IO.
                       ;; #dbg
                       (let [forced (if (contains? scope ::BYPASS-USER-IO-TO-GET-AN-UNSEEDED-COFREE-CHILD-IN-EMPTY-MULTIPLICITY-MANY-CASE) ; Used by hyperfiddle.navigator6.selection2/find-selectable-point
                                      '(nil)
                                      (do #?(:clj (log/trace "Running IO for * traversal." (::ana/symbolic-edge (or (::ana/edge point) point))))
                                          (either/either
                                            (constantly ()) ; stop navigation at first error
                                            identity
                                            (force-thunk value) ; ⚠ will force all effects up to the root to compute children multiplicity
                                            )))
                             ;; Unfold next child once, then fan-out by seeding it multiple times in parallel, once for each value. Put each value in scope as '%.
                             childT (unfoldT coalgebra (first children)) ; There can only be one child, because fan-outs happen on traversals only and a traversal only has one continuation.
                             scope (dissoc scope ::BYPASS-USER-IO-TO-GET-AN-UNSEEDED-COFREE-CHILD-IN-EMPTY-MULTIPLICITY-MANY-CASE)
                             ]
                         (with-meta ; carry metadata over
                           ;; Can't use `sequence` here. `sequence` produces a chunked iterator seq and we must not force any form of batching in presence of IO. The user decides upstream.
                           (map-indexed  ; Record item's index for eventual positional selection.
                             (fn [positional-index value_i]
                               (let [% (binding [thunks/*cache-strategy* ::thunks/weak] ; lazy-seq spine do not hold onto navigated values. Leaves does.
                                         (thunk
                                           (try
                                             (either/right (safe-ccp-nav forced nil value_i)) ; hard-wired navigate call – a fan-out is never a leaf.
                                             (catch #?(:clj Throwable, :cljs :default) err
                                               (either/left err)))))]
                                 (-> (seed (assoc scope '% %) childT) ; no need to check if current point is "pull" or not. A pull cannot be multiplicity many.
                                   (vary-meta assoc ::hfql/positional-index positional-index) ; note positional index isn't a stable identity in the general case.
                                   )))
                             (to-seq forced))
                           (meta forced)))))))))
           (lexical-env point))))))

(defn seed
  "Sets the environment of an HFQLT instance. Any point of the tree can be re-seeded. Re-seeding busts cached IO."
  ([env] (partial seed env))
  ([env t]
   (->hfqlT (assoc (meta t) ::scope env)
            (seek env (runCofreeT t)))))

(defmacro -hfql [hfql] `(unfoldT hfqlT (ana/hfql ~hfql)))

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
  (let [point (meta W)]
    (case (projection-type point)
      ::ana/pull
      (thunk
        (into {}
          (map (fn [W value]
                 (let [point (meta W)]
                   (case (projection-type point)
                     ::ana/traversal (force-thunk value)
                     (pair (symbolic-edge W ::point-free true) (original-object (force-thunk value))))))
            (unwrap W)
            children)))
      ::ana/traversal
      (thunk
        (let [key (symbolic-edge W ::point-free true)
              val (if (seq children) ; regular traversal always have children, but bounded recursion final point doesn't.
                    (if (multiplicity-many? point)
                      (map (comp original-object force-thunk) children)
                      (original-object (force-thunk (first children))))
                    ;; bounded recursion case, no more children, return value as-is.
                    (original-object (extract-right-value-or-keep-left-error (force-thunk (extract W)))))]
          (if (star-traversal? point) ; e.g. without this check {* [ns-name]} on (all-ns) would produce {nil (…)} because `*` is purely syntactic and is not a named point.
            val
            {key val})))
      (fmap extract-right-value-or-keep-left-error (extract W)))))

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
  ((synthesize pull-algebra) (duplicate W)))


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
  (from-either (force-thunk (extract (seed {'% {:a 1}} (-hfql *))))) 
  := {:a 1}

  (from-either (force-thunk (extract (seed {'% {:a 1}} (-hfql :a)))))
  := 1

  (from-either (force-thunk (extract (seed {'% {:a 1}} (-hfql identity)))))
  := {:a 1}

  (from-either (force-thunk (extract (seed {'% {identity 'identity}} (-hfql identity)))))
  := 'identity
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
    (from-either (force-thunk (extract w))) := 2
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
(ns hyperfiddle.hfql1.coalgebra
  (:refer-clojure :exclude [extend resolve meta bound-fn*])
  (:require
   [clojure.core.protocols :as ccp]
   [contrib.data :refer [group-by-pred]]
   [clojure.datafy :as datafy]
   #?(:clj [clojure.tools.logging :as log])
   [hyperfiddle.ct.function :refer [flip]]
   [hyperfiddle.ct.functor :refer [Functor fmap]]
   [hyperfiddle.ct.bifunctor :as bf :refer [Bifunctor IPair fst snd pair swap bimap &&&]]
   [hyperfiddle.ct.monoid :refer [mappend]]
   [hyperfiddle.ct.comonad :refer [Comonad extract duplicate extend -extract]]
   [hyperfiddle.ct.cofree :refer [synthesize unwrap runCofreeT ->CofreeF ->CofreeT headF tree-seqT hoistCofreeT]]
   [hyperfiddle.ct.store :refer [store seek seeks peeks pos #?(:cljs Store) memoize-1]]
   [hyperfiddle.ct.monad-reader :refer [reader local runReader]]
   [hyperfiddle.hfql2 :as-alias hfql]
   [hyperfiddle.rcf :refer [tests tap %]]
   [hyperfiddle.hfql1.analyzer :as ana]
   [hyperfiddle.hfql1.utils :refer [has-meta?]])
  #?(:clj (:import [hyperfiddle.ct.store Store]))
  #?(:cljs (:require-macros [hyperfiddle.hfql1.coalgebra :refer [-hfql]])))

(defn safe-ccp-nav [object key value]
  (if (satisfies? ccp/Navigable object)
    (datafy/nav object key value)
    value))

(def ^:dynamic *navigate* safe-ccp-nav) ; default is to navigate objects, except in UIs where, at leaves, where we want to see the symbolic values

(defn- meta [object]
  (when (has-meta? object)
    (clojure.core/meta object)))

(def metas fst)
(def star? (comp #(= ::ana/star %) ::ana/type metas))
(def star-traversal? (comp #(= ::ana/star %) ::ana/kind metas))
(def multiplicity-many? (comp #(= ::ana/many %) ::ana/multiplicity metas))

(defn projection-type [point]
  (let [metas (metas point)]
    (case (::ana/type metas)
      ::ana/symbol (if (fn? (::ana/value metas))
                     ::ana/fn
                     ::ana/symbol)
      (::ana/type metas))))

(defn- resolve
  "Lookup `symbol` into `env` and return associated value. Throw if symbol cannot be found in `env`."
  ([symbol] (partial resolve symbol))
  ([symbol env]
   (if-let [[_symbol value] (find env symbol)]
     value
     (throw (ex-info (str "Unable to resolve `" symbol "` from environment.") {:symbol symbol, :env env}))))
  ([symbol env not-found]
   (try (resolve symbol env)
        (catch #?(:clj Throwable, :cljs :default) ex
          not-found))))

(defn delayed "delayed :: (a -> b) -> a -> Delay b"
  ([f] (fn delayed-f [a] (delayed f a)))
  ([f a] (let [f (#?(:clj clojure.core/bound-fn*, :cljs identity) f)]
           (delay (force (f (force a)))))))

(tests

  ;; force ∘ delayed f = f
  (delayed inc 1)
  (delay? *1 ) := true
  (force *2) := 2

  ;; delayed ∘ delayed = delayed
  ((delayed (delayed inc)) 1)
  (delay? *1) := true
  (force *2) := 2
  )

(def focus-% (local (delayed (resolve '%))))

(defn resolve-arguments
  ([metas] (partial resolve-arguments metas))
  ([metas env]
   (map (fn [parameter argument]
          (if (symbol? parameter)
            (let [resolved (resolve parameter env)]
              (if (= '% parameter)
                (force resolved)
                resolved))
            (if (= ::unbound argument)
              (throw (ex-info "Unbound function argument" {:parameter parameter}))
              argument)))
     (::ana/parameters metas) (concat (::ana/arguments metas) (repeat ::unbound)))))

(def focus-arguments (comp local delayed resolve-arguments))

(defn template-symbolic-edge [env metas & [point-free?]]
  (let [{::ana/keys [form parameters]} metas]
    (case (::ana/type metas)
      ::ana/call (cons (first form)
                   (map (fn [parameter argument]
                          (if (or (:symbolic (meta parameter)) (:symbolic (meta argument)))
                            parameter
                            argument))
                     parameters (resolve-arguments metas env)))
      ::ana/symbol (if point-free?
                     form
                     (list form (force (resolve '% env '%))) ; introduce a point
                     )
      form)))

(defn symbolic-edge-static [point]
  (let [metas (metas point)]
    (::ana/symbolic-edge
     (case (::ana/type metas)
       ::ana/traversal (::ana/edge metas)
       metas))))

(defn symbolic-edge [point & {::keys [point-free] :or {point-free true}}]
  (case (projection-type point)
    ::ana/traversal (template-symbolic-edge (::scope (metas point)) (::ana/edge (metas point)) point-free)
    ;; ::ana/call (template-symbolic-edge (::scope (metas point)) (metas point) point-free) ; TODO
    (symbolic-edge-static point)))

(def mem-reader "Like `monad-reader/reader` but the read function only reruns if the environment has changed." (comp reader memoize-1))

(defn- getter [metas]
  (let [key (get metas ::ana/form)]
    (mem-reader ;; reader is algebraically equivalent to a plain function (fn [env] ...). But we keep an
      ;; explicit reader for clarity. Also mem-reader is optimized
      (delayed
        (fn [%]
          #?(:clj (log/debug "[IO]" `getter key))
          (*navigate* % key (get % key)))))))

(def ^:private dup-first ":: (a, b) -> (a, a)" (&&& fst fst))

(defn- get-or-call [metas]
  (mem-reader ; explicit reader for clarity
    (delayed
      (fn [%]
        ;; (prn (dissoc metas ::AST))
        ;; (prn "get-or-call" (hash %) %)
        (let [ifn (::ana/value metas)]
          (if (try (contains? % ifn) ; FIXME find a better check
                   (catch #?(:clj Throwable, :cljs :default) _
                     #_(#?(:clj log/debug, :cljs js/console.debug) "object doesn't support `contains?`" (type object))
                     false))
            (do #?(:clj (log/debug "[IO]" 'get-or-call ">" '*navigate* (::ana/form metas)))
                (*navigate* % ifn (get % ifn)))
            (do #?(:clj (log/debug "[IO]" 'get-or-call ">" 'apply (::ana/form metas)))
                (apply ifn [%]))))))))

(defn- call [metas]
  (mem-reader
    (delayed
      (fn [args]
        #?(:clj (log/debug "[IO]" `call (::ana/symbolic-edge metas)))
        (apply (::ana/value metas) args)))))

(defn- touch [object] (if (seqable? object) (doall object) object))
(def ^:private star (mem-reader (delayed touch)))

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

(defn leaf [Ra]
  (->> (reader (fn [env]
                 ((bound-fn* {#'*navigate* right} (runReader Ra)) env)))
    (fmap (fn [v] (pair v ())))))

(declare seed)

(defn- pull-peek-at-immediate-kids! [env kidsT]
  ;; Unless I'm mistaken, this is only a test convenience utility.
  ;; A proper UI wouldn't ask for a pull value at point but
  ;; would "just" render a key-value layout and iterate on
  ;; children. Static keys are already available in metas. Dynamic
  ;; keys are available on children metas. So in a UI there's no
  ;; need to build a map here, unless there's a use case to render
  ;; the raw value at point (unpulled), e.g. for an EDN <pre>,
  ;; which may be no different from of test utility?
  (let [realized-kids (->> kidsT
                        (fmap (seed env))
                        (fmap runCofreeT)
                        (fmap extract) ; store
                        (fmap headF)   ; inner layer
                        )]
    (into {} (map pair (fmap symbolic-edge realized-kids)
               (fmap (comp force snd) realized-kids)))))

(declare unfoldT)

(defn- recursion-point? [hfql]
  (or (= ::ana/recur (projection-type (extract hfql)))
    (::ana/recursion-point (metas (extract hfql)))
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
  (try (keys (force (runReader star object))) ; expects an IPersistentMap or a sequences of MapEntries
       (catch #?(:clj Throwable, :cljs :default) _ ())))

(defn- inject-dynamic-bindings
  "Transform a Reader (env -> a) such that when run, the value associated to
  `hyperfiddle.hfql2/*bindings*` in `env` is installed in dynamic scope. The `*bindings*` value is
  expected to be compatible with `clojure.core/with-bindings*`. I.e. a map of
  `{#'var value, ...}`."
  [Ra]
  (mem-reader ; no need to memoize/delay here, all possible IO is in `Ra`, let Ra deal with it.
    (fn [env]
      ((bound-fn* (get env `hfql/*bindings* {}) (runReader Ra)) env))))

(defn hfqlT
  "Docstring is outdated TODO revise
  A coalgebra turning a statically analyzed HFQL Cofree tree into lazy,
  effectful, step-by-step navigator of type (CofreeT (Store s (Meta, Scope)))).

  Where a Cofree is fully realized, a CofreeT required a monad (here Reader) to
  compute the next step. In our model (Reader a) produces the \"context at
  point\", a.k.a. '%. This lazy CofreeT tree has no initial value and no
  dimension, meaning children are unknown. Use (seed env ...) to produce a
  concrete Cofree Comonad, lazily realizing successive navs as a tree
  traversal."
  [root hfql]
  (let [hfqlT (partial hfqlT root)
        point (dup-first (extract hfql))
        children (unwrap hfql)
        transform
        (case (projection-type point)
          ::ana/symbol  (comp leaf focus-% getter)
          ::ana/keyword (comp leaf focus-% getter)
          ::ana/string  (comp leaf focus-% getter)
          ::ana/literal (comp leaf focus-% getter)
          ::ana/fn      (comp leaf focus-% get-or-call)
          ::ana/var     (comp leaf (focus-arguments (metas point)) call)
          ::ana/quoted-var (comp leaf focus-% getter)
          ::ana/call    (comp leaf (focus-arguments (metas point)) call)
          ::ana/lambda  (comp leaf (focus-arguments (metas point)) call)
          ::ana/star    (comp leaf focus-% (constantly star))
          ::ana/recur   (constantly (mem-reader (fn [env] (pair env [root]))))
          ::ana/pull
          (constantly
            (mem-reader
              (fn [env]
                (let [[[star?] kids] (group-by-pred (comp star? extract) children)
                      dynamic-keys (when star? (star-keys (get env '%)))
                      star-kids (let [static-keys (set (map (comp symbolic-edge extract) kids))
                                      remaining-star-keys (remove static-keys dynamic-keys)]
                                  (fmap (fmap swap) (unwrap (ana/analyze {} (vec remaining-star-keys)))))
                      recur-point (first (filter direct-recursion-point? (concat kids star-kids)))
                      kids (remove direct-recursion-point? kids)
                      star-kids (remove direct-recursion-point? star-kids)]
                  (pair (delay
                          (->> (concat kids star-kids) ; star kids cannot overwrite static kids – they are already filtered at this point
                            (map (unfoldT hfqlT))
                            (pull-peek-at-immediate-kids! env)))
                    (mappend kids star-kids
                      (if recur-point
                        (let [limit (or (::ana/recursion-limit (metas (extract recur-point))) (::ana/recursion-limit (metas (extract root))))] ; TODO deduplicate with traversal
                          (if (> limit 1)
                            [(-> (fmap (bf/first #(assoc % ::ana/recursion-limit (dec limit))) root)
                               (ana/vary-meta #(assoc % ::ana/recursive-child true)))]
                            []))
                        [])))))))
          ::ana/traversal
          (let [edgeT (unfoldT hfqlT (fmap swap (cond (star-traversal? point) (ana/hfql identity)
                                                      (::ana/bypass (metas point)) (ana/hfql force)
                                                      :else
                                                  (ana/hydrate {::ana/form (::ana/form (::ana/edge (metas point))) ; FIXME wart, should be handled by the analyzer
                                                                ::ana/meta (::ana/edge (metas point))}))))]
            (constantly
              (mem-reader
                (fn [env]
                  (pair (snd (extract (seed (force env) edgeT)))
                    (let [direct-child (first children)]
                      (if (recursion-point? hfql)
                        (let [limit (or (::ana/recursion-limit (metas (extract direct-child))) (::ana/recursion-limit (metas (extract root))))] ; TODO deduplicate with pull
                          (if (> limit 1)
                            [(fmap (bf/first #(assoc % ::ana/recursion-limit (dec limit) )) root)]
                            []))
                        (if (star-traversal? (extract direct-child))
                          [(first (unwrap direct-child))]
                          [direct-child])))))))))]
    (fmap (comp inject-dynamic-bindings transform) point)))

(defprotocol SetScope
  (set-scope-value [w scope]))

(declare env-w)

(declare env-w-equals)
(deftype EnvW [meta w] ; not a defrecord because we want custom equality
  Functor
  (-fmap [_fa f] (env-w meta (fmap f w)))
  Bifunctor
  (-bimap [_bf f g] (env-w (f meta) (g w)))
  IPair
  (fst [_] meta)
  (snd [_] w)
  Comonad
  (-extract [_] (-extract w))
  (-duplicate [_wa] (env-w meta (fmap #(env-w meta %) (duplicate w))))
  (-extend [wa f] (fmap f (duplicate wa)))
  SetScope
  (set-scope-value [_ scope] (env-w (assoc meta ::scope scope) (set-scope-value w scope)))

  ;; Custom equality because we want two CofreeT instances to be comparable by =.
  ;; CofreeT, Store, MapEntry, etc... compare by record fields equality.
  ;; But Store contains [function, env] and equality on functions is not defined besides reference equality.
  ;; Therefore we consider two (CofreeT (EnvW metas (Store f env))) equal if:
  ;;  - both metas are equal
  ;;  - both env are equal
  ;; because metas contains the original AST, and so the Store's function got derived from the same AST.
  ;; therefore both Store functions are equal.
  #?(:clj Object)
  #?(:clj (equals [this other] (env-w-equals this other)))
  #?(:clj (hashCode [_] (hash [meta (if (instance? hyperfiddle.ct.store.Store w) (pos w) w)])))

  #?(:cljs cljs.core/IEquiv)
  #?(:cljs (-equiv [this other] (env-w-equals this other)))
  #?(:cljs cljs.core/IHash)
  #?(:cljs (-hash [_] (hash [meta (if (instance? Store w) (pos w) w)]))))

(defn- env-w-equals [this other]
  (assert (instance? EnvW this))
  (and (instance? EnvW other)
    (= (fst this) (fst other))
    (if (instance? hyperfiddle.ct.store.Store (snd this))
      (and (instance? hyperfiddle.ct.store.Store (snd other))
        (= (pos (snd this)) (pos (snd other))))
      true)))

(defn env-w [meta w]
  (assert (satisfies? Comonad w))
  (->EnvW meta w))

(extend-type Store
  SetScope
  (set-scope-value [w scope] (seek scope w)))

(defn- wrap-with-static-metadata [metas t] (->CofreeT (->EnvW metas (runCofreeT t))))
(defn vary-static-meta [t f] (->CofreeT (bf/first f (runCofreeT t))))

(defn unfoldT
  ([coalgebra] (partial unfoldT coalgebra))
  ([coalgebra AST]
   (let [[metas step :as t] (coalgebra AST)]
     ;; (hoistCofreeT (fn [w] (->EnvW metas w)))
     (wrap-with-static-metadata (assoc metas ::AST AST ::static-children (map (unfoldT coalgebra) (unwrap AST)))
       (->CofreeT
         (store
           (fn peek [scope]
             ;; IO issue: scope can change due to dynamic bindings (e.g. search
             ;; needle). Even if `step` is memoized, a new ->CofreeF will be
             ;; built. And under * traversal IO will be forced to produce a new
             ;; list of children. FIXME, memoize the CofreeF instance and
             ;; rebuild it only if `value` or `children` did change.
             #?(:clj (log/debug "Unwrapping" (::ana/symbolic-edge (or (::ana/edge metas) metas))))
             (let [[value children] (runReader step scope)]
               (->CofreeF (pair (assoc metas ::scope scope) value) ; TODO move to EnvW, as `value` is supposed to always be a Delay comonad
                 (if-not (multiplicity-many? t) ; if multiplicity one
                   ;; don't use fmap under here - we want to enforce lazy children.
                   (map ; for each child, unfold it and seed it with next scope.
                     (comp (seed (case (projection-type t) ; decide what's next scope.
                                   ::ana/pull scope ; a "pull" point doesn't alter '% – it doesn't run anything. Only collects kvs.
                                   (assoc scope '% value)))
                       (unfoldT coalgebra))
                     children)
                   (if (empty? children) ; if recursion is bounded and over, * won't have more children
                     ()
                     (let [forced (do #?(:clj (log/trace "Running IO for * traversal." (::ana/symbolic-edge (or (::ana/edge metas) metas))))
                                      (force value))] ; ⚠ will force all effects up to the root to compute children multiplicity
                       (with-meta ; carry metadata over
                         (map ; Unfold next child once, then fan-out by seeding it multiple times in parallel, once for each value. Put each value in scope as '%.
                              ; There can only be one child, because fan-outs happen on traversals only and a traversal only has one continuation.
                           (comp (flip seed (unfoldT coalgebra (first children)))
                             (partial assoc scope '%) ; no need to check if current point is "pull" or not. A pull cannot be multiplicity many.
                             (fn [value_i] (delay (*navigate* (force value) nil value_i))))
                           forced)
                         (meta forced))))))))
           (::ana/lexical-env metas)))))))

(defn seed
  "Docstirng is outdated – TODO revise
  Turns an HFQL program into a fully lazily navigable structure by providing an
  initial environment value. An HFQL program is built by calling `(-hfql <analyzed-hfql-expr>)`.

  Once seeded, an HFQL tree can be lazily traversed (think navigated, e.g. with tree-seq) without
  forcing any userland side effect, up to multiplicity-many branching points (e.g. *)¹.

  Any point of the tree can be re-seeded using `reseed`.

  Seeding \"hydrates\" an HFQL program into a navigable tree.
  Reseed \"overwrites\" an evaluation context with an arbitrary value, effectively \"rehydrating\"
  the original HFQL (sub)program at point.
   ↳ Use case: inject function arguments from UI.

  Both `seed` and `reseed` are pure and can be considered O(1)².

  Concretely: Turns the value returned by `-hfql` (a CofreeT) virtual tree with no state (no Reader
  context) and so no known multiplicity, into a concrete Comonadic tree (aka. Cofree: an infinitely
  unfolding structure) where all \"value at points\" are lazily available and where all children
  multiplicity is lazily known. Traversing the tree (e.g. tree-seq) will lazily realize values at
  branching points and realize actual subtrees. Computing a value does not require any knowledge
  about children³.

  Note: today, computing children always realizes the value at point. TODO optimize by leveraging
  static knowledge of the original HFQL expression, which is already available as metadata. There's
  no difficulty here, it's just TBD. E.g.:
  - {* ...} means multiplicity-many traversal and the fan-out multiplicity is statically unknown.
  - [*] means the keyset on which to eventually pull further is statically unknown.
  - all other forms should be statically known to be multiplicity-one, under their respective multiplicity-many context.

  ¹: {* ...} and [* ...] means multiplicity is statically unknown, and we must realize the value at
  point to know what to do next. Note that if `*` corresponds to a lazy structure, children will
  unfold lazily too. Traversing children will consume the `*` value lazily.

  ²: `seed` and `reseed` may eagerly precompute a subset of the tree structure for performance
  optimization reasons. But always without running any userland side effect.

  ³: except for \"pull\" ops – they peek at their direct children (max one level) to build a {key
  value} structure.
  "
  ([env] (partial seed env))
  ([env t]
   (->CofreeT (set-scope-value (runCofreeT t) env))))

(defmacro -hfql [hfql] `(let [ast# (fmap swap (ana/hfql ~hfql))]
                          (unfoldT (partial hfqlT ast#) ast#)))

(defn- -values [T] (map force (fmap snd (map extract (tree-seqT T)))))


(tests

  (def _inc (fn [x] (prn 'IO! '_inc x) (inc x)))

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
  (-values (seed {'% {:a 1, :b "b"}} (-hfql [* {:a inc}])))
  := '({:a 1, :b "b"} 1 2 "b")
  )

(defn- pull-algebra [W children]
  (let [point (extract W)]
    (case (projection-type point)
      ::ana/pull (fmap (fn [_]
                         (delay
                           (into {}
                             (map (fn [point value]
                                    (case (projection-type point)
                                      ::ana/traversal (force value)
                                      (pair (symbolic-edge point ::point-free true) (force value))))
                               (map extract (unwrap W))
                               (map snd children)))))
                   point)
      ::ana/traversal
      (fmap (fn [_]
              (delay {(symbolic-edge point ::point-free true)
                     (if (seq children) ; regular traversal always have children, but bounded recursion final point doesn't.
                       (if (multiplicity-many? point)
                         (map (comp force snd) children)
                         (force (snd (first children))))
                       ;; bounded recursion case, no more children, return value as-is.
                       (force (snd point)))}))
        point)
      point)))

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
  ((synthesize pull-algebra) #_W (duplicate W)))

#_(tests
  (let [t (seed {'% 1} (-hfql {inc {inc inc}}))
        t1 (first (unwrap t))
        p (pull t1)]
    (clojure.data/diff
      (fst (runCofreeT t1))
      #_(fst (runCofreeT p))
      (fst (extract t1))
      #_(fst (extract p)))))

(tests
  (force (snd (extract (pull (seed {'% 1} (-hfql identity))))))
  := 1

  (force (snd (extract (pull (seed {'% {:a 1}} (-hfql :a))))))
  := 1

  (force (snd (extract (pull (seed {'% {:a 1, :b 2}} (-hfql [:a]))))))
  := {:a 1}

  (force (snd (extract (pull (seed {'% {:a 1}} (-hfql [:a :b]))))))
  := {:a 1, :b nil}

  (force (snd (extract (pull (seed {'% {:a 1, :b 2}} (-hfql [:a :b]))))))
  := {:a 1, :b 2}

  (force (snd (extract (pull (seed {'% {:a 1}} (-hfql [:a identity]))))))
  := {:a 1, 'identity {:a 1}}

  (force (snd (extract (pull (seed {'% {:a 1}} (-hfql [{:a inc}]))))))
  := {:a 2}

  (force (snd (extract (pull (seed {'% {:a 1}} (-hfql [{:a {inc inc}}]))))))
  := {:a {'inc 3}}

  (force (snd (extract (pull (seed {'% {:a 1}} (-hfql {identity :a}))))))
  := {'identity 1}

  (force (snd (extract (pull (seed {'% {:a 1}} (-hfql {identity [:a]}))))))
  := {'identity {:a 1}}

  (force (snd (extract (pull (-hfql (range 3))))))
  := '(0 1 2)

  (-values (-hfql {(range 2) identity}))
  := '((0 1) (0 1))

  (-values (-hfql {(range 2) {* identity}}))
  := '((0 1) 0 1)

  (force (snd (extract (pull (-hfql {(range 2) {* inc}})))))
  := '{(range 2) (1 2)}

  (force (snd (extract (pull (-hfql {(range 2) {* {inc inc}}})))))
  := '{(range 2) ({inc 2} {inc 3})})

#?(:clj
   (tests
     (force (snd (extract (pull (-hfql {(ns-publics *ns*) {* val}})))))
     := {(list 'ns-publics *ns*) (vals (ns-publics *ns*))}
     ))

#?(:clj
   (tests
     (force (snd (extract (pull (-hfql {(ns-publics *ns*) {* {val symbol}}})))))
     := {(list 'ns-publics *ns*) (map #(hash-map 'val %) (map symbol (vals (ns-publics *ns*))))}
     ))

#?(:clj
   (tests
     (force (snd (extract (pull (-hfql {(ns-publics *ns*) {vals {* symbol}}})))))
     := {(list 'ns-publics *ns*) {'vals (map symbol (vals (ns-publics *ns*)))}}
     ))

#?(:clj
   (tests
     "Arguements tagged ^:symbolic do not template in resulting pull keys."
     (force (snd (extract (pull (-hfql {(ns-publics ^:symbolic *ns*) {* val}})))))
     := {'(ns-publics *ns*) (vals (ns-publics *ns*))}
     ))


#?(:clj
   (tests
     "Multiplicity many traversals stays lazy, even through pull."
     (->> (-hfql {(ns-publics *ns*) {* (fn [%] (tap "IO!") (val %))}})
       pull ; CofreeT
       extract ; [metas, pulled value]
       snd ; pulled value {ns-publics (...)}
       force
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
  (force (snd (extract (seed {'% {:a 1}} (-hfql *)))))
  := {:a 1}

  (force (snd (extract (seed {'% {:a 1}} (-hfql :a)))))
  := 1

  (force (snd (extract (seed {'% {:a 1}} (-hfql identity)))))
  := {:a 1}

  (force (snd (extract (seed {'% {identity 'identity}} (-hfql identity)))))
  := 'identity
  )

(tests
  (-values (seed {'% {:a 1, :b 2, :c 3}} (-hfql [:a :b])))
  := '({:a 1, :b 2} 1 2)
  )


(tests
  (-values (pull (seed {'% {:a 1, :b 2, :c 3}} (-hfql :a))))
  := '(1)

  (-values (pull (seed {'% {:a 1, :b 2, :c 3}} (-hfql [:a :b]))))
  := '({:a 1, :b 2} 1 2)
  )

(tests
  (-values (pull (seed {'% 1} (-hfql {identity identity}))))
  := [{'identity 1} 1]

  (-values (pull (seed {'% 1} (-hfql {identity inc}))))
  := [{'identity 2} 2]

  (-values (pull (seed {'% 1} (-hfql {inc inc}))))
  :=
  [{'inc 3}
   3]

  (-values (pull (seed {'%  1} (-hfql {inc {inc {inc inc}}}))))
  := '[{inc {inc {inc 5}}}
       {inc {inc 5}}
       {inc 5}
       5]

  (-values (seed {'% {:a 1, :b 2}} (-hfql {identity [:a]})))
  := '({:a 1, :b 2} {:a 1} 1)

  (-values (pull (seed {'% {:a 1, :b 2}} (-hfql {identity [:a]}))))
  := '({identity {:a 1}} {:a 1} 1)
  )

(tests
  (-values (-hfql {(range 3) {* inc}}))
  := '((0 1 2) 1 2 3)

  (-values (pull (-hfql {(range 3) {* inc}})))
  := '({(range 3) (1 2 3)} 1 2 3)
 )

(tests
  "recursive nav – unbounded"
  (take 12 (-values (seed '{% 1} (-hfql {inc [str {identity ...}]}))))
  := '(2 {str "2", identity 2} "2" 2 3 {str "3", identity 3} "3" 3 4 {str "4", identity 4} "4" 4)
  )

(tests
  "recursive nav – bounded"
  (-values (seed '{% 1} (-hfql {inc [str {identity 3}]})))
  := '(2 {str "2", identity 2} "2" 2 3 {str "3", identity 3} "3" 3 4 {str "4", identity 4} "4" 4)
  )

(tests
  "recursive nav through pull – unbounded"
  (take 12 (-values (seed '{% 1} (-hfql {inc [str ...]}))))
  := '(2 {str "2"} "2" 3 {str "3"} "3" 4 {str "4"} "4" 5 {str "5"} "5")
  )

(tests
  "recursive pull" ; datomic pull does detect cycles and return only :db/id at leaves.
  (force (snd (extract (pull (seed '{% 1} (-hfql {inc [str {identity 2}]}))))))
  := '{inc {str "2", identity {inc {str "3", identity 3}}}})

#?(:clj
   (tests
     "Unbounded pull will overflow"
     (force (snd (extract (pull (seed {'% nil} (-hfql {identity ...}))))))
     :throws java.lang.StackOverflowError
     ))

(tests
  "recursive pull across star traversal"
  (force (snd (extract (pull (seed {'% (range 3)} (-hfql {identity {* 2}}))))))
  := '{identity ({identity 0} {identity 1} {identity 2})}
  )

(tests
  "pull re-navigation on recursive pull"
  (-values (pull (seed '{% 1} (-hfql {inc [str {identity 2}]}))))
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
  (-values (seed {'% 1} (-hfql {inc [str 3]})))
  := '(2 {str "2"} "2" 3 {str "3"} "3" 4 {str "4"} "4")

  "unbounded recursion"
  (take 9 (-values (seed {'% 1} (-hfql {inc [str ...]}))))
  := '(2 {str "2"} "2" 3 {str "3"} "3" 4 {str "4"} "4")

  "bounded pull"
  (-values (pull (seed {'% 1} (-hfql {inc [str 3]}))))
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
     (first (-values (pull (seed {'% 1} (-hfql {inc [str ...]})))))
     :throws java.lang.StackOverflowError))

(tests
  "reseeding"
  (let [w (seed {'% 1} (-hfql {inc {inc {inc inc}}}) )]
    (force (snd (extract w))) := 2
    (let [w2 (first (unwrap w))]
      [(-values w2)
       (-values (seed {'% 10} w2))])

    := '[(3 4 5) (11 12 13)]
    ))

(tests
  (-values (seed {'% {:a 10}} (seed {'% {:a 1}} (-hfql [:a]))))
  := '({:a 10} 10))

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
(ns hyperfiddle.hfql1.coalgebra
  (:refer-clojure :exclude [extend resolve])
  (:require
   [clojure.core.protocols :as ccp]
   [contrib.data :refer [group-by-pred]]
   [clojure.datafy :as datafy]
   [hyperfiddle.hfql1.functor :refer [fmap]]
   [hyperfiddle.hfql1.bifunctor :as bf :refer [fst snd pair swap bimap]]
   [hyperfiddle.hfql1.monoid :refer [mappend]]
   [hyperfiddle.hfql1.comonad :refer [extract duplicate]]
   [hyperfiddle.hfql1.cofree :refer [synthesize unwrap runCofreeT ->CofreeF ->CofreeT headF tree-seqT memoize-1]]
   [hyperfiddle.hfql1.store :refer [store seek seeks peeks]]
   [hyperfiddle.hfql1.monad-reader :refer [reader local runReader]]
   [hyperfiddle.rcf :refer [tests tap %]]
   [hyperfiddle.hfql1.analyzer :as ana])
  #?(:cljs (:require-macros [hyperfiddle.hfql1.coalgebra :refer [-hfql]])))

(defn safe-ccp-nav [object key value]
  (if (satisfies? ccp/Navigable object)
    (datafy/nav object key value)
    value))

(def ^:dynamic *navigate* safe-ccp-nav) ; default is to navigate objects, except in UIs where, at leaves, where we want to see the symbolic values

(def metas fst)
(def star? (comp #(= ::ana/star %) ::ana/type metas))
(def star-traversal? (comp #(= ::ana/star %) ::ana/kind metas))

(defn projection-type [point]
  (let [metas (metas point)]
    (case (::ana/type metas)
      ::ana/symbol (if (fn? (::ana/value metas))
                     ::ana/fn
                     ::ana/symbol)
      (::ana/type metas))))

(defn symbolic-edge [point]
  (case (projection-type point)
    ::ana/traversal (::ana/symbolic-edge (::ana/edge (metas point)))
    (::ana/symbolic-edge (metas point))))

(defn- resolve
  "Lookup `symbol` into `env` and return associated value. Throw if symbol cannot be found in `env`."
  ([symbol] (partial resolve symbol))
  ([symbol env]
   (if-let [[_symbol value] (find env symbol)]
     value
     (if (symbol? symbol)
       (throw (ex-info (str "Unable to resolve `" symbol "` from environment.") {:symbol symbol, :env env}))
       symbol))))

(def focus-% (local (resolve '%)))

(defn focus-arguments* [parameters]
  (local (fn [env] (map #(resolve % env) parameters))))

(defn focus-arguments [point] (focus-arguments* (::ana/parameters (metas point))))

(defn- getter [key]
  (focus-%
    (reader ;; reader is algebraically equivalent to a plain function (fn [env] ...). But we keep an
      ;; explicit reader for clarity.
      (memoize-1
        (fn [%] (*navigate* % key (get % key)))))))

(defn- get-or-call [[metas _form]]
  (pair metas
    (focus-%
      (reader ; explicit reader for clarity - equivalent to plain (fn [env] ...)
        (memoize-1
          (fn [%]
            (let [ifn (::ana/value metas)]
              (if (try (contains? % ifn) ; FIXME find a better check
                       (catch #?(:clj Throwable, :cljs :default) _
                         #_(#?(:clj log/debug, :cljs js/console.debug) "object doesn't support `contains?`" (type object))
                         false))
                (runReader (getter ifn) {'% %})
                (apply ifn [%])
                ))))))))

(defn- call [[metas _form]]
  (pair metas
    (reader
      (memoize-1
        (fn [args]
          (apply (::ana/value metas) args))))))

(defn- touch [object] (if (seqable? object) (doall object) object))
(defn- star [_] (focus-% (reader (memoize-1 (fn [env] (touch env))))))

(defn leaf [f] (fmap (fn [v] (pair v ())) f))

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
                        (fmap headF) ; inner layer
                        )]
    (into {} (map pair (fmap symbolic-edge realized-kids)
               (fmap snd realized-kids)))))

(declare unfoldT)

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
  [hfql]
  (let [content  (extract hfql)
        children (unwrap hfql)]
    (case (projection-type content)
      ::ana/star    (fmap leaf (fmap star content))
      ::ana/symbol  (fmap leaf (fmap getter content))
      ::ana/keyword (fmap leaf (fmap getter content))
      ::ana/string  (fmap leaf (fmap getter content))
      ::ana/var     (fmap leaf (fmap getter content))
      ::ana/fn      (fmap leaf (get-or-call content))
      ::ana/call    (fmap leaf (fmap (focus-arguments content) (call content)))
      ::ana/lambda  (fmap leaf (fmap (focus-arguments content) (call content)))
      ::ana/pull
      (fmap (fn [_]
              (reader
                (memoize-1
                  (fn [env]
                    (let [[[star?] kids] (group-by-pred (comp star? extract) children)
                          dynamic-keys   (when star? (keys (runReader (star nil) env)))
                          star-kids     (let [static-keys         (set (map (comp symbolic-edge extract) kids))
                                              remaining-star-keys (remove static-keys dynamic-keys)]
                                          (fmap (fmap swap) (unwrap (ana/analyze {} (vec remaining-star-keys)))))]
                      (pair (delay (mappend
                                     (pull-peek-at-immediate-kids! env (map (unfoldT hfqlT) kids))
                                     (pull-peek-at-immediate-kids! env (map (unfoldT hfqlT) star-kids))))
                        (mappend kids star-kids)))))))
        content)
      ::ana/traversal
      (let [edgeT (unfoldT hfqlT (fmap swap (ana/hydrate {::ana/form (::ana/form (::ana/edge (metas content))) ; FIXME wart, should be handled by the analyzer
                                                          ::ana/meta (::ana/edge (metas content))})))]
        (fmap (fn [_]
                (reader
                  (memoize-1
                    (fn [env]
                      (let [[_metas new-context] (extract (seed env edgeT))
                            child (let [direct-child (first children)] ; we know traversals have only one child.
                                    (if (star-traversal? (extract direct-child))
                                      (first (unwrap direct-child))
                                      direct-child))]
                        (pair new-context
                          [child]))))))
          content)))))

(defn- unfoldT
  ([coalgebra] (partial unfoldT coalgebra))
  ([coalgebra AST]
   (let [[metas step :as t] (coalgebra AST)]
     (->CofreeT
       (store
         (fn peek [scope]
           (let [[value children] (runReader step scope)
                 next-scope (case (projection-type t)
                              ::ana/pull scope
                              (assoc scope '% value))]
             (->CofreeF (pair (assoc metas ::scope scope, ::AST AST)
                          value)
               (if (= ::ana/many (::ana/multiplicity metas))
                 (map (fn [value]
                        (hyperfiddle.hfql1.coalgebra/seed (assoc scope '% value) (unfoldT coalgebra (first children))))
                   value)
                 (map (fn [child-seed]
                        (hyperfiddle.hfql1.coalgebra/seed next-scope (unfoldT coalgebra child-seed)))
                   children)))))
         (::ana/lexical-env metas))))))

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
   (->CofreeT (seek env (runCofreeT t)))))

(defmacro -hfql [hfql] `(unfoldT hfqlT (fmap swap (ana/hfql ~hfql))))
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
                         (into {}
                           (map (fn [point value]
                                  (case (projection-type point)
                                    ::ana/traversal value
                                    (pair (symbolic-edge point) value)))
                             (map extract (unwrap W))
                             (map snd children))))
                   point)
      ::ana/traversal
      (fmap (fn [_] {(symbolic-edge point)
                     (if (= ::ana/many (::ana/multiplicity (metas point)))
                       (map snd children) #_(vary-meta children assoc ::ana/multiplicity ::ana/many)
                       (snd (first children)))})
        point)
      point)))

;; FIXME eager. was lazy and should be made lazy again.
(defn pull "Equivalent of `datomic.api/pull`. Reshape an HFQL tree `W` into a post-order version of the same tree.
  Lazy in theory, eager in practice.
  `(pull W)` is O(1) and its result can be navigated just like `W`. The type is
  W a -> W a. But because the tree returned by pull is now in post-order, then
  navigating onto the first value will eagerly traverse the entire tree `W`.
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

(tests
  (snd (extract (pull (seed {'% 1} (-hfql identity)))))
  := 1

  (snd (extract (pull (seed {'% {:a 1}} (-hfql :a)))))
  := 1

  (snd (extract (pull (seed {'% {:a 1, :b 2}} (-hfql [:a])))))
  := {:a 1}

  (snd (extract (pull (seed {'% {:a 1}} (-hfql [:a :b])))))
  := {:a 1, :b nil}

  (snd (extract (pull (seed {'% {:a 1, :b 2}} (-hfql [:a :b])))))
  := {:a 1, :b 2}

  (snd (extract (pull (seed {'% {:a 1}} (-hfql [:a identity])))))
  := {:a 1, 'identity {:a 1}}

  (snd (extract (pull (seed {'% {:a 1}} (-hfql [{:a inc}])))))
  := {:a 2}

  (snd (extract (pull (seed {'% {:a 1}} (-hfql [{:a {inc inc}}])))))
  := {:a {'inc 3}}

  (snd (extract (pull (seed {'% {:a 1}} (-hfql {identity :a})))))
  := {'identity 1}

  (snd (extract (pull (seed {'% {:a 1}} (-hfql {identity [:a]})))))
  := {'identity {:a 1}}

  (snd (extract (pull (-hfql (range 3)))))
  := '(0 1 2)

  (-values (-hfql {(range 2) identity}))
  := '((0 1) (0 1))

  (-values (-hfql {(range 2) {* identity}}))
  := '((0 1) 0 1)

  (snd (extract (pull (-hfql {(range 2) {* inc}}))))
  := '{(range 2) (1 2)}

  (snd (extract (pull (-hfql {(range 2) {* {inc inc}}}))))
  := '{(range 2) ({inc 2} {inc 3})})

#?(:clj
   (tests
     (snd (extract (pull (-hfql {(ns-publics *ns*) {* val}}))))
     := {'(ns-publics *ns*) (vals (ns-publics *ns*))}
     ))

#?(:clj
   (tests
     (snd (extract (pull (-hfql {(ns-publics *ns*) {* {val symbol}}}))))
     := {'(ns-publics *ns*) (map #(hash-map 'val %) (map symbol (vals (ns-publics *ns*))))}
     ))

#?(:clj
   (tests
     (snd (extract (pull (-hfql {(ns-publics *ns*) {vals {* symbol}}}))))
     := {'(ns-publics *ns*) {'vals (map symbol (vals (ns-publics *ns*)))}}
     ))

#?(:clj
   (tests
     ;; Laziness
     (->> (-hfql {(ns-publics *ns*) {* (fn [%] (tap "IO!") (val %))}})
       pull ; CofreeT
       extract ; [metas, pulled value]
       snd ; pulled value {ns-publics (...)}
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
  (snd (extract (seed {'% {:a 1}} (-hfql *))))
  := {:a 1}

  (snd (extract (seed {'% {:a 1}} (-hfql :a))))
  := 1

  (snd (extract (seed {'% {:a 1}} (-hfql identity))))
  := {:a 1}

  (snd (extract (seed {'% {identity 'identity}} (-hfql identity))))
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

(defn reseed
  "See `seed` for a high-level overview. Given an injected Cofree node `w` and a
  new environment `env'`, re-hydrates the corresponding subtree."
  ([w] (partial reseed w))
  ([w env']
   (if-let [ast (::AST (metas (extract w)))]
     (seed env' (unfoldT hfqlT ast))
     (throw (ex-info "Missing ::AST backref. Cannot reseed." {})))))

(tests
  "reseeding"
  (let [w (seed {'% 1} (-hfql {inc {inc {inc inc}}}) )]
    (snd (extract w)) := 2
    (let [w2 (first (unwrap w))]
      [(-values w2)
       (-values (reseed w2 {'% 10}))])

    := '[(3 4 5) (11 12 13)]
    ))

(tests
  (-values (reseed (seed {'% {:a 1}} (-hfql [:a])) {'% {:a 10}}))
  := '({:a 10} 10))




;;; Extra tests


(tests
  (-values (seed {'% {:a 1, :b 2}} (-hfql :a)))
  := '(1)

  ;; (-values (seed {'% {:a 1, :b 2, 'c 3}} (-hfql 'c))) ; FAIL at analyze phase due to extra quote
  ;; := '(3)

  (-values (seed {'% {:a 1, :b 2, 'c 3, "d" 4}} (-hfql "d")))
  := '(4)

  (-values (seed {'% {:a 1, :b 2}} (-hfql str)))
  := '("{:a 1, :b 2}")
  )

#_ ; TODO port to CofreeT API
(tests
  ((hfql 'str) {'str 'str, str str, #'str #'str})
  := ['str nil]
  ((hfql str) {'str 'str, str str, #'str #'str})
  := [str nil]
  ((hfql #'str) {'str 'str, str str, #'str #'str})
  := ["{str str, #function[clojure.core/str] #function[clojure.core/str], #'clojure.core/str #'clojure.core/str}"
      nil]
  ((hfql '#'str) {'str 'str, str str, #'str #'str})
  := [#'clojure.core/str nil]

  ((hfql 'str) {str str, #'str #'str})
  := [nil nil]
  ((hfql '#'str) {'str 'str, str str})
  := [nil nil]
  )






;;; Misc

(defn- unchunk-seq ; keeping around for reference
  "Turn a clojure chunked seq into a one-step-at-a-time seq. Noop on non-chunked
  seqs. Preserves eventual metadata. Must be called on the original chunked seq.
  Calling `unchunk-seq` on any descendent seq won't have any effect.
  E.g. `(unchunk-seq (range 3))` – turns a chunked LongRange into a proper
  lazy-seq. But `(unchunk-seq (map prn (range 3)))` will still print 0 1 2 because
  `map` eagerly preprocesses the first chunk.

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
  "
  [aseq]
  (if (chunked-seq? aseq)
    (with-meta
      (lazy-seq
        (when-let [s (seq aseq)]
          (cons (first s) (unchunk-seq (rest s)))))
      (meta aseq))
    aseq))

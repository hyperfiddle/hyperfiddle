(ns hyperfiddle.hfql1.coalgebra
  (:refer-clojure :exclude [extend])
  (:require
   [clojure.core.protocols :as ccp]
   [clojure.datafy :as datafy]
   [hyperfiddle.hfql1.functor :refer [fmap]]
   [hyperfiddle.hfql1.comonad :refer [extract duplicate]]
   [hyperfiddle.hfql1.cofree-comonad :as coco :refer [values synthesize unwrap cofreeT ->Cofree unwrapT]]
   [hyperfiddle.hfql1.monad-reader :refer [asks reader local runReader]]
   [hyperfiddle.rcf :refer [tests]]))

(defn safe-ccp-nav [object key value]
  (if (satisfies? ccp/Navigable object)
    (datafy/nav object key value)
    value))

(def ^:dynamic *navigate* safe-ccp-nav) ; default is to navigate objects, except in UIs where, at leaves, where we want to see the symbolic values

(defn- projection-type [projection]
  (or #_(preanalyzed-projection-type projection)
    (some-> projection meta ::projection projection-type)
    (cond
      (= '* projection)    ::star
      (ident? projection)  ::ident
      (vector? projection) ::pull
      (map? projection)    ::traversal
      (ifn? projection)    ::ifn)))

(defn- pull-key [expr]
  (cond
    (map? expr) (key (first expr))
    :else expr))

(defn- getter [key] (asks (fn [%] (*navigate* (force %) key (get (force %) key)))))
(defn- get-or-call [ifn]
  (asks (fn [%]
          (delay
            (let [% (force %)]
              (if (try (contains? % ifn) ; FIXME find a better check
                       (catch #?(:clj Throwable, :cljs :default) _
                         #_(#?(:clj log/debug, :cljs js/console.debug) "object doesn't support `contains?`" (type object))
                         false))
                (runReader (getter ifn) %)
                (apply ifn % ()) ; FIXME provide args
                ))))))

(defn- touch [object] (if (seqable? object) (doall object) object))
(defn- star [object] (delay (touch (force object))))

(defn leaf [reader] (fmap (fn [v] [v ()]) reader))

(declare unchunk-seq)

(defn hfqlT ; TODO if possible, express in term of Ψ and unfoldT/coiterT. I.e. separate ψ from unfold, ensuring no recursivity.
  "Analyze phase: turns a symbolic HFQL expression into a CofreeT Reader, producing an HFQL program
  with no initial value (no seed) and so no dimension. Use (seed ... env) to produce a concrete
  Cofree Comonad, lazily realizing successive navs as a tree traversal."
  [projection]
  (->
    (case (projection-type projection)
      ::ident (cofreeT (leaf (getter projection)))
      ::star (vary-meta (cofreeT (leaf (asks star)))
               assoc ::star? true)
      ::ifn (cofreeT (leaf (get-or-call projection)))
      ::pull (let [build-kids (fn [kids] (map #(vary-meta (hfqlT %) assoc ::key (pull-key %)) kids))
                   kids (build-kids projection)
                   kids-no-star (remove #(::star? (meta %)) kids)]
               (cofreeT
                 (asks (fn [m]
                         (let [kids (delay (map (fn [pt] [(::key (meta pt)) (force (first (runReader (unwrapT pt) m)))]) kids))
                               with-star (delay (into {} (force kids)))
                               base (delay (let [base (force with-star)]
                                             (if (contains? base '*)
                                               (into (get base '*) (dissoc base '*))
                                               base)))]
                           [base
                            (delay
                              (let [dynamic-star-keys (keys (get (force with-star) '*))
                                    static-user-keys (map (comp ::key meta) kids-no-star)
                                    new-star-keys (remove (set static-user-keys) dynamic-star-keys)]
                                (concat kids-no-star (build-kids new-star-keys))))])))))
      ::traversal
      (let [[f sub] (first projection) ; assumes all multi traversals have been normalized to single traversals already
            star? (and (map? sub) (contains? sub '*))]
        (-> (cofreeT (asks (fn [env]
                             (let [childT (hfqlT (if star? (get sub '*) sub))
                                   v (delay (f env))
                                   next-child (fn [v]
                                                (delay
                                                  (-> (cofreeT (local (constantly (force v)) (unwrapT childT)))
                                                    (vary-meta merge (meta childT) {::param-base childT}))))]
                               [v (delay (map next-child (if star? (unchunk-seq (force v)) [(force v)])))]))))
          (vary-meta assoc ::star star?))))
    (vary-meta assoc ::projection projection, ::key (pull-key projection))))

;;;;;;;;;;
;; Pull ;;
;;;;;;;;;;

(defn- pull-algebra [W children]
  (let [v (extract W)]
    (case (projection-type W)
      ::pull (reduce (fn [r [k child]]
                       (if (map? child)
                         (assoc r k (get child k child))
                         (assoc r k child)))
               v (map vector (keys v) children))
      ::traversal {(::key (meta W))
                   (if (::star (meta W))
                     (vary-meta children assoc ::star true)
                     (first children))}
      (if (empty? children)
        v
        {(::key (meta W)) v}))))

(def pull "Equivalent of `datomic.api/pull`. Reshape an HFQL tree `W` into a post-order version of the same tree.
  Lazy in theory, eager in practice.
  `(pull W)` is O(1) and its result can be navigated just like `W`. The type is
  W a -> W a. But because the tree returned by pull is now in post-order, then
  navigating onto the first value will eagerly traverse the entire tree `W`.
  Further navigation will still be lazy, but will be produced from an already
  fully realized tree.

  Asking for all values of (pull W) means traversing the eagerly projected
  expression. This can be seen as a \"renavigation\" onto a precomputed tree.
  "
  ;; Note `pull` is defined in term of comonadic operations. `pull-algebra` is the
  ;; interpreter "zipping" values up the tree. The sole purpose of `pull-algebra` is to re-attach
  ;; keys onto projected values, producing a result of the same shape as the original symbolic
  ;; query.
  (comp (synthesize pull-algebra) duplicate))

;;; ----------------------------------

(defn seed
  "Turns an HFQL program into a fully lazily navigable structure by providing an
  initial environment value. An HFQL program is built by calling `(hfqlT
  <hfql-expr>)`.

  Once seeded, an HFQL tree can be lazily traversed (think navigated, e.g. with tree-seq) without
  forcing any userland side effect, up to multiplicity-many branching points (e.g. *)¹.

  Any point of the tree can be re-seeded using `reseed`.

  Seeding \"hydrates\" an HFQL program into a navigable tree.
  Reseed \"overwrites\" an evaluation context with an arbitrary value, effectively \"rehydrating\"
  the original HFQL (sub)program at point.
   ↳ Use case: inject function arguments from UI.

  Both `seed` and `reseed` are pure and can be considered O(1)².

  Concretely: Turns the value returned by `hfqlT` (a CofreeT) virtual tree with no state (no Reader
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
[t env]
  (let [t (force t)
        [a kidsT] (runReader (unwrapT t) env)]
    (-> (->Cofree a
          (delay
            (sequence
              (comp (map (juxt identity #(seed % env)))
                (map (fn [[pt c]]
                       (let [base (or (::param-base (meta (force pt))) (force pt))]
                         (vary-meta c #(merge % {::param base}))))))
              (unchunk-seq (force kidsT)))))
      (vary-meta #(merge (meta t) {::param t} %)))))

(defn reseed
  "See `seed` for a high-level overview. Given an injected Cofree node `w` and a
  new environment `env'`, re-hydrates the corresponding subtree."
  [w env']
  (if-let [t (::param (meta w))]
    (seed t env')
    (throw (ex-info "No ::param backref on node; cannot reseed" {}))))


(defn- unfoldT ; for future improvements
  "unfoldT :: (b -> Reader env [a, f b]) -> b -> CofreeT f Reader a
   `coalgebra b` produces a Reader that, when run, yields (value, next seeds)."
  [coalgebra seed]
  (cofreeT
    (reader
      (fn [env]
        (let [[a bs] (runReader (coalgebra seed) env)]
          [a (map #(unfoldT coalgebra %) bs)])))))

(defn- coiterT ; for future improvements
  "coiterT :: (a -> Reader env (f a)) -> a -> CofreeT f Reader a
   Special case where the node label is the seed itself."
  [coalgebra a0]
  (cofreeT
    (reader
      (fn [env]
        (let [fa (runReader (coalgebra a0) env)]
          [a0 (map #(coiterT coalgebra %) fa)])))))

;;; Misc

(defn- unchunk-seq
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


;;;;;;;;;;;
;; TESTS ;;
;;;;;;;;;;;

(tests
  ;; coiterT with branching that depends on env (Reader)
  (defn psi-branch [n]
    (reader (fn [_] (when (pos? n) [(dec n) (dec n)]))))  ; ignore env here
  (def tT (coiterT psi-branch 2))
  (def t  (seed tT :env))
  (map extract (mapcat unwrap (unwrap t)))  ;; => '(0 0 0 0)

  )

(tests
  ;; unfoldT where label depends on env
  (defn psi-plus [n] (reader (fn [e] [(+ n e) []])))
  (extract (seed (unfoldT psi-plus 10) 5))
  := 15
  )


(tests
  ;; Simple HFQL-style case {identity [:a :b]} via unfoldT
  (defn psi-pull [spec]
    (reader
      (fn [m]
        (cond
          (= spec identity) [m []]
          (ident? spec)     [(spec m) []]
          (vector? spec)    [(select-keys m spec) (seq spec)]
          :else             [nil []]))))
  (values (seed (unfoldT psi-pull identity ) {:a 1 :b 2}))
  := '({:a 1, :b 2})

  (values (seed (unfoldT psi-pull [:a]) {:a 1 :b 2}))
  := '({:a 1} 1)

  )


(tests
  (values (seed (hfqlT inc) 1))
  := '(2)

  (values (seed (hfqlT :a) {:a 1}))
  := '(1)

  (values (seed (hfqlT [:a]) {:a 1, :b 2}))
  := '({:a 1} 1)

  (values (seed (hfqlT [:a '*]) {:a 1, :b 2}))
  := '({:a 1, :b 2} 1 2)

  (values (seed (hfqlT {:a inc}) {:a 1, :b 3}))
  := '(1 2)

  (values (seed (hfqlT [{:a inc}]) {:a 1, :b 3}))
  := '({:a 1} 1 2)

  (values (seed (hfqlT {inc inc}) 1))
  := '(2 3)

  (values (seed (hfqlT {inc {inc {inc inc}}}) 1))
  := '(2 3 4 5)

  (let [fork (fn [x] (list x x))]
    (values (seed (hfqlT {identity {fork {fork fork}}}) '.)))
   := '(              .
                    (. .)
                ((. .) (. .))
        (((. .) (. .)) ((. .) (. .)))) ; 8 elements


   (let [fork (fn [x] (list x x))]
     (values (seed (hfqlT {identity {fork {'* {fork {'* fork}}}}}) '.)))
    := '(. (. .) (. .) (. .) (. .) (. .) (. .) (. .)) ; 8 elements
    ;; * doesn't flatten, it fans-out, `values` is responsible for the apparent flattening. Values arbitrarily traverses in pre-order.

  )

(tests
  "calling fn in pull"
  (values (seed (hfqlT [identity :a]) {:a 1, :b 2}))
  := [{identity {:a 1, :b 2}, :a 1} {:a 1, :b 2} 1]
  )

(tests
  "traversal in pull"
  (values (seed (hfqlT ['* {:a inc}]) {:a 1, :b "b"}))
  := '({:a 1, :b "b"} 1 2 "b")
  )

(tests
  (extract (pull (seed (hfqlT identity) 1)))
  := 1

  (extract (pull (seed (hfqlT :a) {:a 1})))
  := 1

  (extract (pull (seed (hfqlT [:a]) {:a 1, :b 2})))
  := {:a 1}

  (extract (pull (seed (hfqlT [:a :b]) {:a 1})))
  := {:a 1, :b nil}

  (extract (pull (seed (hfqlT [:a :b]) {:a 1, :b 2})))
  := {:a 1, :b 2}

  (extract (pull (seed (hfqlT [:a identity]) {:a 1})))
  := {:a 1, identity {:a 1}}

  (extract (pull (seed (hfqlT [{:a inc}]) {:a 1})))
  := {:a 2}

  (extract (pull (seed (hfqlT [{:a {inc inc}}]) {:a 1})))
  := {:a {inc 3}}

  (extract (pull (seed (hfqlT {identity :a}) {:a 1})))
  := {identity 1}

  (extract (pull (seed (hfqlT {identity [:a]}) {:a 1})))
  := {identity {:a 1}}

  (extract (pull (seed (hfqlT range) 3)))
  := '(0 1 2)

  (extract (pull (seed (hfqlT {range {'* {inc inc}}}) 3)))

  (extract (pull (seed (hfqlT {ns-publics {'* val}}) *ns*)))

  )


(tests
  (extract (seed (hfqlT '*) {:a 1}))
  := {:a 1}

  (extract (seed (hfqlT :a) {:a 1}))
  := 1

  (extract (seed (hfqlT identity) {:a 1}))
  := {:a 1}

  (extract (seed (hfqlT identity) {identity 'identity}))
  := 'identity

  )

(tests
  (values (seed (hfqlT [:a :b]) {:a 1, :b 2, :c 3}))
  := '({:a 1, :b 2} 1 2)
  )


(tests
  (values (pull (seed (hfqlT :a) {:a 1, :b 2, :c 3})))
  := '(1)

  (values (pull (seed (hfqlT [:a :b]) {:a 1, :b 2, :c 3})))
  := '({:a 1, :b 2} 1 2)

  )

(tests
  (values (pull (seed (hfqlT {identity identity}) 1)))
  := [{identity 1} 1]

  (values (pull (seed (hfqlT {identity inc}) 1)))
  := [{identity 2} 2]

  (values (pull (seed (hfqlT {inc inc}) 1)))
  :=
  [{inc 3}
   3]

  (values (pull (seed (hfqlT {inc {inc {inc inc}}}) 1)))
  := [{inc {inc {inc 5}}}
      {inc {inc 5}}
      {inc 5}
      5]

  (values (seed (hfqlT {identity [:a]}) {:a 1, :b 2}))
  := '({:a 1, :b 2} {:a 1} 1)

  (values (pull (seed (hfqlT {identity [:a]}) {:a 1, :b 2})))
  := [{identity {:a 1}} {:a 1} 1]
  )

(tests
  (values (seed (hfqlT {range {'* inc}}) 3))
  := '((0 1 2) 1 2 3)

  (values (pull (seed (hfqlT {range {'* inc}}) 3)))
  := [{range '(1 2 3)} 1 2 3]

 )

(tests
  "reseeding"
  (let [w (seed (hfqlT {inc {inc {inc inc}}}) 1)]
    (extract w) := 2
    (let [w2 (first (unwrap w))]
      [(values w2)
       (values (reseed w2 10))])

    := '[(3 4 5) (11 12 13)]
    ))

(tests
  (values (reseed (seed (hfqlT [:a]) {:a 1}) {:a 10}))
  := '({:a 10} 10))
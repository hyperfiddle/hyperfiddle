(ns hyperfiddle.hfql1.coalgebra
  (:refer-clojure :exclude [extend resolve])
  (:require
   [clojure.core.protocols :as ccp]
   [contrib.data :refer [group-by-pred]]
   [clojure.datafy :as datafy]
   [hyperfiddle.hfql1.function :refer [flip]]
   [hyperfiddle.hfql1.functor :refer [Functor fmap]]
   [hyperfiddle.hfql1.bifunctor :as bf :refer [Bifunctor IPair fst snd pair swap bimap &&&]]
   [hyperfiddle.hfql1.monoid :refer [mappend]]
   [hyperfiddle.hfql1.comonad :refer [Comonad extract duplicate extend -extract]]
   [hyperfiddle.hfql1.cofree :refer [synthesize unwrap runCofreeT ->CofreeF ->CofreeT headF tree-seqT memoize-1 hoistCofreeT]]
   [hyperfiddle.hfql1.store :refer [store seek seeks peeks #?(:cljs Store)]]
   [hyperfiddle.hfql1.monad-reader :refer [reader local runReader]]
   [hyperfiddle.rcf :refer [tests tap %]]
   [hyperfiddle.hfql1.analyzer :as ana])
  #?(:clj (:import [hyperfiddle.hfql1.store Store]))
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

(defn focus-arguments [metas]
  (local (fn [env] (map #(resolve % env) (::ana/parameters metas)))))

(def mem-reader "Like `monad-reader/reader` but the read function only reruns if the environment has changed." (comp reader memoize-1))

(defn- getter [metas]
  (let [key (get metas ::ana/form)]
    (mem-reader ;; reader is algebraically equivalent to a plain function (fn [env] ...). But we keep an
      ;; explicit reader for clarity. Also mem-reader is optimized
      (fn [%] (*navigate* % key (get % key))))))

(def ^:private dup-first ":: (a, b) -> (a, a)" (&&& fst fst))

(defn- get-or-call [metas]
  (mem-reader ; explicit reader for clarity
    (fn [%]
      (let [ifn (::ana/value metas)]
        (if (try (contains? % ifn) ; FIXME find a better check
                 (catch #?(:clj Throwable, :cljs :default) _
                   #_(#?(:clj log/debug, :cljs js/console.debug) "object doesn't support `contains?`" (type object))
                   false))
          (*navigate* % ifn (get % ifn))
          (apply ifn [%])
          )))))

(defn- call [metas]
  (mem-reader
    (fn [args]
      (apply (::ana/value metas) args))))

(defn- touch [object] (if (seqable? object) (doall object) object))
(def ^:private star (mem-reader touch))

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
                          (fmap headF)   ; inner layer
                          )]
    (into {} (map pair (fmap symbolic-edge realized-kids)
               (fmap snd realized-kids)))))

(declare unfoldT)

(defn recursion-point? [hfql]
  (or (::ana/recursion-point (metas (extract hfql)))
    (let [child (first (unwrap hfql))]
      (and (star-traversal? (extract child))
        (recursion-point? child)))))

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
          ::ana/var     (comp leaf focus-% getter)
          ::ana/fn      (comp leaf focus-% get-or-call)
          ::ana/call    (comp leaf (focus-arguments (metas point)) call)
          ::ana/lambda  (comp leaf (focus-arguments (metas point)) call)
          ::ana/star    (comp leaf focus-% (constantly star))
          ::ana/pull
          (constantly
            (mem-reader
              (fn [env]
                (let [[[star?] kids] (group-by-pred (comp star? extract) children)
                      dynamic-keys (when star? (keys (runReader star (get env '%))))
                      star-kids (let [static-keys (set (map (comp symbolic-edge extract) kids))
                                      remaining-star-keys (remove static-keys dynamic-keys)]
                                  (fmap (fmap swap) (unwrap (ana/analyze {} (vec remaining-star-keys)))))]
                  (pair (delay (mappend
                                 (pull-peek-at-immediate-kids! env (map (unfoldT hfqlT) kids))
                                 (pull-peek-at-immediate-kids! env (map (unfoldT hfqlT) star-kids))))
                    (mappend kids star-kids))))))
          ::ana/traversal
          (let [edgeT (unfoldT hfqlT (fmap swap (ana/hydrate {::ana/form (::ana/form (::ana/edge (metas point))) ; FIXME wart, should be handled by the analyzer
                                                              ::ana/meta (::ana/edge (metas point))})))]
            (constantly
              (mem-reader
                (fn [env]
                  (let [[_metas new-context] (extract (seed env edgeT))]
                    (pair new-context
                      (let [direct-child (first children)]
                        (if (recursion-point? hfql)
                          (let [limit (or (::ana/recursion-limit (metas (extract direct-child))) (::ana/recursion-limit (metas (extract root))))]
                            (if (> limit 1)
                              [(fmap (bf/first #(assoc % ::ana/recursion-limit (dec limit) )) root)]
                              []))
                          (if (star-traversal? (extract direct-child))
                            [(first (unwrap direct-child))]
                            [direct-child]))))))))))]
    (fmap transform point)))

(defprotocol SetScope
  (set-scope-value [w scope]))

(declare env-w)

(defrecord EnvW [meta w]
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
  (set-scope-value [_ scope] (env-w (assoc meta ::scope scope) (set-scope-value w scope))))

(defn env-w [meta w]
  (assert (satisfies? Comonad w))
  (->EnvW meta w))

(extend-type Store
  SetScope
  (set-scope-value [w scope] (seek scope w)))

(defn- unfoldT
  ([coalgebra] (partial unfoldT coalgebra))
  ([coalgebra AST]
   (let [[metas step :as t] (coalgebra AST)]
     (hoistCofreeT (fn [w] (->EnvW metas w))
       (->CofreeT
         (store
           (fn peek [scope]
             (let [[value children] (runReader step scope)]
               (->CofreeF (pair (assoc metas ::scope scope) value)
                 (if-not (= ::ana/many (::ana/multiplicity metas)) ; if multiplicity one
                   ;; don't use fmap under here - we want to enforce lazy children.
                   (map ; for each child, unfold it and seed it with next scope.
                     (comp (seed (case (projection-type t) ; decide what's next scope.
                                   ::ana/pull scope ; a "pull" point doesn't alter '% – it doesn't run anything. Only collects kvs.
                                   (assoc scope '% value)))
                       (unfoldT coalgebra))
                     children)
                   (if (empty? children) ; if recursion is bounded and over, * won't have more children
                     ()
                     (map ; Unfold next child once, then fan-out by seeding it multiple times in parallel, once for each value. Put each value in scope as '%.
                                        ; There can only be one child, because fan-outs happen on traversals only and a traversal only has one continuation.
                       (comp (flip seed (unfoldT coalgebra (first children)))
                         (partial assoc scope '%)) ; no need to check if current point is "pull" or not. A pull cannot be multiplicity many.
                       value))))))
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
(defn- -values [T] (map force (fmap snd (map extract (tree-seqT  T)))))


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
                     (if (seq children) ; regular traversal always have children, but bounded recursion final point doesn't.
                       (if (= ::ana/many (::ana/multiplicity (metas point)))
                         (map snd children)
                         (snd (first children)))
                       ;; bounded recursion case, no more children, return value as-is.
                       (snd point))})
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
  "recursive pull" ; datomic pull does detect cycles and return only :db/id at leaves.
  (snd (extract (pull (seed '{% 1} (-hfql {inc [str {identity 2}]})))))
  := '{inc {str "2", identity {inc {str "3", identity 3}}}})

#?(:clj
   (tests
     "Unbounded pull will overflow"
     (snd (extract (pull (seed {'% nil} (-hfql {identity ...})))))
     :throws java.lang.StackOverflowError
     ))

(tests
  "recursive pull across star traversal"
  (snd (extract (pull (seed {'% (range 3)} (-hfql {identity {* 2}})))))
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
  "reseeding"
  (let [w (seed {'% 1} (-hfql {inc {inc {inc inc}}}) )]
    (snd (extract w)) := 2
    (let [w2 (first (unwrap w))]
      [(-values w2)
       (-values (seed {'% 10} w2))])

    := '[(3 4 5) (11 12 13)]
    ))

(tests
  (-values (seed {'% {:a 10}} (seed {'% {:a 1}} (-hfql [:a]))))
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

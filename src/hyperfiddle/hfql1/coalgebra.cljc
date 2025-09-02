(ns hyperfiddle.hfql1.coalgebra
  (:refer-clojure :exclude [extend])
  (:require
   [clojure.core.protocols :as ccp]
   [clojure.datafy :as datafy]
   [hyperfiddle.hfql1.functor :refer [fmap]]
   [hyperfiddle.hfql1.comonad :refer [extract duplicate]]
   [hyperfiddle.hfql1.cofree-comonad :as coco :refer [unfold values synthesize unwrap cofreeT ->Cofree unwrapT]]
   [hyperfiddle.hfql1.monad-reader :refer [asks reader local runReader]]
   [hyperfiddle.rcf :refer [tests]]))

(defn safe-ccp-nav [object key value]
  (if (satisfies? ccp/Navigable object)
    (datafy/nav object key value)
    value))

(def ^:dynamic *navigate* safe-ccp-nav)

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
          (let [% (force %)]
            (if (try (contains? % ifn) ; FIXME find a better check
                     (catch #?(:clj Throwable, :cljs :default) _
                       #_(#?(:clj log/debug, :cljs js/console.debug) "object doesn't support `contains?`" (type object))
                       false))
              (runReader (getter ifn) %)
              (apply ifn % ()) ; FIXME provide args
              )))))

(defn- touch [object] (if (seqable? object) (doall object) object))
(defn- star [object] (touch (force object)))

(defn leaf [reader] (fmap (fn [v] [v ()]) reader))

(defn hfqlT ; TODO if possible, express in term of Ψ and unfoldT/coiterT. I.e. separate ψ from unfold
  "Analyze phase: turn a HFQL expression into a CofreeT Reader, producing an HFQL evaluator with no initial value.
  Use (injectR ... env) to produce a concrete Cofree Comonad, lazily realizing successive navs as a tree traversal."
  [projection]
  (->
    (case (projection-type projection)
      ::ident (cofreeT (leaf (getter projection)))
      ::star (cofreeT (leaf (asks star) ))
      ::ifn (cofreeT (leaf (get-or-call projection)))
      ::pull (let [kids (map (fn [s] (vary-meta (hfqlT s) assoc ::key (pull-key s))) projection)]
               (cofreeT
                 (asks (fn [m]
                         [(into {} (map (fn [pt] [(::key (meta pt)) (first (runReader (unwrapT pt) m))])
                                     kids))
                          kids]))))
    ::traversal
    (let [[f sub] (first projection) ; assumes all multi traversals have been normalized to single traversals already
          star? (and (map? sub) (contains? sub '*))]
      (-> (cofreeT (asks (fn [env]
                           (let [childT (hfqlT (if star? (get sub '*) sub))
                                 v (f env)
                                 next-child (fn [v]
                                              (-> (cofreeT (local (constantly v) (unwrapT childT)))
                                                (vary-meta merge (meta childT) {::param-base childT})))]
                             [v (iterator-seq (.iterator (eduction (map next-child) (if star? v [v]))))]))))
        (vary-meta assoc ::star star?))))
    (vary-meta assoc ::projection projection, ::key (pull-key projection))))

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

(def pull (comp (synthesize pull-algebra) duplicate))



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

(defn- trully-lazy-map [f & colls]
  (apply sequence (map f) colls))

(defn unchunk [s]
  (iterator-seq (.iterator s))
  #_(lazy-seq
    (when-let [s (seq s)]
      (cons (first s) (unchunk (rest s))))))

(defn injectR [t env]
  (let [[a kidsT] (runReader (unwrapT t) env)]
    (-> (->Cofree (delay (force a))
          (unchunk(map (fn [pt c]
                         (let [base (or (::param-base (meta pt)) pt)]
                           (with-meta c (assoc (meta c) ::param base))))
                    (unchunk kidsT) (unchunk (map #(force (injectR % env)) kidsT)))))
      (vary-meta #(merge (meta t) % {::param t})))))

(comment
  (def _range (fn [x] (prn "called range"x ) (sequence (map identity) (range x))))
  (def _inc (fn [x] (Thread/sleep 10) (prn "called inc" x ) (inc x)))

  (time (def _C (injectR (hfqlT {_range {'* _inc}}) 10000000)))
  (extract _C)
  (extract (nth (unwrap _C) 1000))
  (values _C)
  )

(defn reseed
  "Given an injected Cofree node `w` and a new environment `env'`, re-inject
   the corresponding subtree."
  [w env']
  (if-let [t (::param (meta w))]
    (injectR t env')
    (throw (ex-info "No ::param backref on node; cannot reseed" {}))))

(tests
  "reseeding"
  (let [w (injectR (hfqlT {inc {inc {inc inc}}}) 1)]
    (extract w) := 2
    (let [w2 (first (unwrap w))
          w3 (reseed w2 10)]
      (extract w2) := 3
      (values w3)
      )
    )


  ;; (values (reseed (injectR (hfqlT [:a]) {:a 1}) {:a 10}))
  (values (reseed (injectR (hfqlT {inc {inc {inc inc}}}) 1) 100))
  )



(tests
  ;; coiterT with branching that depends on env (Reader)
  (defn psi-branch [n]
    (reader (fn [_] (when (pos? n) [(dec n) (dec n)]))))  ; ignore env here
  (def tT (coiterT psi-branch 2))
  (def t  (injectR tT :env))
  (map extract (mapcat unwrap (unwrap t)))  ;; => '(0 0 0 0)

  )

(tests
  ;; unfoldT where label depends on env
  (defn psi-plus [n] (reader (fn [e] [(+ n e) []])))
  (extract (injectR (unfoldT psi-plus 10) 5))
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
  (values (injectR (unfoldT psi-pull identity ) {:a 1 :b 2}))
  := '({:a 1, :b 2})

  (values (injectR (unfoldT psi-pull [:a]) {:a 1 :b 2}))
  := '({:a 1} 1)

  )


(tests
  (values (injectR (hfqlT inc) 1))
  := '(2)

  (values (injectR (hfqlT :a) {:a 1}))
  := '(1)

  (values (injectR (hfqlT [:a]) {:a 1, :b 2}))
  := '({:a 1} 1)

  (values (injectR (hfqlT [:a '*]) {:a 1, :b 2})) ; FAIL
  := '({:a 1, :b 2} 1 2)

  (values (injectR (hfqlT {:a inc}) {:a 1, :b 3}))
  := '(1 2)

  (values (injectR (hfqlT [{:a inc}]) {:a 1, :b 3}))
  := '({:a 1} 1 2)

  (values (injectR (hfqlT {inc inc}) 1))
  := '(2 3)

  (values (injectR (hfqlT {inc {inc {inc inc}}}) 1))
  := '(2 3 4 5)

  (let [fork (fn [x] (list x x))]
    (values (injectR (hfqlT {identity {fork {fork fork}}}) '.)))
   := '(              .
                    (. .)
                ((. .) (. .))
        (((. .) (. .)) ((. .) (. .)))) ; 8 elements


   (let [fork (fn [x] (list x x))]
     (values (injectR (hfqlT {identity {fork {'* {fork {'* fork}}}}}) '.)))
    := '(. (. .) (. .) (. .) (. .) (. .) (. .) (. .)) ; 8 elements
    ;; * doesn't flatten, it fans-out, `values` is responsible for the apparent flattening. Values arbitrarily traverses in pre-order.

  )

(tests
  "calling fn in pull"
  (values (injectR (hfqlT [identity :a]) {:a 1, :b 2}))
  := [{identity {:a 1, :b 2}, :a 1} {:a 1, :b 2} 1]
  )

(tests
  "traversal in pull"
  (values (injectR (hfqlT ['* {:a inc}]) {:a 1, :b 2})) ; FAIL
  := '({:a 1, :b 2} 1 2)
  )


(tests
  (extract (pull (injectR (hfqlT identity) 1)))
  := 1

  (extract (pull (injectR (hfqlT :a) {:a 1})))
  := 1

  (extract (pull (injectR (hfqlT [:a]) {:a 1, :b 2})))
  := {:a 1}

  (extract (pull (injectR (hfqlT [:a :b]) {:a 1})))
  := {:a 1, :b nil}

  (extract (pull (injectR (hfqlT [:a :b]) {:a 1, :b 2})))
  := {:a 1, :b 2}

  (extract (pull (injectR (hfqlT [:a identity]) {:a 1})))
  := {:a 1, identity {:a 1}}

  (extract (pull (injectR (hfqlT [{:a inc}]) {:a 1})))
  := {:a 2}

  (extract (pull (injectR (hfqlT [{:a {inc inc}}]) {:a 1})))
  := {:a {inc 3}}

  (extract (pull (injectR (hfqlT {identity :a}) {:a 1})))
  := {identity 1}

  (extract (pull (injectR (hfqlT {identity [:a]}) {:a 1})))
  := {identity {:a 1}}

  (extract (pull (injectR (hfqlT range) 3)))
  := '(0 1 2)

  (extract (pull (injectR (hfqlT {range {'* {inc inc}}}) 3)))

  (extract (pull (injectR (hfqlT {ns-publics {'* val}}) *ns*)))

  )


(tests
  (extract (injectR (hfqlT '*) {:a 1}))
  := {:a 1}

  (extract (injectR (hfqlT :a) {:a 1}))
  := 1

  (extract (injectR (hfqlT identity) {:a 1}))
  := {:a 1}

  (extract (injectR (hfqlT identity) {identity 'identity}))
  := 'identity

  )

(tests
  (values (injectR (hfqlT [:a :b]) {:a 1, :b 2, :c 3}))
  := '({:a 1, :b 2} 1 2)
  )


(tests
  (values (pull (injectR (hfqlT :a) {:a 1, :b 2, :c 3})))
  := '(1)

  (values (pull (injectR (hfqlT [:a :b]) {:a 1, :b 2, :c 3})))
  := '({:a 1, :b 2} 1 2)

  )

(tests
  (values (pull (injectR (hfqlT {identity identity}) 1)))
  := [{identity 1} 1]

  (values (pull (injectR (hfqlT {identity inc}) 1)))
  := [{identity 2} 2]

  (values (pull (injectR (hfqlT {inc inc}) 1)))
  :=
  [{inc 3}
   3]

  (values (pull (injectR (hfqlT {inc {inc {inc inc}}}) 1)))
  := [{inc {inc {inc 5}}}
      {inc {inc 5}}
      {inc 5}
      5]

  (values (injectR (hfqlT {identity [:a]}) {:a 1, :b 2}))
  := '({:a 1, :b 2} {:a 1} 1)

  (values (pull (injectR (hfqlT {identity [:a]}) {:a 1, :b 2})))
  := [{identity {:a 1}} {:a 1} 1]

  )


(tests
  (values (injectR (hfqlT {range {'* inc}}) 3))
  := '((0 1 2) 1 2 3)

  (values (pull (injectR (hfqlT {range {'* inc}}) 3)))
  := [{range '(1 2 3)} 1 2 3]

 )


(tests
  "reseeding"
  (let [w (injectR (hfqlT {inc {inc {inc inc}}}) 1)]
    (extract w) := 2
    (let [w2 (first (unwrap w))]
      [(values w2)
       (values (reseed w2 10))]))
  := '[(3 4 5) (11 12 13)]

  (values (reseed (injectR (hfqlT [:a]) {:a 1}) {:a 10}))
  := '({:a 10} 10)

  )
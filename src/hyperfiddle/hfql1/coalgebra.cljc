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

(defn- getterR [key] (asks (fn [%] (*navigate* (force %) key (get (force %) key)))))
(defn- get-or-callR [ifn]
  (asks (fn [%]
          (let [% (force %)]
            (if (try (contains? % ifn) ; FIXME find a better check
                     (catch #?(:clj Throwable, :cljs :default) _
                       #_(#?(:clj log/debug, :cljs js/console.debug) "object doesn't support `contains?`" (type object))
                       false))
              (runReader (getterR ifn) %)
              (apply ifn % ()) ; FIXME provide args
              )))))

(defn- touch [object] (if (seqable? object) (doall object) object))
(defn- star [object] (touch (force object)))

(defn leaf [reader] (fmap (fn [v] [v ()]) reader))

(defn hfqlT ; TODO express in term of Ψ and unfoldT/coiterT separate ψ from unfold
  "Analyze phase: turn a HFQL expression into a CofreeT Reader, producing an HFQL evaluator with no initial value.
  Use (injectR ... env) to produce a concrete Cofree Comonad, lazily realizing successive navs as a tree traversal."
  [projection]
  (->
    (case (projection-type projection)
      ::ident (cofreeT (leaf (getterR projection)))
      ::star (cofreeT (leaf (asks star) ))
      ::ifn (cofreeT (leaf (get-or-callR projection)))
      ::pull (let [kids (map (fn [s] (vary-meta (hfqlT s) assoc ::key (pull-key s))) projection)]
               (cofreeT
                 (asks (fn [m]
                         [(into {} (map (fn [pt] [(::key (meta pt)) (first (runReader (unwrapT pt) m))])
                                     kids))
                          kids]))))
    ::traversal
    (let [[f sub] (first projection)] ; assumes all multi traversals have been normalized to single traversals already
      (cond
        (and (map? sub) (contains? sub '*)) ; {f {'* sub}}
        (let [childT (hfqlT (get sub '*))]
          (vary-meta
            (cofreeT (asks (fn [a] (let [ys (f a)] ; TODO template call args
                                     [ys (map (fn [e] ; child uses env = e
                                                (vary-meta
                                                  (cofreeT (local (constantly e) (unwrapT childT)))
                                                  merge (meta childT)))
                                           ys)]))))
            assoc ::star true))

        :else
        (let [subT (when sub (hfqlT sub))]
          (cofreeT (asks (fn [%]
                           (let [% (f %)]
                             [% (if subT
                                  [(vary-meta (cofreeT (local (constantly %) (unwrapT subT)))
                                     merge (meta subT))]
                                  [])]))))))))
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



(defn- unfoldT
  "unfoldT :: (b -> Reader env [a, f b]) -> b -> CofreeT f Reader a
   `coalgebra b` produces a Reader that, when run, yields (label, next seeds)."
  [coalgebra seed]
  (cofreeT
    (reader
      (fn [env]
        (let [[a bs] (runReader (coalgebra seed) env)]
          [a (map #(unfoldT coalgebra %) bs)])))))


(defn coiterT
  "coiterT :: (a -> Reader env (f a)) -> a -> CofreeT f Reader a
   Special case where the node label is the seed itself."
  [coalgebra a0]
  (cofreeT
    (reader
      (fn [env]
        (let [fa (runReader (coalgebra a0) env)]
          [a0 (map #(coiterT coalgebra %) fa)])))))

(defn injectR
  "Run a CofreeT Reader at environment `env` to obtain a plain Cofree.
   Copies ::key from *param children* to injected children; stores ::param backrefs."
  [cofree-t env]
  (let [[a kidsT] (runReader (unwrapT cofree-t) env)
        kidsC     (map #(injectR % env) kidsT)
        node      (->Cofree (delay a)
                    (map (fn [pt c] (vary-meta c #(merge (meta pt) % {::param pt})))
                      kidsT kidsC))]
    (vary-meta node #(merge (meta cofree-t) % {::param cofree-t}))))



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
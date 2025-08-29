(ns hyperfiddle.hfql1.coalgebra
  (:refer-clojure :exclude [extend])
  (:require
   [clojure.core.protocols :as ccp]
   [clojure.datafy :as datafy]
   [hyperfiddle.hfql1.comonad :refer [extract duplicate]]
   [hyperfiddle.hfql1.cofree-comonad :as coco :refer [unfold values synthesize unwrap]]
   [hyperfiddle.rcf :refer [tests]]))

(defn safe-ccp-nav [object key value]
  (if (satisfies? ccp/Navigable object)
    (datafy/nav object key value)
    value))

(def ^:dynamic *navigate* safe-ccp-nav)

(defn- getter [object key] (*navigate* (force object) key (get (force object) key)))
(defn- caller [ifn & args] (apply ifn (map force args)))
(defn- get-or-call [ifn object & args]
  (let [object (force object)]
    (if (try (contains? object ifn) ; FIXME find a better check
             (catch #?(:clj Throwable, :cljs :default) _
               #_(#?(:clj log/debug, :cljs js/console.debug) "object doesn't support `contains?`" (type object))
               false))
      (*navigate* object ifn (get object ifn))
      (apply ifn object args))))

(defn- touch [object] (if (seqable? object) (doall object) object))
(defn- star [object] (touch (force object)))



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

(defn hfql-coalgebra
  "A coalgebra unfolding a pre-analyzed HFQL AST into a tree of steps: [value next-steps].
  Unfolding this coalgebra into the cofree comonad makes value and next-step
  computation fully lazy: values and next steps are only computed as the resulting cofree
  comonad is navigated, step by step.
  "
  [[projection value]]
  (->
    (case (projection-type projection)
      ::star      [(star value) nil]
      ::ident     [(getter value projection) nil]
      ::pull      (let [value (into {} (mapcat (fn [k]
                                                 (let [v (first (values (unfold hfql-coalgebra [k value])))]
                                                   (if (= '* k)
                                                     (update-keys v pull-key)
                                                     [[(pull-key k) v]])))
                                         projection))]
                    [value
                     (if (some #{'*} projection)
                       (map vector (keys value) (repeat value))
                       (map (fn [proj] [proj value]) projection))])
      ::traversal (let [[f sub] (first projection) ; TODO support maps with multiple entries
                        y       (f value)]
                    (cond
                      ;; *-traversal special case: {f {'* sub}}
                      (and (map? sub) (contains? sub '*))
                      ^::star [y (sequence (map (fn [e] [(get sub '*) e])) y)]
                      ;; regular case
                      (vector? sub) [y [[sub y]]]
                      (nil? sub)    [y nil]
                      :else         [y [[sub y]]]))
      ::ifn       [(get-or-call projection value) nil]
      (throw (ex-info "Unknown projection" {:type (type projection), :projection projection})))
    (vary-meta assoc
      ::key (pull-key projection)
      ::projection projection)))



(tests
  (values (unfold hfql-coalgebra [inc 1]))
  := '(2)

  (values (unfold hfql-coalgebra [:a {:a 1}]))
  := '(1)

  (values (unfold hfql-coalgebra [[:a] {:a 1, :b 2}]))
  := '({:a 1} 1)

  (values (unfold hfql-coalgebra [[:a '*] {:a 1, :b 2}]))
  := '({:a 1, :b 2} 1 2)

  (values (unfold hfql-coalgebra [{:a inc} {:a 1, :b 3}]))
  := '(1 2)

  (values (unfold hfql-coalgebra [[{:a inc}] {:a 1, :b 3}]))
  := '({:a 1} 1 2)

  (values (unfold hfql-coalgebra [{inc inc} 1]))
  := '(2 3)

  (values (unfold hfql-coalgebra [{inc {inc {inc inc}}} 1]))
  := '(2 3 4 5)

  (let [fork (fn [x] (list x x))]
    (values (unfold hfql-coalgebra [{identity {fork {fork fork}}} '.])))
   := '(              .
                    (. .)
                ((. .) (. .))
        (((. .) (. .)) ((. .) (. .)))) ; 8 elements


   (let [fork (fn [x] (list x x))]
     (values (unfold hfql-coalgebra [{identity {fork {'* {fork {'* fork}}}}} '.])))
    := '(. (. .) (. .) (. .) (. .) (. .) (. .) (. .)) ; 8 elements
    ;; * doesn't flatten, it fans-out, `values` is responsible for the apparent flattening. Values arbitrarily traverses in pre-order.

  )

(tests
  "calling fn in pull"
  (values (unfold hfql-coalgebra [[identity :a] {:a 1, :b 2}]))
  := [{identity {:a 1, :b 2}, :a 1} {:a 1, :b 2} 1]
  )

(tests
  "traversal in pull"
  (values (unfold hfql-coalgebra [['* {:a inc}] {:a 1, :b 2}]))
  := '({:a 1, :b 2} 1 2)
  )

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

(tests
  (extract (pull (unfold hfql-coalgebra [identity 1])))
  := 1

  (extract (pull (unfold hfql-coalgebra [:a {:a 1}])))
  := 1

  (extract (pull (unfold hfql-coalgebra [[:a] {:a 1, :b 2}])))
  := {:a 1}

  (extract (pull (unfold hfql-coalgebra [[:a :b] {:a 1}])))
  := {:a 1, :b nil}

  (extract (pull (unfold hfql-coalgebra [[:a :b] {:a 1, :b 2}])))
  := {:a 1, :b 2}

  (extract (pull (unfold hfql-coalgebra [[:a identity] {:a 1}])))
  := {:a 1, identity {:a 1}}

  (extract (pull (unfold hfql-coalgebra [[{:a inc}] {:a 1}])))
  := {:a 2}

  (extract (pull (unfold hfql-coalgebra [[{:a {inc inc}}] {:a 1}])))
  := {:a {inc 3}}

  (extract (pull (unfold hfql-coalgebra [{identity :a} {:a 1}])))
  := {identity 1}

  (extract (pull (unfold hfql-coalgebra [{identity [:a]} {:a 1}])))
  := {identity {:a 1}}

  (extract (pull (unfold hfql-coalgebra [range 3])))
  := '(0 1 2)

  (extract (pull (unfold hfql-coalgebra [{range {'* {inc inc}}} 3])))

  (extract (pull (unfold hfql-coalgebra [{ns-publics {'* val}} *ns*])))

  )



(defn hfqlR-coalgebra
  "A coalgebra unfolding a pre-analyzed HFQL AST into a tree of steps: [value next-steps].
  Unfolding this coalgebra into the cofree comonad makes value and next-step
  computation fully lazy: values and next steps are only computed as the resulting cofree
  comonad is navigated, step by step.
  "
  [projection]
  (->
    (case (projection-type projection)
      ;; ::star      [#(star %) (constantly ())]
      ::ident     [#(getter % projection) (constantly ())]
      ::ifn       [#(apply get-or-call projection %&) (constantly ())]
      #_#_::pull      (let [value (into {} (mapcat (fn [k]
                                                 (let [v (first (values (unfold hfql-coalgebra [k value])))]
                                                   (if (= '* k)
                                                     (update-keys v pull-key)
                                                     [[(pull-key k) v]])))
                                         projection))]
                    [value
                     (if (some #{'*} projection)
                       (map vector (keys value) (repeat value))
                       (map (fn [proj] [proj value]) projection))])
      #_#_::traversal (let [[f sub] (first projection) ; TODO support maps with multiple entries
                        y       (f value)]
                    (cond
                      ;; *-traversal special case: {f {'* sub}}
                      (and (map? sub) (contains? sub '*))
                      ^::star [y (sequence (map (fn [e] [(get sub '*) e])) y)]
                      ;; regular case
                      (vector? sub) [y [[sub y]]]
                      (nil? sub)    [y nil]
                      :else         [y [[sub y]]]))
      
      (throw (ex-info "Unknown projection" {:type (type projection), :projection projection})))
    (vary-meta assoc
      ::key (pull-key projection)
      ::projection projection)))

(defn unfold "Build a Cofree from a `coalgebra` and a `seed`.
  `coalgebra` must return a pair of a value `a` and a seqable container of next seeds.
  Maps are supported (children come from (vals fb)).

  unfold :: Functor f => (b    -> [a, f b   ]) -> b    -> Cofree f a
  unfold :: Functor f => (seed -> [a, f seed]) -> seed -> Cofree f a
"
  ([coalgebra] (partial unfold coalgebra))
  ([coalgebra seed]
   (let [[a fb :as step] (coalgebra seed)]
     (vary-meta
       (cr/->CofreeR a
         (map (unfold coalgebra)
           (cond
             (map? fb)   (vals fb) ; not sure about this one, we may need it for traversals?
             ;; (nil? fb)   nil
             :else       fb)))
       merge (meta step)))))



(comment
  (defn- prebind
    "Freeze a param subtree to a fixed env e (ignores the env it will be called with)."
    [WR e]
    (cr/->CofreeR
      (fn [_] (cr/extractR WR e))
      (fn [_] (map #(prebind % e) (cr/unwrapR WR e)))))

  (defn to-CofreeR
    "Lift the hfql-coalgebra `[spec env] -> [a, fb]` into a parametric CofreeR built
   from `spec` only. Root gets ::key = (pull-key spec) so folds can wrap it."
    [hfql-coalgebra spec]
    (letfn [(rec [spec]
              (let [analyzed (hfql-coalgebra [spec nil])]
                (let [valf   (fn [env] (first (hfql-coalgebra [spec env])))
                      edgesf (fn [env]
                               (let [[_ fb :as analyzed] (hfql-coalgebra [spec env])]
                                 (map (fn [[subspec subenv :as child]]
                                        ;; build param child; fix its local env to subenv
                                        (vary-meta (prebind (rec subspec) subenv)
                                          #(merge (meta child) %)))
                                   (or fb []))))]
                  (vary-meta (cr/->CofreeR valf edgesf)
                    #(merge (meta analyzed) %)))))]
      (rec spec)))

  (to-CofreeR hfql-coalgebra :a)
  (to-CofreeR hfql-coalgebra [:a])


  ;; [:a :b]
  (let [p (to-CofreeR hfql-coalgebra [:a])
        t (cr/inject p {:a 1 :b 2})]
    [(extract t)                         ; => {:a 1, :b 2}
     (map extract (unwrap t))])          ; => (1 2)

  (let [p (to-CofreeR hfql-coalgebra [:a])
        t (cr/inject p {:a 1, :b 2})]
    (coco/fold-postorder pull-algebra (duplicate t))
    )

  ;; {identity [:a :b]}
  (let [p (to-CofreeR hfql-coalgebra {identity [:a :b]})
        t (cr/inject p {:a 1 :b 2})
        proj (coco/fold-postorder pull-algebra (duplicate t)) #_(fold-keyed φ-pull t)]
    ;; (extract proj)
    proj)                                 ; => {identity {:a 1, :b 2}}

  ;; Traversal {range {'* inc}} with different environments
  (let [p (to-CofreeR hfql-coalgebra {range {'* inc}})]
    [(values (inject p 3))               ; => ((0 1 2) 1 2 3)
     (values (inject p 5))])             ; => ((0 1 2 3 4) 1 2 3 4 5)

  ;; Reseed a subtree: analyze once, vary env later
  (let [p  (to-CofreeR hfql-coalgebra inc)
        t1 (inject p 1)
        t2 (inject p 2)]
    [(extract t1) (extract t2)])         ; => [2 3]
                                        ; => (1 2)


  (comment

    (defn unfoldR "Build a Cofree from a `coalgebra` and a `seed`.
  `coalgebra` must return a pair of a value `a` and a seqable container of next seeds.
  Maps are supported (children come from (vals fb)).

  unfold :: Functor f => (b    -> [a, f b   ]) -> b    -> Cofree f a
  unfold :: Functor f => (seed -> [a, f seed]) -> seed -> Cofree f a
  "
      ([coalgebra] (partial unfoldR coalgebra))
      ([coalgebra seed]
       (let [[aF nextF :as step] (coalgebra seed)]
         (vary-meta
           (cr/->CofreeR aF
             (map (unfold coalgebra)
               (cond
                 (map? fb)   (vals fb) ; not sure about this one, we may need it for traversals?
                 ;; (nil? fb)   nil
                 :else       fb)))
           merge (meta step)))))

    (defn hfqlR-coalgebra
      "Like `hfql-coalgebra`, but unfolds to cofree coalgebra unfolding a pre-analyzed HFQL AST into a tree of steps: [value next-steps].
  Unfolding this coalgebra into the cofree comonad makes value and next-step
  computation fully lazy: values and next steps are only computed as the resulting cofree
  comonad is navigated, step by step.
  "
      [projection]
      (->
        (case (projection-type projection)
          ::star      [#(star %) (constantly ())]
          ::ident     [#(getter % projection) (constantly ())]
          #_#_::pull      (let [value (into {} (mapcat (fn [k]
                                                         (let [v (first (values (unfold hfql-coalgebra [k value])))]
                                                           (if (= '* k)
                                                             (update-keys v pull-key)
                                                             [[(pull-key k) v]])))
                                                 projection))]
                            [value
                             (if (some #{'*} projection)
                               (map vector (keys value) (repeat value))
                               (map (fn [proj] [proj value]) projection))])
          #_#_::traversal (let [[f sub] (first projection) ; TODO support maps with multiple entries
                                y       (f value)]
                            (cond
                              ;; *-traversal special case: {f {'* sub}}
                              (and (map? sub) (contains? sub '*))
                              ^::star [y (sequence (map (fn [e] [(get sub '*) e])) y)]
                              ;; regular case
                              (vector? sub) [y [[sub y]]]
                              (nil? sub)    [y nil]
                              :else         [y [[sub y]]]))
          ::ifn       [(fn val [& args] (apply get-or-call projection args)) (fn next [& _] nil)]
          (throw (ex-info "Unknown projection" {:type (type projection), :projection projection})))
        (vary-meta assoc
          ::key (pull-key projection)
          ::projection projection)))


    (tests
      
      (cr/extract (cr/inject 2 (cr/unfold hfqlR-coalgebra +)))
      ;; (((cr/unfold hfqlR-coalgebra +)))

      

      (str (constantly ()))
      
      )))
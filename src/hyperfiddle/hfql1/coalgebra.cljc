(ns hyperfiddle.hfql1.coalgebra
  (:refer-clojure :exclude [extend])
  (:require
   [clojure.core.protocols :as ccp]
   [clojure.datafy :as datafy]
   [hyperfiddle.hfql1.cofree-comonad :as coco :refer [unfold extract extend duplicate values synthesize]]
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

(defn hfql
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
                                                 (let [v (first (values (unfold hfql [k value])))]
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
  (values (unfold hfql [inc 1]))
  := '(2)

  (values (unfold hfql [:a {:a 1}]))
  := '(1)

  (values (unfold hfql [[:a] {:a 1, :b 2}]))
  := '({:a 1} 1)

  (values (unfold hfql [[:a '*] {:a 1, :b 2}]))
  := '({:a 1, :b 2} 1 2)

  (values (unfold hfql [{:a inc} {:a 1, :b 3}]))
  := '(1 2)

  (values (unfold hfql [[{:a inc}] {:a 1, :b 3}]))
  := '({:a 1} 1 2)

  (values (unfold hfql [{inc inc} 1]))
  := '(2 3)

  (values (unfold hfql [{inc {inc {inc inc}}} 1]))
  := '(2 3 4 5)

  (let [fork (fn [x] (list x x))]
    (values (unfold hfql [{identity {fork {fork fork}}} '.])))
   := '(              .
                    (. .)
                ((. .) (. .))
        (((. .) (. .)) ((. .) (. .)))) ; 8 elements


   (let [fork (fn [x] (list x x))]
     (values (unfold hfql [{identity {fork {'* {fork {'* fork}}}}} '.])))
    := '(. (. .) (. .) (. .) (. .) (. .) (. .) (. .)) ; 8 elements
    ;; * doesn't flatten, it fans-out, `values` is responsible for the apparent flattening. Values arbitrarily traverses in pre-order.

  )

(tests
  "calling fn in pull"
  (values (unfold hfql [[identity :a] {:a 1, :b 2}]))
  := [{identity {:a 1, :b 2}, :a 1} {:a 1, :b 2} 1]
  )

(tests
  "traversal in pull"
  (values (unfold hfql [['* {:a inc}] {:a 1, :b 2}]))
  := '({:a 1, :b 2} 1 2)
  )

(defn- φ-pull2 [W children]
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

(def pull (comp (synthesize φ-pull2) duplicate))

(tests
  (extract (pull (unfold hfql [identity 1])))
  := 1

  (extract (pull (unfold hfql [:a {:a 1}])))
  := 1

  (extract (pull (unfold hfql [[:a] {:a 1, :b 2}])))
  := {:a 1}

  (extract (pull (unfold hfql [[:a :b] {:a 1}])))
  := {:a 1, :b nil}

  (extract (pull (unfold hfql [[:a :b] {:a 1, :b 2}])))
  := {:a 1, :b 2}

  (extract (pull (unfold hfql [[:a identity] {:a 1}])))
  := {:a 1, identity {:a 1}}

  (extract (pull (unfold hfql [[{:a inc}] {:a 1}])))
  := {:a 2}

  (extract (pull (unfold hfql [[{:a {inc inc}}] {:a 1}])))
  := {:a {inc 3}}

  (extract (pull (unfold hfql [{identity :a} {:a 1}])))
  := {identity 1}

  (extract (pull (unfold hfql [{identity [:a]} {:a 1}])))
  := {identity {:a 1}}

  (extract (pull (unfold hfql [range 3])))
  := '(0 1 2)

  (extract (pull (unfold hfql [{range {'* {inc inc}}} 3])))

  (extract (pull (unfold hfql [{ns-publics {'* val}} *ns*])))

  )
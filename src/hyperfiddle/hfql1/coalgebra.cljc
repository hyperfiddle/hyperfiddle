(ns hyperfiddle.hfql1.coalgebra
  (:require
   [clojure.core.protocols :as ccp]
   [clojure.datafy :as datafy]
   [hyperfiddle.hfql1.cofree-comonad :as coco :refer [unfold values]]
   [hyperfiddle.rcf :refer [tests]]))

(defn safe-ccp-nav [object key value]
  (if (satisfies? ccp/Navigable object)
    (datafy/nav object key value)
    value))

(def ^:dynamic *navigate* safe-ccp-nav)

(defn- getter [object key] (*navigate* (force object) key (get (force object) key)))
(defn- caller [ifn & args] (apply ifn (map force args)))

(defn- touch [object] (if (seqable? object) (doall object) object))
(defn- star [object] (touch (force object)))

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
  (cond
    (= '* projection)       [(star value) nil]
    (ident? projection)     [(getter value projection) nil]
    (fn? projection)        [(caller projection value) nil]
    (vector? projection)    (let [value (into {} (mapcat (fn [k]
                                                           (let [v (first (values (unfold hfql [k value])))]
                                                             (if (= '* k)
                                                               (update-keys v pull-key)
                                                               [[(pull-key k) v]])))
                                                   projection))]
                              [value
                               (if (some #{'*} projection)
                                 (map vector (keys value) (repeat value))
                                 (map (fn [proj] [proj value]) projection))])
    (map? projection)       (let [[f sub] (first projection) ; TODO support maps with multiple entries
                                  y (f value)]
                              (cond
                                ;; *-traversal special case: {f {'* sub}}
                                (and (map? sub) (contains? sub '*))
                                [y (sequence (map (fn [e] [(get sub '*) e])) y)]
                                ;; regular case
                                (vector? sub) [y [[sub y]]]
                                (nil? sub)    [y nil]
                                :else         [y [[sub y]]]))))

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
  (values (unfold hfql [[identity :a] {:a 1, :b 2}])) ; FAIL FIXME
  := [{identity {:a 1, :b 2}, :a 1} {:a 1, :b 2} 1]
  )

(tests
  "traversal in pull"
  (values (unfold hfql [['* {:a inc}] {:a 1, :b 2}]))
  := '({:a 1, :b 2} 1 2)
  )

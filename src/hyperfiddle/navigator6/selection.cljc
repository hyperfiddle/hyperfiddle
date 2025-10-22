(ns hyperfiddle.navigator6.selection
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.hfql2 :as hfql]
            [hyperfiddle.navigator6.state :as state]
            [hyperfiddle.hfql2.analyzer :as ana]
            [geoffreygaillard.predicates :as pred]
            [geoffreygaillard.debug-tools :refer [debug-exceptions]]
            [hyperfiddle.navigator6.utils :as utils]
            [hyperfiddle.ct.cofree :refer [unwrap]]
            [dustingetz.loader :refer [Offload]]
            )
  #?(:cljs (:require-macros [hyperfiddle.navigator6.selection :refer [with-local-selection]])))

(e/declare *local-selection *parent-selection)

(defn- symbolic-coordinates [cellT] (when cellT ; glitch-guard
                                      (list (hfql/symbolic-edge cellT) (hfql/identify (hfql/value-at-point cellT)))))

;; (symbolic-cell-coordinates hyperfiddle.navigator6.matrix/_cellT)
;; (symbolic-row-coordinates hyperfiddle.navigator6.matrix/_cellT)

(e/defn WithLocalSelection [state-key resolve-fn Body]
  (e/client
    (binding [*local-selection (e/server (let [selection-state (get state/*local-state state-key)
                                               resolve-fn ((e/capture-fn) resolve-fn)]
                                           (Offload #(resolve-fn selection-state) {:label 'resolve-selected-object})))]
      (->> (Body)
        (utils/InterceptCommand `SetSelection!
          (e/fn [[_ Getter]]
            [`state/ReplaceState! assoc state-key (e/server (symbolic-coordinates (Getter)))]))))))

(defmacro with-local-selection [state-key resolve-fn & body]
  `(WithLocalSelection ~state-key ~resolve-fn (e/fn [] ~@body)))



(e/defn WithLocalSelectedIndex [selection-op Body]
  (e/client
    (let [!selected-index (e/server (atom nil)) ; siting on server ensures row turns blue/white synchronously with data change on index-ring tape loop
          selected-index (e/server (e/watch !selected-index))]
      (->> (Body selected-index !selected-index)
        (utils/InterceptCommand selection-op (e/fn [[_ index :as command]] (case (e/server (reset! !selected-index index)) command)))))))



(defn select-row-if-match-url! [!index row-index match? #_#_row-object selected-object]
  {:pre [(pred/atom-like? !index) (number? row-index)]
   :post [(nil? %)]}
  (if match? #_(= row-object selected-object) ; both can be nil
      (reset! !index row-index) ; current row matches. Set selection.
      (compare-and-set! !index row-index nil)) ; current row used to match and now doesn't. Clear selection.
  nil)

(defn select-pull-step [stepT]
  (case (::ana/type (hfql/metas stepT))
    ::ana/pull stepT
    ::ana/traversal (some-> (unwrap stepT) first select-pull-step)
    ;; (some-> (unwrap stepT) first select-pull-step)
    (hfql/wrap-in-pull stepT)
    #_(throw (ex-info "No pull child" {:type (::ana/type (hfql/metas stepT))}))))

(defn resolve-selected-object-BIG-O-N [[column row] bindings childrenT]
  #?(:clj
     (debug-exceptions `resolve-selected-object
       (with-bindings bindings
         ;; (prn "resolve-selected-object-BIG-O-N" column row)
         [column
          (when (> 10000 (bounded-count 10000 (unwrap childrenT))) ; FIXME HACK don't scan large collections. We must find a way to support selection over arbitrary large collections – probably by enforcing selected objects to be globally resolvable.
            (when (and row (some? childrenT))
              (or
                (when (and (some? row) (= row (hfql/identify (hfql/value-at-point childrenT))))
                  childrenT)
                (if (= ::ana/many (::ana/multiplicity (hfql/metas childrenT)))
                  (hfql/find-child-by #(= row (hfql/identify (hfql/value-at-point %))) childrenT)
                  (hfql/find-child column (hfql/augment-with-suggestions! (select-pull-step childrenT)))))))]))))

(defn get-rows-container [stepT]
  (case (::ana/type (hfql/metas stepT))
    ::ana/traversal (if (= ::ana/many (::ana/multiplicity (hfql/metas stepT)))
                      stepT
                      (get-rows-container (first (unwrap stepT))))
    stepT))


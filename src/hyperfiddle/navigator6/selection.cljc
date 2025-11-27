(ns hyperfiddle.navigator6.selection
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.hfql2 :as hfql]
            [hyperfiddle.hfql2.protocols :refer [hfql-resolve]]
            [hyperfiddle.navigator6.state :as state]
            [hyperfiddle.hfql2.analyzer :as ana]
            [geoffreygaillard.debug-tools :refer [debug-exceptions]]
            [hyperfiddle.navigator6.utils :as utils]
            [hyperfiddle.ct.cofree :refer [unwrap]]
            [dustingetz.loader :refer [Offload]]
            )
  #?(:cljs (:require-macros [hyperfiddle.navigator6.selection :refer [with-local-selection]])))

(e/declare *local-selection *parent-selection)

(defn- symbolic-coordinates [cellT]
  (when cellT ; glitch-guard
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



(defn select-pull-step [stepT]
  (case (::ana/type (hfql/metas stepT))
    ::ana/pull stepT
    ::ana/traversal (some-> (unwrap stepT) first select-pull-step)
    ;; (some-> (unwrap stepT) first select-pull-step)
    (hfql/wrap-in-pull stepT)
    #_(throw (ex-info "No pull child" {:type (::ana/type (hfql/metas stepT))}))))

(defn resolve-selected-object-BIG-O-1 [[column row] bindings childrenT]
  (when childrenT
    (when row
      (let [val-at-point (hfql/value-at-point childrenT)]
        (or (and (= row (hfql/identify val-at-point)) ; direct identity equality case
              childrenT)
          (and (map? val-at-point) ; map lookup case
            (not (= ::ana/many (::ana/multiplicity (hfql/metas childrenT))))
            (-> (case column
                  key (hfql/hfql key (find val-at-point row))
                  (hfql/hfql val (find val-at-point row)) ; mapentry is not interesting on its own, shortcut to val
                  )
              (hfql/set-parent childrenT)))
          (when-let [resolved (hfql-resolve row)]
            (when (not= resolved row)
              (when (= ::ana/many (::ana/multiplicity (hfql/metas childrenT)))
                (hfql/update-scope (first (unwrap childrenT)) assoc '% resolved)))
            ))))))

(defn resolve-selected-object-BIG-O-N [[column row] bindings childrenT]
  #?(:clj
     (debug-exceptions `resolve-selected-object
       (with-bindings bindings
         [column
          (when childrenT
            (when row
              (or
                (resolve-selected-object-BIG-O-1 [column row] bindings childrenT)
                (do
                  (clojure.tools.logging/warn "O(n) scan")
                  (if (= ::ana/many (::ana/multiplicity (hfql/metas childrenT)))
                    (hfql/find-child-by #(= row (hfql/identify (hfql/value-at-point %))) childrenT)
                    (hfql/find-child column (hfql/augment-with-suggestions! (select-pull-step childrenT))))))))]))))

(defn get-rows-container [stepT]
  (case (::ana/type (hfql/metas stepT))
    ::ana/traversal (if (= ::ana/many (::ana/multiplicity (hfql/metas stepT)))
                      stepT
                      (get-rows-container (first (unwrap stepT))))
    stepT))

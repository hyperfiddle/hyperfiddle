(ns hyperfiddle.hfql2.ast
  "HFQL AST node predicates operating on annotated AST nodes (maps with ::ana/ keys)."
  (:require [clojure.spec.alpha :as s]
            [hyperfiddle.hfql2.analyzer :as-alias ana]))

;; ─── Spec ────────────────────────────────────────────────────────────────────

(s/def ::type #{::symbol ::keyword ::string
                ::literal
                ::star ::recur
                ::var ::quoted-var
                ::call
                ::pull
                ::traversal ::traversals
                ::fn ::lambda})

;; ─── AST node predicates ────────────────────────────────────────────────────
;; Operate on annotated AST nodes (maps produced by the analyzer's φ algebra).

(def star? (comp #(= ::ana/star %) ::ana/type))
(def star-traversal? (comp #(= ::ana/star %) ::ana/kind))
(def multiplicity-many? (comp #(= ::ana/many %) ::ana/multiplicity))

(defn projection-type
  "Classify an annotated AST node. Refines ::ana/symbol to ::ana/fn when the
  node's value is a function."
  [point]
  (case (::ana/type point)
    ::ana/symbol (if (fn? (::ana/value point))
                   ::ana/fn
                   ::ana/symbol)
    (::ana/type point)))
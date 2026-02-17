(ns hyperfiddle.hfql2.ast
  "HFQL AST definition, predicates, and manipulation.

  This namespace provides:
  - Form-level helpers for quoting/unquoting and classification,
  - AST node predicates operating on annotated AST nodes (maps with ::ana/ keys)."
  (:require [clojure.spec.alpha :as s]
            [contrib.data :as data]
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

;; ─── Form-level helpers ──────────────────────────────────────────────────────

(defn quoted? [form]
  (and (seq? form) (= 'quote (first form))))

(defn unquote* [form]
  (if (and (seq? form) (= 'quote (first form)))
    (second form)
    form))

(defn literal? [obj]
  (or (nil? obj)
    (number? obj)
    (string? obj)
    (keyword? obj)))

(def realize
  "Deprecated: use contrib.data/realize instead."
  data/realize)

(defn quote
  "Quote a form. Invoke it as `(#'quote x)` to avoid conflict with the 'quote special form."
  [form]
  (if (or (literal? form) (quoted? form))
    form
    (list 'quote (realize form))))

(defn quote-nat-ints [form] (if (nat-int? form) (list 'quote form) form))

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
(ns hyperfiddle.hfql2.ast
  "HFQL AST node API: specs, constructors, accessors, predicates, and mutators.
  All AST node keys and type values live in the ::ast/ (this) namespace."
  (:require [clojure.spec.alpha :as s]))

;; ═══════════════════════════════════════════════════════════════════════════════
;; Specs — keys
;; ═══════════════════════════════════════════════════════════════════════════════

(s/def ::type #{::star ::recur ::literal ::keyword ::string ::symbol
                ::var ::quoted-var ::pull ::fn ::lambda ::call ::traversal})
(s/def ::form any?)
(s/def ::lexical-env map?)
(s/def ::partial boolean?)
(s/def ::id int?)
(s/def ::symbolic-edge any?)
(s/def ::symbolic-edges (s/coll-of any?))
(s/def ::form-resolved any?)
(s/def ::form-short any?)
(s/def ::value any?)
(s/def ::var any?)
(s/def ::parameters any?)
(s/def ::arguments any?)
(s/def ::has-star? boolean?)
(s/def ::recursion-point boolean?)
(s/def ::recursion-limit any?)
(s/def ::multiplicity #{::many})
(s/def ::kind #{::star ::keyword-fn})
(s/def ::edge map?)
(s/def ::leaf boolean?)
;; Interpreter-set keys (not part of static node spec)
(s/def ::recursion-root boolean?)
(s/def ::recursive-child boolean?)

;; ═══════════════════════════════════════════════════════════════════════════════
;; Specs — node multi-spec
;; ═══════════════════════════════════════════════════════════════════════════════

(defmulti node-spec ::type)

(defmethod node-spec ::star [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id]))

(defmethod node-spec ::recur [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id ::recursion-limit]))

(defmethod node-spec ::literal [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id ::symbolic-edge]))

(defmethod node-spec ::keyword [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id
                ::symbolic-edge ::form-resolved ::form-short]))

(defmethod node-spec ::string [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id ::symbolic-edge]))

(defmethod node-spec ::symbol [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id
                ::symbolic-edge ::value ::var ::form-resolved ::form-short]))

(defmethod node-spec ::var [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id
                ::symbolic-edge ::value ::parameters ::form-resolved ::form-short]))

(defmethod node-spec ::quoted-var [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id
                ::symbolic-edge ::form-resolved ::form-short]))

(defmethod node-spec ::pull [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id
                ::symbolic-edges ::has-star?]
          :opt [::recursion-point ::recursion-limit]))

(defmethod node-spec ::fn [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id
                ::symbolic-edge ::value]))

(defmethod node-spec ::lambda [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id
                ::symbolic-edge ::parameters ::value]
          :opt [::kind ::form-resolved ::form-short]))

(defmethod node-spec ::call [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id
                ::symbolic-edge ::value ::var ::parameters ::arguments ::form-resolved]
          :opt [::lexical-env]))

(defmethod node-spec ::traversal [_]
  (s/keys :req [::type ::form ::lexical-env ::partial ::id]
          :opt [::kind ::multiplicity ::edge ::recursion-point ::recursion-limit ::leaf]))

(s/def ::node (s/multi-spec node-spec ::type))

;; ═══════════════════════════════════════════════════════════════════════════════
;; Internal helpers
;; ═══════════════════════════════════════════════════════════════════════════════

(defn compute-node-id [node]
  (hash (select-keys node #{::type ::form ::symbolic-edge ::symbolic-edges ::form-short ::form-resolved})))

(defn- finalize [node]
  (let [node (if (contains? node ::partial) node (assoc node ::partial true))]
    (s/assert ::node (assoc node ::id (compute-node-id node)))))

;; ═══════════════════════════════════════════════════════════════════════════════
;; Constructors
;; ═══════════════════════════════════════════════════════════════════════════════
;; Each constructor takes `form-meta` (metadata from the original form, may be
;; nil) as first argument, followed by required fields. The form-meta is merged
;; first, then AST keys on top — matching how φ works.

(defn ->star [form-meta form lexical-env]
  (finalize (merge form-meta {::type ::star, ::form form, ::lexical-env lexical-env})))

(defn ->recur [form-meta form lexical-env recursion-limit]
  (finalize (merge form-meta {::type ::recur, ::form form, ::lexical-env lexical-env
                              ::recursion-limit recursion-limit})))

(defn ->literal [form-meta form lexical-env symbolic-edge]
  (finalize (merge form-meta {::type ::literal, ::form form, ::lexical-env lexical-env
                              ::symbolic-edge symbolic-edge})))

(defn ->keyword [form-meta form lexical-env symbolic-edge form-resolved form-short]
  (finalize (merge form-meta {::type ::keyword, ::form form, ::lexical-env lexical-env
                              ::symbolic-edge symbolic-edge, ::form-resolved form-resolved
                              ::form-short form-short})))

(defn ->string [form-meta form lexical-env symbolic-edge]
  (finalize (merge form-meta {::type ::string, ::form form, ::lexical-env lexical-env
                              ::symbolic-edge symbolic-edge})))

(defn ->symbol [form-meta form lexical-env symbolic-edge value var form-resolved form-short]
  (finalize (merge form-meta {::type ::symbol, ::form form, ::lexical-env lexical-env
                              ::symbolic-edge symbolic-edge, ::value value, ::var var
                              ::form-resolved form-resolved, ::form-short form-short})))

(defn ->var [form-meta form lexical-env symbolic-edge value parameters form-resolved form-short]
  (finalize (merge form-meta {::type ::var, ::form form, ::lexical-env lexical-env
                              ::symbolic-edge symbolic-edge, ::value value, ::parameters parameters
                              ::form-resolved form-resolved, ::form-short form-short})))

(defn ->quoted-var [form-meta form lexical-env symbolic-edge form-resolved form-short]
  (finalize (merge form-meta {::type ::quoted-var, ::form form, ::lexical-env lexical-env
                              ::symbolic-edge symbolic-edge, ::form-resolved form-resolved
                              ::form-short form-short})))

(defn ->pull [form-meta form lexical-env symbolic-edges has-star?
              & {:keys [recursion-point recursion-limit]}]
  (finalize (cond-> (merge form-meta {::type ::pull, ::form form, ::lexical-env lexical-env
                                      ::symbolic-edges symbolic-edges, ::has-star? has-star?})
              recursion-point (assoc ::recursion-point true, ::recursion-limit recursion-limit))))

(defn ->fn [form-meta form lexical-env symbolic-edge value]
  (finalize (merge form-meta {::type ::fn, ::form form, ::lexical-env lexical-env
                              ::symbolic-edge symbolic-edge, ::value value})))

(defn ->lambda [form-meta form lexical-env symbolic-edge parameters value
                & {:keys [kind form-resolved form-short]}]
  (finalize (cond-> (merge form-meta {::type ::lambda, ::form form, ::lexical-env lexical-env
                                      ::symbolic-edge symbolic-edge, ::parameters parameters
                                      ::value value})
              kind          (assoc ::kind kind)
              form-resolved (assoc ::form-resolved form-resolved)
              form-short    (assoc ::form-short form-short))))

(defn ->call [form-meta form lexical-env symbolic-edge value var parameters arguments
              partial form-resolved]
  (finalize (merge form-meta {::type ::call, ::form form, ::lexical-env lexical-env
                              ::symbolic-edge symbolic-edge, ::value value, ::var var
                              ::parameters parameters, ::arguments arguments, ::partial partial
                              ::form-resolved form-resolved})))

(defn ->traversal [form-meta form lexical-env partial
                   & {:keys [kind multiplicity edge recursion-point recursion-limit]}]
  (finalize (cond-> (merge form-meta {::type ::traversal, ::form form
                                      ::lexical-env lexical-env, ::partial partial})
              kind             (assoc ::kind kind)
              multiplicity     (assoc ::multiplicity multiplicity)
              edge             (assoc ::edge edge)
              recursion-point  (assoc ::recursion-point true
                                      ::recursion-limit recursion-limit))))

;; ═══════════════════════════════════════════════════════════════════════════════
;; Accessors
;; ═══════════════════════════════════════════════════════════════════════════════

(defn node-type       [node] (::type node))
(defn form            [node] (::form node))
(defn form-short      [node] (::form-short node))
(defn form-resolved   [node] (::form-resolved node))
(defn symbolic-edge   [node] (::symbolic-edge node))
(defn symbolic-edges  [node] (::symbolic-edges node))
(defn node-value      [node] (::value node))
(defn node-var        [node] (::var node))
(defn parameters      [node] (::parameters node))
(defn arguments       [node] (::arguments node))
(defn lexical-env     [node] (::lexical-env node))
(defn node-id         [node] (::id node))
(defn has-star?       [node] (::has-star? node))
(defn edge            [node] (::edge node))
(defn kind            [node] (::kind node))
(defn multiplicity    [node] (::multiplicity node))
(defn recursion-limit [node] (::recursion-limit node))

;; ═══════════════════════════════════════════════════════════════════════════════
;; Type predicates
;; ═══════════════════════════════════════════════════════════════════════════════

(defn star?       [node] (= ::star (node-type node)))
(defn recur-node? [node] (= ::recur (node-type node)))
(defn literal?    [node] (= ::literal (node-type node)))
(defn kw?         [node] (= ::keyword (node-type node)))
(defn str-node?   [node] (= ::string (node-type node)))
(defn sym?        [node] (= ::symbol (node-type node)))
(defn var-node?   [node] (= ::var (node-type node)))
(defn quoted-var? [node] (= ::quoted-var (node-type node)))
(defn pull?       [node] (= ::pull (node-type node)))
(defn fn-node?    [node] (= ::fn (node-type node)))
(defn lambda?     [node] (= ::lambda (node-type node)))
(defn call?       [node] (= ::call (node-type node)))
(defn traversal?  [node] (= ::traversal (node-type node)))

;; ═══════════════════════════════════════════════════════════════════════════════
;; Semantic predicates
;; ═══════════════════════════════════════════════════════════════════════════════

(defn star-traversal?    [node] (= ::star (kind node)))
(defn multiplicity-many? [node] (= ::many (multiplicity node)))
(defn recursion-point?   [node] (boolean (::recursion-point node)))
(defn partial?           [node] (boolean (::partial node)))
(defn leaf?              [node] (not (false? (::leaf node))))

(defn projection-type
  "Classify an AST node. Refines ::symbol to ::fn when the node value is a function."
  [point]
  (case (node-type point)
    ::symbol (if (fn? (node-value point)) ::fn ::symbol)
    (node-type point)))

;; ═══════════════════════════════════════════════════════════════════════════════
;; Mutators (for interpreter-set keys)
;; ═══════════════════════════════════════════════════════════════════════════════

(defn mark-recursion-root  [node] (assoc node ::recursion-root true))
(defn recursion-root?      [node] (boolean (::recursion-root node)))
(defn mark-recursive-child [node] (assoc node ::recursive-child true))
(defn recursive-child?     [node] (boolean (::recursive-child node)))
(defn dec-recursion-limit  [node] (update node ::recursion-limit dec))
(defn set-leaf             [node v] (assoc node ::leaf v))
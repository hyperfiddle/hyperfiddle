(ns hyperfiddle.hfql2.analyzer
  (:require [clojure.core :as cc]
            [contrib.data :refer [realize update-existing]]
            [contrib.walk :refer [prewalk postwalk]]
            [hyperfiddle.hfql2.analyzer.lambda :refer [lambda?]]
            [hyperfiddle.hfql2.ast :as ast]
            [hyperfiddle.rcf :refer [tests]]
            [hyperfiddle.ct.functor :refer [fmap]]
            [hyperfiddle.ct.pair :refer [pair pair? fst snd]]
            [hyperfiddle.ct.comonad :refer [extract]]
            [hyperfiddle.ct.cofree :refer [unwrap]]
            [hyperfiddle.ct.rose-tree :refer [rose-tree coiter]]
            [clojure.string :as str])
  #?(:cljs (:require-macros [hyperfiddle.hfql2.analyzer :refer [hfql resolve-direct-method]])))

(defn- var-form? [form]
  (and (seq? form)
    (= 'var (first form))
    (= 2 (count form))))

(defn- symbolic-call? [form]
  (and (seq? form)
    (let [[f & _args] form]
      (and (symbol? f) (not (#{'fn `fn 'fn* 'quote 'var 'hfql `hfql} f))))))

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

(defn quote-form
  "Quote a form. Named quote-form to avoid conflict with the 'quote special form."
  [form]
  (if (or (literal? form) (quoted? form))
    form
    (list 'quote (realize form))))

(defn quote-nat-ints [form] (if (nat-int? form) (list 'quote form) form))

(defn- quoted-var-form? [form] (and (quoted? form) (var-form? (unquote* form))))

;;;;;;;;;;;;;;;;;;;;;
;; KW special form ;;
;;;;;;;;;;;;;;;;;;;;;

(defn- generated-symbol? [sym]
  {:pre [(symbol? sym)]}
  (and (simple-symbol? sym)
    (str/ends-with? (name sym) "#")))

(defn- expanded-fn-form? [form]
  (and (seq? form)
    (= 'fn* (first form))
    (vector? (second form))))

(defn- unary-fn-form? [form]
  (and (expanded-fn-form? form)
    (= 1 (count (second form))) ; 1 arg
    ))

(defn- is-structural? [form]
  (and (seq? form)
    (#{'do} (first form))))

(defn- last-form [forms]
  (let [last-form (last forms)]
    (if (is-structural? last-form)
      (last-form last-form)
      last-form)))

(defn- is-kw-fn? [form]
  (when (and (expanded-fn-form? form)
          (unary-fn-form? form))
    (let [[_fn* [arg-sym] & body] form
          last-form (last-form body)]
      (and (generated-symbol? arg-sym)
        (seq? last-form)
        (keyword? (first last-form))
        (= arg-sym (second last-form))))))

(defn- get-kw-fn-form-kw [kw-fn-form]
  (when (is-kw-fn? kw-fn-form)
    (let [[_fn* _args & body] kw-fn-form]
      (first (last-form body)))))



(defn projection-type [form]
  (cond
    (= '* form)           ::ast/star
    (or (= '... form)
      (nat-int? form))    ::ast/recur
    (symbol? form)        ::ast/symbol
    (keyword? form)       ::ast/keyword
    (string? form)        ::ast/string
    (fn? form)            ::ast/fn
    (var-form? form)      ::ast/var
    (symbolic-call? form) ::ast/call
    (pair? form)          ::ast/traversal
    (record? form)        ::ast/literal
    (map? form)           ::ast/traversals
    (vector? form)        ::ast/pull
    (quoted-var-form? form) ::ast/quoted-var
    (quoted? form)    ::ast/literal
    (lambda? form)        ::ast/lambda
    ;; TODO ifn? – maybe
    ;; TODO? method sym
    :else
    ::ast/literal))

(defn- reshape-traversals-down [hfql]
  (case (projection-type hfql)
    ::ast/call (reduced (reduced hfql))
    ::ast/lambda (reduced (reduced hfql))
    ::ast/literal (reduced (reduced hfql))
    ::ast/quoted-var (reduced (reduced hfql))
    (::ast/pull ::ast/traversals ::ast/traversal ::ast/star ::ast/recur ::ast/symbol ::ast/keyword ::ast/string ::ast/fn ::ast/var)
    hfql))

(defn- reshape-traversals-up [hfql]
  (if (reduced? hfql)
    (unreduced hfql)
    (case (projection-type hfql)
      ::ast/pull (into [] (mapcat (fn [hfql] (if (and (vector? hfql) (not (pair? hfql))) hfql [hfql])) hfql))
      ::ast/traversals (if (= 1 (count hfql))
                     (if-let [metas (meta hfql)]
                       (vary-meta (apply pair (first hfql)) merge metas)
                       (first hfql))
                     (with-meta (into [] hfql) (meta hfql)))
      (::ast/call ::ast/lambda ::ast/literal ::ast/quoted-var) (throw (ex-info "Encountered invalid AST node." {:value hfql}))
      (::ast/traversal ::ast/star ::ast/recur ::ast/symbol ::ast/keyword ::ast/string ::ast/fn ::ast/var)
      hfql)))

(defn- lexical? [env sym] (contains? env sym))

(defn- indirect-method-call? [sym]
  (and (simple-symbol? sym)
    (str/starts-with? (name sym) ".")))

(defn- direct-method-call? [sym]
  (and (qualified-symbol? sym)
    (str/starts-with? (name sym) ".")))

(defn- static-method-call? [sym]
  (and (qualified-symbol? sym)
    (not (str/starts-with? (name sym) "."))))

(defn- supports-direct-method-syntax? []
  #?(:clj (>= 1.12 (parse-double (re-find #"^[0-9]+\.[0-9]+" (clojure-version))))
     :cljs false ; TODO I actually don't know. Maybe cljs does support it.
     ))

(defn- escape-method-call [form]
  (cond
    (indirect-method-call? form) `#(~form %)

    (and (direct-method-call? form) (not (supports-direct-method-syntax?)))
    , (escape-method-call (symbol (name form)))

    :else form))

(defn- var-resolver [env form]
  (cond
    (lexical? env form)          nil
    (indirect-method-call? form) nil
    (direct-method-call? form)   nil
    (static-method-call? form)   nil
    (symbol? form)               `(var ~form)
    :else                        nil))

(defmacro resolve-direct-method [sym] ; No call to resolve, respects ②
  {:pre [(direct-method-call? sym)]}
  `(symbol (.getName ~(symbol (namespace sym))) ~(name sym)))

(defn- resolver [env form]
  (cond
    (lexical? env form)          form
    (indirect-method-call? form) nil
    (direct-method-call? form)   `(resolve-direct-method ~form)
    (static-method-call? form)   form
    (symbol? form)               `(symbol (var ~form))
    :else                        nil))

(defn- lexical-bindings-map [lexical-env]
  (zipmap (map #(quote-form %) (keys lexical-env))
    (keys lexical-env)))

#?(:clj
   (defn aliased-form
     ([ident] (aliased-form (ns-name *ns*) ident))
     ([ns-sym ident]
      (if-not (qualified-ident? ident)
        ident
        (if (= (namespace ident) (name ns-sym))
          (if (keyword? ident)
            (str "::" (name ident))
            (symbol (name ident)))
          (let [ns (find-ns ns-sym)
                aliases (ns-aliases ns)
                ns->alias (zipmap (vals aliases) (keys aliases)) ; hot spot
                ns-str->alias-str (-> ns->alias
                                    (update-keys (comp str ns-name))
                                    (update-vals str))
                alias-str (get ns-str->alias-str (namespace ident))
                aliased? (some? alias-str)
                alias (or alias-str (namespace ident))]
            (if (keyword? ident)
              (if aliased?
                (str "::" alias "/" (name ident))
                ident)
              (symbol alias (name ident)))))))))

(tests
  (aliased-form (ns-name *ns*) ::test) := "::test"
  (aliased-form (ns-name *ns*) :hyperfiddle.hfql2.analyzer/test) := "::test"
  (aliased-form (ns-name *ns*) :clojure.core/test) := "::cc/test"
  (aliased-form (ns-name *ns*) ::cc/test) := "::cc/test"
  (aliased-form (ns-name *ns*) `asdf) := 'asdf
  (aliased-form (ns-name *ns*) 'hyperfiddle.hfql2.analyzer/asdf) := 'asdf
  )

(defn- φ ; phi
  "An algebra collecting static knowledge about an HFQL expression."
  ([env] (partial φ env))
  ([env form]
   (as-> (meta form) %
     (merge
       %
       {::ast/type        (projection-type form)
        ::ast/form        (quote-form form)
        ::ast/lexical-env (lexical-bindings-map env)
        ::ast/partial true ; a partial point depends on %. % is late-bound.
        }
       (case (projection-type form)
         ::ast/star      {} ; '* and recur have no edge, and no value on purpose. They are purely syntactic.
         ::ast/recur     {::ast/recursion-limit (if (nat-int? form) form ##Inf)}
         ::ast/literal   {::ast/symbolic-edge form}
         ::ast/keyword   {::ast/symbolic-edge form
                      ::ast/form-resolved form ; keyword auto-resolve at read-time.
                      ::ast/form-short #?(:clj (aliased-form form), :cljs form)}
         ::ast/string    {::ast/symbolic-edge form}
         ::ast/symbol    {::ast/symbolic-edge form ; potentially aliased
                      ::ast/resolved-runtime-value (escape-method-call form), ; macroexpand will auto-resolve
                      ::ast/var (var-resolver env form) ; nil if symbol is lexicaly bound
                      ::ast/form-resolved (resolver env form) ; resolve via eventual ns alias
                      ::ast/form-short #?(:clj (aliased-form form), :cljs form)}
         ::ast/var       (let [sym (second form)]
                      {::ast/symbolic-edge form ; the var object itself
                       ::ast/resolved-runtime-value form ; also the var object itself
                       ::ast/parameters '(%)
                       ::ast/form-resolved (resolver env sym)
                       ::ast/form-short #?(:clj (aliased-form sym), :cljs sym)})
         ::ast/quoted-var (let [sym (second (unquote* form))]
                       {::ast/form (unquote* form)
                        ::ast/symbolic-edge form
                        ::ast/form-resolved (resolver env sym)
                        ::ast/form-short #?(:clj (aliased-form sym), :cljs sym)})
         ::ast/pull      (let [recursion-point (some-fn #{'...} nat-int?)
                           form' (vec (remove recursion-point form))]
                       (merge
                         {::ast/form form'
                          ::ast/symbolic-edges form'
                          ::ast/pull-has-star? (boolean (some #(= ::ast/star (::ast/type %)) (map (φ env) form)))}
                         (when-some [recursion (first (filter recursion-point form))]
                           {::ast/recursion-point true
                            ::ast/recursion-limit (::ast/recursion-limit (φ env recursion))})))
         ::ast/fn        {::ast/symbolic-edge form ; e.g. '#function[clojure.core/identity] – useful for tests
                      ::ast/resolved-runtime-value form} ; already resolved
         ::ast/lambda    (let [base {::ast/symbolic-edge (let [[_fn ?name ?parameters] form]
                                                   (if (symbol? ?name)
                                                     `(~'fn ~?name ~'...)
                                                     '(fn anonymous ...)))
                                 ::ast/parameters '(%)
                                 ::ast/partial true
                                 ::ast/resolved-runtime-value form}]
                       (if-let [kw (get-kw-fn-form-kw form)]
                         (merge base (select-keys (φ env kw) #{::ast/symbolic-edge ::ast/form-resolved ::ast/form-short}) {::ast/kind ::ast/keyword-fn})
                         base))
         ::ast/call      (let [[f & params] form] ; symbolic – quoted by defmacro
                       {::ast/symbolic-edge form ; symbolic – interpreter will template later
                        ::ast/resolved-runtime-value f ; macroexpand will auto-resolve, from lexical, dynamic or global scope.
                        ::ast/var (var-resolver env f)
                        ::ast/parameters params
                        ::ast/arguments (replace {'% ''%} params)
                        ::ast/partial (contains? (set params) '%)
                        ::ast/form-resolved (resolver env f)
                        ::ast/lexical-env (let [params (remove #{'%} params)]
                                        (merge (lexical-bindings-map env)
                                          (zipmap (map quote-form (filter symbol? params))
                                            (filter symbol? params))))})
         ::ast/traversal (let [edge (φ env (fst form))
                           base (case (::ast/type edge)
                                  ::ast/star {::ast/kind ::ast/star, ::ast/multiplicity ::ast/many}
                                  {::ast/edge (cond-> edge (not= ::ast/symbol (::ast/type edge)) (assoc ::ast/leaf false))
                                   ::ast/lexical-env (::ast/lexical-env edge)})
                           continuation (φ env (snd form))]
                       (merge base
                         {::ast/partial (and (::ast/partial edge) (::ast/partial continuation))}
                         (when (= ::ast/star (::ast/kind continuation)) {::ast/multiplicity ::ast/many})
                         (when (= ::ast/recur (::ast/type continuation))
                           {::ast/recursion-point true
                            ::ast/recursion-limit (::ast/recursion-limit continuation)})
                         ))))
     (assoc % ::ast/id (ast/compute-node-id %)))))

(defmulti -escape ::ast/type)
(defmethod -escape ::ast/star [point] point)
(defmethod -escape ::ast/recur [point] point)
(defmethod -escape ::ast/literal [point] point)
(defmethod -escape ::ast/keyword [point] (update point ::ast/form-short quote-form))
(defmethod -escape ::ast/string [point] point)
(defmethod -escape ::ast/symbol [point] (-> (update point ::ast/symbolic-edge quote-form)
                                      (update ::ast/form-short quote-form)))
(defmethod -escape ::ast/var [point] (-> (update point ::ast/parameters quote-form)
                                    (update ::ast/form-short quote-form)))
(defmethod -escape ::ast/quoted-var [point] (-> (update point ::ast/form unquote*) ; special case
                                           (update ::ast/form-short quote-form)))
(defmethod -escape ::ast/pull [point] (-> (update point ::ast/form quote-form)
                                    (update ::ast/symbolic-edges quote-form)))
(defmethod -escape ::ast/fn [point] (update point ::ast/symbolic-edge quote-form))
(defmethod -escape ::ast/lambda [point] (-> (update point ::ast/symbolic-edge quote-form)
                                      (update ::ast/parameters quote-form)))

(defmethod -escape ::ast/call [point] (-> (update point ::ast/symbolic-edge quote-form)
                                    (update ::ast/parameters quote-form)
                                    (update ::ast/arguments conj 'list)))

(defmethod -escape ::ast/traversal [point] (update-existing point ::ast/edge -escape))

(defn escape [point]
  (-escape (update point ::ast/form quote-form)))


(defn- ψ ; psi
  "A coalgebra describe how to unfold an HFQL expression into a tree. For any HFQL form, return its children."
  [hfql]
  (case (projection-type hfql)
    ::ast/pull      hfql
    ::ast/traversal [(snd hfql)]
    (::ast/star ::ast/keyword ::ast/symbol ::ast/var ::ast/string ::ast/fn ::ast/call ::ast/lambda ::ast/literal ::ast/quoted-var)
    ()
    ::ast/recur ; we don't produce an infinite AST to ease debugging.
    ()))

(defn- unfold [φ ψ seed] (fmap φ (coiter ψ seed)))

(defn analyze
  "Unfold an HFQL symbolic expression into an AST, gathering a maximum of static knowledge about expressions."
  ([env] (partial analyze env))
  ([env hfql] (unfold (φ env) ψ (postwalk reshape-traversals-up (prewalk reshape-traversals-down hfql)))))

(tests
  (::ast/pull-has-star? (extract (analyze {} [:a])))
  := false
  (::ast/pull-has-star? (extract (analyze {} [:a '*])))
  := true
  )

(defmulti emit* type)
(defmethod emit* :default [x] (quote-form x))
(defmethod emit*  #?(:clj clojure.lang.AMapEntry :cljs MapEntry) [[k v]]
  `(pair ~(quote-form k) ~(quote-form v)))

(defn emit [w] ; emit :: Comonad w => w a -> (a, [(a, [...]) ...])
  (pair
    (extract w)
    (mapv emit (unwrap w))))

(defn hydrate [[point children]] ; hydrate :: (a, [(a, [...]) ...]) -> RoseTree a
  (rose-tree point (mapv hydrate children)))

(defn marshal [w] (fmap (comp escape #(update % ::ast/form emit*)) w))
(defn unmarshal [rose-tree] (fmap #(update % ::ast/form unquote*) rose-tree))

(defmacro hfql [form] `(unmarshal (hydrate ~(emit (marshal (analyze &env form))))))

(defn- traverse-two [f W1 W2]
  (rose-tree (f W1 W2)
    (mapv (partial traverse-two f) (unwrap W1) (unwrap W2))))

(defn wrap-in-pull [W]
  (let [point (extract W)
        pullW (analyze (::ast/lexical-env point) [(::ast/form point)])]
    (rose-tree (extract pullW)
      [(traverse-two (fn [_W1 W2] (extract W2))
         (first (unwrap pullW)) W)])))


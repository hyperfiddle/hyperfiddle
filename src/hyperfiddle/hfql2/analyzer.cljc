(ns hyperfiddle.hfql2.analyzer
  (:require [clojure.core :as cc]
            [contrib.data :refer [update-existing]]
            [contrib.walk :refer [prewalk postwalk]]
            [hyperfiddle.hfql2.analyzer.lambda :refer [lambda?]]
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

(defn quoted? [form]
  (and (seq? form) (= 'quote (first form))))

(defn unquote* [form]
  (if (and (seq? form) (= 'quote (first form)))
    (second form)
    form))

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
    (= '* form)           ::star
    (or (= '... form)
      (nat-int? form))    ::recur
    (symbol? form)        ::symbol
    (keyword? form)       ::keyword
    (string? form)        ::string
    (fn? form)            ::fn
    (var-form? form)      ::var
    (symbolic-call? form) ::call
    (pair? form)          ::traversal
    (record? form)        ::literal
    (map? form)           ::traversals
    (vector? form)        ::pull
    (quoted-var-form? form) ::quoted-var
    (quoted? form)        ::literal
    (lambda? form)        ::lambda
    ;; TODO ifn? – maybe
    ;; TODO? method sym
    :else
    ::literal))

(defn- reshape-traversals-down [hfql]
  (case (projection-type hfql)
    ::call (reduced (reduced hfql))
    ::lambda (reduced (reduced hfql))
    ::literal (reduced (reduced hfql))
    ::quoted-var (reduced (reduced hfql))
    (::pull ::traversals ::traversal ::star ::recur ::symbol ::keyword ::string ::fn ::var)
    hfql))

(defn- reshape-traversals-up [hfql]
  (if (reduced? hfql)
    (unreduced hfql)
    (case (projection-type hfql)
      ::pull (into [] (mapcat (fn [hfql] (if (and (vector? hfql) (not (pair? hfql))) hfql [hfql])) hfql))
      ::traversals (if (= 1 (count hfql))
                     (if-let [metas (meta hfql)]
                       (vary-meta (apply pair (first hfql)) merge metas)
                       (first hfql))
                     (with-meta (into [] hfql) (meta hfql)))
      (::call ::lambda ::literal ::quoted-var) (throw (ex-info "Encountered invalid AST node." {:value hfql}))
      (::traversal ::star ::recur ::symbol ::keyword ::string ::fn ::var)
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

(defn literal? [obj]
  (or (nil? obj)
    (number? obj)
    (string? obj)
    (keyword? obj)))

(defn realize ; keeping around for reference
  "Realize a lazy-seq to a plain list. Values that are not lazy-seqs passes
  through. Differs from `doall` in that `(str (doall seq))` will print an opaque
  object address, whereas `(str (realize seq))` will print as a plain list. Will
  hang forever on infinite seqs. Also see `unchunk-seq`."
  [supposedly-lazy-seq]
  (if (seq? supposedly-lazy-seq)
    (with-meta
      (or (list* supposedly-lazy-seq) ())
      (meta supposedly-lazy-seq))
    supposedly-lazy-seq))

(defn quote "Quote a form. Invoke it as `(#'quote x)` to avoid conflict with the 'quote special form."
  [form]
  (if (or (literal? form) (quoted? form))
    form
    (list 'quote (realize form))))

(defn quote-nat-ints [form] (if (nat-int? form) (list 'quote form) form))

(defn- lexical-bindings-map [lexical-env]
  (zipmap (map #(#'quote %) (keys lexical-env))
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

(defn- compute-id [node] (hash (select-keys node #{::type ::form ::symbolic-edge ::symbolic-edges ::form-short ::form-resolved})))

(defn- φ ; phi
  "An algebra collecting static knowledge about an HFQL expression."
  ([env] (partial φ env))
  ([env form]
   (as-> (meta form) %
     (merge
       %
       {::type        (projection-type form)
        ::form        (#'quote form)
        ::lexical-env (lexical-bindings-map env)
        ::partial true ; a partial point depends on %. % is late-bound.
        }
       (case (projection-type form)
         ::star      {} ; '* and recur have no edge, and no value on purpose. They are purely syntactic.
         ::recur     {::recursion-limit (if (nat-int? form) form ##Inf)}
         ::literal   {::symbolic-edge form}
         ::keyword   {::symbolic-edge form
                      ::form-resolved form ; keyword auto-resolve at read-time.
                      ::form-short #?(:clj (aliased-form form), :cljs form)}
         ::string    {::symbolic-edge form}
         ::symbol    {::symbolic-edge form ; potentially aliased
                      ::value (escape-method-call form), ; macroexpand will auto-resolve
                      ::var (var-resolver env form) ; nil if symbol is lexicaly bound
                      ::form-resolved (resolver env form) ; resolve via eventual ns alias
                      ::form-short #?(:clj (aliased-form form), :cljs form)}
         ::var       {::symbolic-edge form ; the var object itself
                      ::value form ; also the var object itself
                      ::parameters '(%)
                      ;; TODO add aliased form
                      }
         ::quoted-var {::form (unquote* form)
                       ::symbolic-edge form
                       ;; TODO add aliased form
                       }
         ::pull      (let [recursion-point (some-fn #{'...} nat-int?)
                           form' (vec (remove recursion-point form))]
                       (merge
                         {::form form'
                          ::symbolic-edges form'
                          ::has-star? (boolean (some #(= ::star (::type %)) (map (φ env) form)))}
                         (when-some [recursion (first (filter recursion-point form))]
                           {::recursion-point true
                            ::recursion-limit (::recursion-limit (φ env recursion))})))
         ::fn        {::symbolic-edge form ; e.g. '#function[clojure.core/identity] – useful for tests
                      ::value form} ; already resolved
         ::lambda    (let [base {::symbolic-edge (let [[_fn ?name ?parameters] form]
                                                   (if (symbol? ?name)
                                                     `(~'fn ~?name ~'...)
                                                     '(fn anonymous ...)))
                                 ::parameters '(%)
                                 ::partial true
                                 ::value form}]
                       (if-let [kw (get-kw-fn-form-kw form)]
                         (merge base (select-keys (φ env kw) #{::symbolic-edge ::form-resolved ::form-short}) {::kind ::keyword-fn})
                         base))
         ::call      (let [[f & params] form] ; symbolic – quoted by defmacro
                       {::symbolic-edge form ; symbolic – interpreter will template later
                        ::value f ; macroexpand will auto-resolve, from lexical, dynamic or global scope.
                        ::var (var-resolver env f)
                        ::parameters params
                        ::arguments (replace {'% ''%} params)
                        ::partial (contains? (set params) '%)
                        ::form-resolved (resolver env f)
                        ::lexical-env (let [params (remove #{'%} params)]
                                        (merge (lexical-bindings-map env)
                                          (zipmap (map #'quote (filter symbol? params))
                                            (filter symbol? params))))})
         ::traversal (let [edge (φ env (fst form))
                           base (case (::type edge)
                                  ::star {::kind ::star, ::multiplicity ::many}
                                  {::edge edge, ::lexical-env (::lexical-env edge)})
                           continuation (φ env (snd form))]
                       (merge base
                         {::partial (and (::partial edge) (::partial continuation))}
                         (when (= ::star (::kind continuation)) {::multiplicity ::many})
                         (when (= ::recur (::type continuation))
                           {::recursion-point true
                            ::recursion-limit (::recursion-limit continuation)})
                         ))))
     (assoc % ::id (compute-id %)))))

(defmulti -escape ::type)
(defmethod -escape ::star [point] point)
(defmethod -escape ::recur [point] point)
(defmethod -escape ::literal [point] point)
(defmethod -escape ::keyword [point] (update point ::form-short #'quote))
(defmethod -escape ::string [point] point)
(defmethod -escape ::symbol [point] (-> (update point ::symbolic-edge #'quote)
                                      (update ::form-short #'quote)))
(defmethod -escape ::var [point] (update point ::parameters #'quote))
(defmethod -escape ::quoted-var [point] (update point ::form unquote*)) ; special case
(defmethod -escape ::pull [point] (-> (update point ::form #'quote)
                                    (update ::symbolic-edges #'quote)))
(defmethod -escape ::fn [point] (update point ::symbolic-edge #'quote))
(defmethod -escape ::lambda [point] (-> (update point ::symbolic-edge #'quote)
                                      (update ::parameters #'quote)))

(defmethod -escape ::call [point] (-> (update point ::symbolic-edge #'quote)
                                    (update ::parameters #'quote)
                                    (update ::arguments conj 'list)))

(defmethod -escape ::traversal [point] (update-existing point ::edge -escape))

(defn escape [point]
  (-escape (update point ::form #'quote)))


(defn- ψ ; psi
  "A coalgebra describe how to unfold an HFQL expression into a tree. For any HFQL form, return its children."
  [hfql]
  (case (projection-type hfql)
    ::pull      hfql
    ::traversal [(snd hfql)]
    (::star ::keyword ::symbol ::var ::string ::fn ::call ::lambda ::literal ::quoted-var)
    ()
    ::recur ; we don't produce an infinite AST to ease debugging.
    ()))

(defn- unfold [φ ψ seed] (fmap φ (coiter ψ seed)))

(defn analyze
  "Unfold an HFQL symbolic expression into an AST, gathering a maximum of static knowledge about expressions."
  ([env] (partial analyze env))
  ([env hfql] (unfold (φ env) ψ (postwalk reshape-traversals-up (prewalk reshape-traversals-down hfql)))))

(tests
  (::has-star? (extract (analyze {} [:a])))
  := false
  (::has-star? (extract (analyze {} [:a '*])))
  := true
  )

(defmulti emit* type)
(defmethod emit* :default [x] (#'quote x))
(defmethod emit*  #?(:clj clojure.lang.AMapEntry :cljs MapEntry) [[k v]]
  `(pair ~(#'quote k) ~(#'quote v)))

(defn emit [w] ; emit :: Comonad w => w a -> (a, [(a, [...]) ...])
  (pair
    (extract w)
    (mapv emit (unwrap w))))

(defn hydrate [[point children]] ; hydrate :: (a, [(a, [...]) ...]) -> RoseTree a
  (rose-tree point (mapv hydrate children)))

(defn marshal [w] (fmap (comp escape #(update % ::form emit*)) w))
(defn unmarshal [rose-tree] (fmap #(update % ::form unquote*) rose-tree))

(defmacro hfql [form] `(unmarshal (hydrate ~(emit (marshal (analyze &env form))))))

(defn- traverse-two [f W1 W2]
  (rose-tree (f W1 W2)
    (mapv (partial traverse-two f) (unwrap W1) (unwrap W2))))

(defn wrap-in-pull [W]
  (let [point (extract W)
        pullW (analyze (::lexical-env point) [(::form point)])]
    (rose-tree (extract pullW)
      [(traverse-two (fn [_W1 W2] (extract W2))
         (first (unwrap pullW)) W)])))


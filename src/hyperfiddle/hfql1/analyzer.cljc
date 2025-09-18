(ns hyperfiddle.hfql1.analyzer
  (:require [clojure.core :as cc]
            [contrib.walk :refer [prewalk]]
            [hyperfiddle.rcf :refer [tests]]
            [hyperfiddle.hfql1.functor :refer [fmap]]
            [hyperfiddle.hfql1.bifunctor :refer [pair pair? fst snd &&&]]
            [hyperfiddle.hfql1.comonad :refer [extract]]
            [hyperfiddle.hfql1.cofree :refer [unwrap]]
            [hyperfiddle.hfql1.rose-tree :refer [rose-tree coiter]]
            [clojure.string :as str]))

(defn- var-form? [form]
  (and (seq? form)
    (= 'var (first form))
    (= 2 (count form))))

(defn- symbolic-call? [form]
  (and (seq? form)
    (let [[f & _args] form]
      (and (symbol? f) (not (#{'fn `fn 'fn* 'quote 'var 'hfql `hfql} f))))))

(defn- lambda? [form]
  (and (seq? form)
    (let [[f & _args] form]
      (#{'fn `fn 'fn*} f))))

(defn- quoted? [form]
  (and (seq? form) (= 'quote (first form))))

(defn projection-type [form]
  (cond
    (= '* form)           ::star
    (or (= '... form)
      (number? form))     ::recur
    (symbol? form)        ::symbol
    (keyword? form)       ::keyword
    (string? form)        ::string
    (fn? form)            ::fn
    (var-form? form)      ::var
    (symbolic-call? form) ::call
    (pair? form)          ::traversal
    (map? form)           ::traversals
    (vector? form)        ::pull
    (lambda? form)        ::lambda
    ;; TODO kw-fn
    ;; TODO ifn? – maybe
    ;; TODO method sym
    :else                 (throw (ex-info "unknown form" {:form form}))))

(defn- reshape-traversals [hfql]
  (case (projection-type hfql)
    ::pull (into [] (mapcat (fn [hfql] (if (= ::traversals (projection-type hfql)) (seq hfql) [hfql]))) hfql)
    ::call (reduced hfql)
    ::lambda (reduced hfql)
    ::traversals (if (= 1 (count hfql))
                   (first hfql)
                   (into [] hfql))
    (::traversal ::star ::recur ::symbol ::keyword ::string ::fn ::var)
    hfql))

(tests
  (reshape-traversals {:a 1, :b 2})
  := [[:a 1] [:b 2]]
  (reshape-traversals {:a 1})
  := [:a 1]
  (reshape-traversals [{:a 1, :b 2}])
  := [[:a 1] [:b 2]]
  )

(defn- lexical? [env sym] (contains? env sym))

(defn- indirect-method-call? [sym]
  (and (simple-symbol? sym)
    (str/starts-with? (name sym) ".")))

(defn- direct-method-call? [sym]
  (and (qualified-symbol? sym)
    (str/starts-with? (name sym) ".")))

(defn- var-resolver [env form]
  (cond
    (lexical? env form)          nil
    (indirect-method-call? form) nil
    (direct-method-call? form)   nil
    (symbol? form)               `(var ~form)
    :else                        nil))

(defn- resolver [env form]
  (cond
    (lexical? env form)          form
    (indirect-method-call? form) nil
    (direct-method-call? form)   `(resolve-direct-method ~form)
    (symbol? form)               `(symbol (var ~form))
    :else                        nil))

(defn- literal? [obj]
  (or (nil? obj)
    (number? obj)
    (string? obj)
    (keyword? obj)))

(defn- quote "Quote a form. Invoke it as `(#'quote x)` to avoid conflict with the 'quote special form."
  [form]
  (if (or (literal? form) (quoted? form))
    form
    (list 'quote form)))

(defn- lexical-bindings-map [lexical-env]
  (zipmap (map #(#'quote %) (keys lexical-env))
    (keys lexical-env)))

(defn- φ ; phi
  "An algebra collecting static knowledge about an HFQL expression."
  ([env] (partial φ env))
  ([env form]
   (merge
     {::type        (projection-type form)
      ::form        (#'quote form)
      ::lexical-env (lexical-bindings-map env)}
     (case (projection-type form)
       ::star      {} ; '* and recur have no edge, and no value on purpose. They are purely syntactic.
       ::recur     {::recursion-limit (if (number? form) form ##Inf)}
       ::keyword   {::symbolic-edge form}
       ::string    {::symbolic-edge form}
       ::symbol    {::symbolic-edge (#'quote form) ; potentially aliased
                    ::value form, ; macroexpand will auto-resolve
                    ::var (var-resolver env form) ; nil if symbol is lexicaly bound
                    ::form-resolved (#'quote (resolver env form))} ; resolve via eventual ns alias
       ::var       {::symbolic-edge form ; the var object itself
                    ::value form} ; also the var object itself
       ::pull      {::symbolic-edges (#'quote form)}
       ::fn        {::symbolic-edge (#'quote form) ; e.g. '#function[clojure.core/identity] – useful for tests
                    ::value form} ; already resolved
       ::lambda    {::symbolic-edge (#'quote (let [[_fn ?name ?args] form]
                                               (if (symbol? ?name)
                                                 `(~'fn ~?name ~'…)
                                                 '(fn anonymous …))))
                    ::parameters (#'quote '(%))
                    ::value form}
       ::call      (let [[f & params] form] ; symbolic – quoted by defmacro
                     {::symbolic-edge (#'quote form) ; symbolic – interpreter will template later
                      ::value f ; macroexpand will auto-resolve, from lexical, dynamic or global scope.
                      ::var (var-resolver env f)
                      ::parameters (#'quote params)
                      ::arguments (cons 'list (replace {'% ''%} params))
                      ::form-resolved (resolver env f)
                      ::lexical-env (merge (lexical-bindings-map env)
                                      (zipmap (map #'quote (filter symbol? params))
                                        (filter symbol? params)))})
       ::traversal (let [edge (φ env (fst form))
                         base (case (::type edge)
                                ::star {::kind ::star}
                                {::edge edge, ::lexical-env (::lexical-env edge)})
                         continuation (φ env (snd form))]
                     (merge base
                       (when (= ::star (::kind continuation)) {::multiplicity ::many})
                       (when (= ::recur (::type continuation)) {::recursion-point true
                                                                ::recursion-limit (::recursion-limit continuation)})
                       ))))))

;; #_(let [% 1]
;;   (hfql (inc %)))
;; (hfql (inc %))

(defn- ψ ; psi
  "A coalgebra describe how to unfold an HFQL expression into a tree. For any HFQL form, return its children."
  [hfql]
  (case (projection-type hfql)
    ::pull      hfql
    ::traversal [(snd hfql)]
    (::star ::keyword ::symbol ::var ::string ::fn ::call ::lambda)
    ()
    ::recur ; we don't produce an infinite AST to ease debugging.
    ()))

(defn- unfold [φ ψ seed] (fmap φ (coiter ψ seed)))

(defn analyze
  "Unfold an HFQL symbolic expression into an AST, gathering a maximum of static knowledge about expressions."
  ([env] (partial analyze env))
  ([env hfql] (unfold (&&& identity (φ env)) ψ (prewalk reshape-traversals hfql))))

(comment
  (analyze {} [])
  (hyperfiddle.hfql1.rose-tree/unfold ψ [])
  (hyperfiddle.hfql1.rose-tree/unfold ψ 'foo)
  (hyperfiddle.hfql1.rose-tree/coiter ψ 'foo)
  (analyze {} 'foo)
  (analyze {} "foo")
  (hyperfiddle.hfql1.rose-tree/coiter ψ inc)
  (analyze {} 'inc)
  (hyperfiddle.hfql1.rose-tree/coiter ψ ['foo "foo"])
  (analyze {} ['foo "foo"])
  (analyze {} {:a inc})
  )

(defmulti emit* type)
(defmethod emit* :default [x] (#'quote x))
(defmethod emit*  #?(:clj clojure.lang.AMapEntry :cljs MapEntry) [[k v]]
  `(pair ~(#'quote k) ~(#'quote v)))

(defn emit [W]
  (let [[form meta] (extract W)]
    {::form (emit* form)
     ::meta meta
     ::children (mapv emit (unwrap W))}))

(defn- unquote* [form]
  (if (and (seq? form) (= 'quote (first form)))
    (second form)
    form))

(defn hydrate [{::keys [form meta children]}] (rose-tree (pair (unquote* form) meta) (mapv hydrate children)))

#_(tests
  "hydrate ∘ emit = id"
  (let [a (analyze {} {:a inc :b dec})]
    a := ((comp hydrate emit) a))
  )

(defmacro hfql [form] `(hydrate ~(emit (analyze &env form))))

(comment
  (hfql inc)
  (hfql #'inc)
  (hfql [:a *])
  (hfql {:a inc})
  (let [fork (fn [x] (list x x))]
    (hfql {identity {fork {fork fork}}})) 

  (hfql (ns-publics *ns*) ) 
  (hfql (ns-publics %) )
  

  (hfql (fn _inc [%] (inc %)))

  (hfql {:a ...})
  )


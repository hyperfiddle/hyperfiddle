(ns hyperfiddle.hfql2.analyzer
  (:require [clojure.core :as cc]
            [contrib.data :refer [realize update-existing]]
            [contrib.walk :refer [prewalk postwalk]]
            [hyperfiddle.hfql2.analyzer.lambda :refer [lambda?]]
            [hyperfiddle.hfql2 :as-alias hfql]
            [hyperfiddle.hfql2.ast :as ast]
            [hyperfiddle.rcf :refer [tests]]
            [hyperfiddle.ct.functor :refer [fmap]]
            [hyperfiddle.ct.pair :refer [pair pair? fst snd]]
            [hyperfiddle.ct.comonad :refer [extract]]
            [hyperfiddle.ct.cofree :refer [unwrap]]
            [hyperfiddle.ct.rose-tree :refer [rose-tree coiter]]
            [clojure.string :as str])
  #?(:cljs (:require-macros [hyperfiddle.hfql2.analyzer :refer [hfql resolve-direct-method resolve-static-method]])))

(deftype OptionsFormWrap [inner])

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



(defn- quoted-symbol-form? [form]
  (and (quoted? form) (symbol? (second form))))

(defn form-type [form]
  (cond
    (instance? OptionsFormWrap form) ::ast/options-form
    (= '* form)           ::ast/star
    (or (= '... form)
      (nat-int? form))    ::ast/recur
    (symbol? form)        ::ast/symbol
    (keyword? form)       ::ast/keyword
    (string? form)        ::ast/string
    (fn? form)            ::ast/runtime-value
    (var-form? form)      ::ast/var
    (symbolic-call? form) ::ast/call
    (pair? form)          ::ast/traversal
    (record? form)        ::ast/literal
    (map? form)           ::ast/traversals
    (vector? form)        ::ast/pull
    (quoted-var-form? form) ::ast/quoted-var
    (quoted-symbol-form? form) ::ast/quoted-symbol
    (quoted? form)    ::ast/literal
    (lambda? form)        ::ast/lambda
    ;; TODO ifn? – maybe
    ;; TODO? method sym
    :else
    ::ast/literal))

(defn- reshape-traversals-down [hfql]
  (case (form-type hfql)
    ::ast/call (reduced (reduced hfql))
    ::ast/lambda (reduced (reduced hfql))
    ::ast/literal (reduced (reduced hfql))
    ::ast/quoted-var (reduced (reduced hfql))
    ::ast/quoted-symbol (reduced (reduced hfql))
    ::ast/options-form (reduced (reduced hfql))
    (::ast/pull ::ast/traversals ::ast/traversal ::ast/star ::ast/recur ::ast/symbol ::ast/keyword ::ast/string ::ast/runtime-value ::ast/var)
    hfql))

(defn- reshape-traversals-up [hfql]
  (if (reduced? hfql)
    (unreduced hfql)
    (case (form-type hfql)
      ::ast/pull (into [] (mapcat (fn [hfql]
                                    (if (and (vector? hfql) (not (pair? hfql)))
                                      [(OptionsFormWrap. hfql)]
                                      [hfql]))
                           hfql))
      ::ast/traversals (if (= 1 (count hfql))
                     (if-let [metas (meta hfql)]
                       (vary-meta (apply pair (first hfql)) merge metas)
                       (first hfql))
                     (with-meta (into [] hfql) (meta hfql)))
      (::ast/call ::ast/lambda ::ast/literal ::ast/quoted-var ::ast/quoted-symbol) (throw (ex-info "Encountered invalid AST node." {:value hfql}))
      (::ast/traversal ::ast/star ::ast/recur ::ast/symbol ::ast/keyword ::ast/string ::ast/runtime-value ::ast/var ::ast/options-form)
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

(defn- method-call? [sym]
  (or (indirect-method-call? sym)
    (direct-method-call? sym)
    (static-method-call? sym)))

(defn- java-class-reference?
  "True if `sym` is a qualified symbol whose namespace refers to a Java class.
  `Math/abs` → true (auto-imported), `java.io.File/separator` → true (FQCN),
  `dustingetz.nav-jar/jar-filename` → false (Clojure ns)."
  [sym]
  #?(:clj  (when-let [ns-str (namespace sym)]
             (let [ns-sym (symbol ns-str)]
               (boolean
                 (or (try (Class/forName ns-str) (catch ClassNotFoundException _ nil))
                     (get (ns-imports *ns*) ns-sym)))))
     :cljs false))

(defn- supports-direct-method-syntax? []
  #?(:clj (>= 1.12 (parse-double (re-find #"^[0-9]+\.[0-9]+" (clojure-version))))
     :cljs false ; TODO I actually don't know. Maybe cljs does support it. Yes, cljs 1.12+ does support it.
     ))

(defn- escape-method-call [form]
  (cond
    (indirect-method-call? form) `#(~form %)

    (and (direct-method-call? form) (not (supports-direct-method-syntax?)))
    , (escape-method-call (symbol (name form)))

    :else form))

(defn- var-resolver [env form]
  (cond
    (lexical? env form)            nil
    (indirect-method-call? form)   nil
    (direct-method-call? form)     nil
    (java-class-reference? form)   nil   ;; Java static — no var
    (symbol? form)                 `(var ~form) ;; unqualified OR qualified Clojure var
    :else                          nil))

(defmacro resolve-direct-method [sym] ; No call to cc/resolve, respects ②
  {:pre [(direct-method-call? sym)]}
  `(symbol (.getName ~(symbol (namespace sym))) ~(name sym)))

(defmacro resolve-static-method
  "Resolve a qualified non-dot symbol to its FQN symbol. Emitted by `resolver`
  for forms matching `static-method-call?`. At macro expansion time we check
  whether the namespace part refers to a Java class. If so → synthesize the FQN
  symbol from the class name (like `resolve-direct-method`). Otherwise → Clojure
  var, resolved by the compiler (handles aliases)."
  [sym]
  {:pre [(static-method-call? sym)]}
  (if (and (not (:js-globals &env)) (java-class-reference? sym))
    ;; CLJ + Java class — resolve like resolve-direct-method
    `(symbol (.getName ~(symbol (namespace sym))) ~(name sym))
    ;; Clojure var (or CLJS) — compiler resolves FQN including aliases
    `(symbol (var ~sym))))

(defn- resolver [env form]
  (cond
    (lexical? env form)            form
    (indirect-method-call? form)   nil
    (direct-method-call? form)     `(resolve-direct-method ~form)
    (static-method-call? form)     `(resolve-static-method ~form)
    (symbol? form)                 `(symbol (var ~form))
    :else                          nil))

(defn- lexical-bindings-map [lexical-env]
  (zipmap (map #(quote-form %) (keys lexical-env)) (keys lexical-env)))

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

(declare unfold ψ)

(defn- φ ; phi
  "An algebra constructing AST nodes from an HFQL expression and its processed children.
  Leaf nodes ignore `children` (always `()`). Pull passes `children` directly to `->pull`.
  Traversal edges are constructed via recursive `(φ env edge-form ())` calls.
  Metadata and lexical-env are set uniformly at the end via mutators."
  ([env] (partial φ env))
  ([env form children]
   (if (instance? OptionsFormWrap form)
     ;; Options form: [attr k v ...].
     ;; Analyze inner attr via full unfold (supports traversal attrs),
     ;; split reader meta from options for provenance tracking.
     (let [inner-vec  (.-inner ^OptionsFormWrap form)
           _          (when (even? (count inner-vec))
                        (throw (ex-info "Options form [attr k v ...]: needs odd element count"
                                {:form inner-vec})))
           attr        (first inner-vec)
           opts-map    (not-empty (apply hash-map (rest inner-vec)))
           reader-meta (meta inner-vec)
           inner-node  (unfold (φ env) ψ attr)]
       (-> inner-node
         (ast/set-metadata reader-meta)   ; reader ^{...} on the inner vec
         (ast/set-options opts-map)))      ; options :k v pairs
     ;; Normal dispatch:
     (let [ftype (form-type form)
           node  (case ftype
                   ::ast/star      (ast/->star)
                   ::ast/recur     (if (nat-int? form)
                                     (ast/->bounded-recursion form)
                                     (ast/->unbounded-recursion))
                   ::ast/literal   (ast/->literal form)
                   ::ast/keyword   (ast/->keyword form #?(:clj (aliased-form form) :cljs form))
                   ::ast/string    (ast/->string form)
                   ::ast/symbol    (ast/->symbol form (escape-method-call form) (var-resolver env form)
                                     (resolver env form) #?(:clj (aliased-form form) :cljs form))
                   ::ast/var       (let [sym (second form)]
                                     (ast/->var form (list 'var (resolver env sym))
                                       #?(:clj (list 'var (aliased-form sym)) :cljs (list 'var sym))
                                       :resolved-runtime-value #?(:clj (resolve sym) :cljs nil)))
                   ::ast/quoted-var (let [sym (second (unquote* form))]
                                      (ast/->quoted-var (unquote* form) (list 'var (resolver env sym))
                                        #?(:clj (list 'var (aliased-form sym)) :cljs (list 'var sym))
                                        :resolved-runtime-value #?(:clj (resolve sym) :cljs nil)))
                   ::ast/runtime-value (ast/->runtime-value form :metadata (meta form)) ; thread fn metadata (e.g. ::label from discover-methods) into AST attrs
                   ::ast/quoted-symbol (ast/->quoted-symbol (second form))
                   ::ast/lambda    (let [kw (get-kw-fn-form-kw form)]
                                     (if kw
                                       (let [kw-node (φ env kw ())]
                                         (ast/->lambda form '(%)
                                           :kind ::ast/keyword-fn
                                           :form-resolved (ast/form-resolved kw-node)
                                           :form-short (ast/form-short kw-node)))
                                       (ast/->lambda form '(%))))
                   ::ast/call      (let [[f & params] form]
                                     (ast/->call form f (var-resolver env f) params
                                       (replace {'% ''%} params) (resolver env f)))
                   ::ast/pull      (ast/->pull children)
                   ::ast/traversal (let [edge-form (fst form)
                                         cont-node (first children)]
                                     (if (= '* edge-form)
                                       (ast/->star-traversal cont-node)
                                       (let [edge-node (φ env edge-form ())
                                             mult (when (ast/star-traversal? cont-node) ::ast/many)]
                                         (ast/->edge-traversal edge-node cont-node :multiplicity mult
                                           :lexical-env (or (not-empty (ast/lexical-env edge-node))
                                                          (lexical-bindings-map env)))))))
           lenv  (lexical-bindings-map env)
           lenv  (if (= ::ast/call ftype)
                   (let [[_f & params] form
                         ps (remove #{'%} params)]
                     (merge lenv (zipmap (map quote-form (filter symbol? ps))
                                   (filter symbol? ps))))
                   lenv)]
       (cond-> (ast/set-metadata node (meta form))
         (not (#{::ast/traversal ::ast/keyword ::ast/string} ftype)) (ast/set-lexical-env lenv))))))

(declare -escape)
(defn escape [point] (-escape (update point ::ast/form quote-form)))

(defmulti -escape ::ast/type)
(defmethod -escape ::ast/star [point] point)
(defmethod -escape ::ast/recur [point] point)
(defmethod -escape ::ast/literal [point] point)
(defmethod -escape ::ast/keyword [point] point)
(defmethod -escape ::ast/string [point] point)
(defmethod -escape ::ast/symbol [point] (update point ::ast/form-short quote-form))
(defmethod -escape ::ast/var [point] (-> (update point ::ast/form-resolved quote-form)
                                    (update ::ast/form-short quote-form)))
(defmethod -escape ::ast/quoted-var [point] (-> (update point ::ast/form-resolved quote-form)
                                           (update ::ast/form-short quote-form)))
(defmethod -escape ::ast/pull [point] point)
(defmethod -escape ::ast/runtime-value [point] point)
(defmethod -escape ::ast/quoted-symbol [point] point)
(defmethod -escape ::ast/lambda [point] (update point ::ast/parameters quote-form))

(defmethod -escape ::ast/call [point] (-> (update point ::ast/parameters quote-form)
                                        (update ::ast/arguments conj 'list)))

(defmethod -escape ::ast/traversal [point] (update point ::ast/edge (comp escape extract)))



(defn- ψ ; psi
  "A coalgebra describe how to unfold an HFQL expression into a tree. For any HFQL form, return its children."
  [hfql]
  (case (form-type hfql)
    ::ast/pull      hfql
    ::ast/traversal [(snd hfql)]
    ::ast/options-form () ; opaque leaf — φ handles inner analysis via unfold
    (::ast/star ::ast/keyword ::ast/symbol ::ast/var ::ast/string ::ast/runtime-value ::ast/quoted-symbol ::ast/call ::ast/lambda ::ast/literal ::ast/quoted-var)
    ()
    ::ast/recur ; we don't produce an infinite AST to ease debugging.
    ()))

(defn- unfold [φ ψ seed]
  (let [tree (coiter ψ seed)]
    ((fn fold [t]
       (let [cs (mapv fold (unwrap t))]
         (φ (extract t) cs)))
     tree)))

(defn analyze
  "Unfold an HFQL symbolic expression into an AST, gathering a maximum of static knowledge about expressions."
  ([env] (partial analyze env))
  ([env hfql] (unfold (φ env) ψ (postwalk reshape-traversals-up (prewalk reshape-traversals-down hfql)))))

(tests
  (ast/pull-has-star? (analyze {} [:a]))
  := false
  (ast/pull-has-star? (analyze {} [:a '*]))
  := true
  )

(defmulti emit* type)
(defmethod emit* :default [x] (quote-form x))
(defmethod emit*  #?(:clj clojure.lang.AMapEntry :cljs MapEntry) [[k v]]
  `(pair ~(emit* k) ~(emit* v)))

(defn emit [w] ; emit :: Cofree w => w a -> (a, [(a, [...]) ...])
  (pair
    (extract w)
    (mapv emit (unwrap w))))

(defn hydrate [[point children]] ; hydrate :: (a, [(a, [...]) ...]) -> RoseTree a
  (rose-tree point (mapv hydrate children)))

(defn marshal [w] (fmap (comp escape #(update % ::ast/form emit*)) w))
(defn unmarshal [ast-node]
  (fmap (fn [attrs]
          (cond-> (-> attrs
                    (update ::ast/form unquote*)
                    (update-existing ::ast/form-resolved unquote*)
                    (update-existing ::ast/form-short unquote*)
                    (update-existing ::ast/parameters unquote*))
            (::ast/edge attrs)
            (update ::ast/edge (fn [m] (rose-tree (update m ::ast/form unquote*) ())))))
    ast-node))

(defmacro hfql [form] `(unmarshal (hydrate ~(emit (marshal (analyze &env form))))))

(tests
  (ast/pull-has-star? (hfql [:a]))
  := false
  (ast/pull-has-star? (hfql [:a *]))
  := true
  )

(tests
  "form ∘ hfql = id — keywords"
  (ast/form (hfql []))           := []
  (ast/form (hfql [:a]))         := [:a]
  )

(tests
  "form ∘ hfql = id — star"
  (ast/form (hfql [*]))          := '[*]
  (ast/form (hfql [:a *]))       := '[:a *]
  (ast/form (hfql [* :a]))       := '[* :a]
  (ast/form (hfql [:a * :b]))    := '[:a * :b]
  )

(tests
  "form ∘ hfql = id — recursion"
  (ast/form (hfql [:a ...]))     := '[:a ...]
  (ast/form (hfql [:a 3]))       := [:a 3]
  (ast/form (hfql [:a ''3]))     := '[:a '3])

(tests
  "form ∘ hfql = id — leaf types in pull"
  (ast/form (hfql [:a inc]))     := '[:a inc]
  (ast/form (hfql [:a "name"]))  := [:a "name"]
  (ast/form (hfql [:a (inc %)]))  := '[:a (inc %)]
  (eval '(ast/form (hfql [:a #'inc]))) ; HACK RCF workaround. RCF chokes on `#'inc` as a value in the macroexpanded form.
  := '[:a (var inc)]
  (eval '(ast/form (hfql [:a '#'inc]))) ; HACK RCF workaround. RCF chokes on `#'inc` as a value in the macroexpanded form.
  := '[:a (quote (var inc))]
  )

(tests
  "form ∘ hfql = id — keyword traversal"
  (ast/form (hfql {:a [:b]}))       := {:a [:b]}
  )

(tests
  "form ∘ hfql = id — star traversal"
  (ast/form (hfql {* inc}))        := '{* inc}
  (ast/form (hfql {* [:a]}))       := '{* [:a]}
  )

(tests
  "form ∘ hfql = id — traversal with recursion"
  (ast/form (hfql {inc ...}))  := '{inc ...}
  (ast/form (hfql {inc 3}))    := '{inc 3})

(tests
  "form ∘ hfql = id — call traversal"
  (ast/form (hfql {(inc %) ...}))  := '{(inc %) ...})

(tests
  "form ∘ hfql = id — nested"
  (ast/form (hfql [:a {:b [:c :d]}]))      := '[:a {:b [:c :d]}]
  (ast/form (hfql [:a * {:b [:c ...]}]))   := '[:a * {:b [:c ...]}]
  (ast/form (hfql [{:a [{:b [:c]}]}]))     := '[{:a [{:b [:c]}]}])

(tests
  "form ∘ hfql = id — mixed pull"
  (ast/form (hfql [:a inc "name" (inc %) *]))
  := '[:a inc "name" (inc %) *]
  (ast/form (hfql [:a {:b [:c]} :d]))
  := '[:a {:b [:c]} :d])

(tests
  "form ∘ hfql = id — metadata on pull"
  (meta (ast/form (hfql ^{:x 1} [:a])))        := {:x 1}
  (meta (ast/form (hfql ^{:x 1} [:a :b])))     := {:x 1}
  (meta (ast/form (hfql ^{:x 1} [:a * :b])))   := {:x 1})

(tests
  "form ∘ hfql = id — metadata on traversal"
  (let [m (meta (first (ast/form (hfql [^{:x 1} {:a [:b]}]))))]
    (:x m) := 1)
  (let [m (meta (first (ast/form (hfql [^{:x 1} {* inc}]))))]
    (:x m) := 1))

(tests
  "form ∘ hfql = id — metadata on nested pull"
  (let [form (ast/form (hfql ^{:outer true} [:a {:b ^{:inner true} [:c :d]}]))]
    (:outer (meta form)) := true
    (let [traversal-form (second form)         ; {:b [:c :d]}
          inner-pull (val (first traversal-form))]
      (:inner (meta inner-pull)) := true)))

;; ─── Options syntax roundtrip tests ──────────────────────────────────────────

(tests
  "options form — keyword roundtrips as options vector"
  (let [node (first (unwrap (hfql [[:a :b :c]])))]
    (ast/keyword-node? node) := true
    (ast/form node) := [:a :b :c]
    ;; Values accessible as top-level attrs on the node:
    (:b (extract node)) := :c
    ;; ::options records which keys came from options syntax:
    (::ast/options (extract node)) := #{:b}))

(tests
  "options form — pull with keyword options roundtrips"
  (ast/form (hfql [:user/name [:user/email ::hfql/default "none"]]))
  := [:user/name [:user/email ::hfql/default "none"]])

(tests
  "options form — traversal options roundtrip as vector"
  (let [f (ast/form (hfql [[{:a [:b]} :c :d]]))]
    (first f) := [{:a [:b]} :c :d]))

(tests
  "options form — single-element inner vector [:a], no opts"
  (let [node (first (unwrap (hfql [[:a]])))]
    (ast/form node) := :a
    (::ast/metadata (extract node)) := nil
    (::ast/options (extract node)) := nil))

;; ─── Options syntax attrs access tests ──────────────────────────────────────

(tests
  "options form — option values are top-level attrs"
  (let [email (second (unwrap (hfql [:user/name [:user/email ::hfql/default "redacted"]])))]
    (::hfql/default (extract email)) := "redacted"))

(tests
  "options form — multiple options as top-level attrs"
  (let [node (first (unwrap (hfql [[:user/email ::hfql/default "none" ::hfql/as :email]])))]
    (::hfql/default (extract node)) := "none"
    (::hfql/as (extract node)) := :email
    (::ast/options (extract node)) := #{::hfql/default ::hfql/as}))

(tests
  "options form — traversal attr gets options as attrs"
  (let [node (first (unwrap (hfql [[{:a [:b]} :c :d]])))]
    (ast/traversal? node) := true
    (:c (extract node)) := :d))

;; ─── Options syntax structure tests ─────────────────────────────────────────

(tests
  "options form — correct edge count on outer pull"
  (count (unwrap (hfql [:user/name [:user/email ::hfql/default "none"]]))) := 2)

(tests
  "plain pull [:a :b :c] is unaffected — no inner vector"
  (count (unwrap (hfql [:a :b :c]))) := 3)

;; ─── Options syntax: symbolic-edge not affected ─────────────────────────────

(tests
  "options form — symbolic-edge returns raw keyword, not options vector"
  (let [node (first (unwrap (hfql [[:user/email ::hfql/default "none"]])))]
    (ast/symbolic-edge node) := :user/email))

(defn- traverse-two [f W1 W2]
  (rose-tree (f W1 W2)
    (mapv (partial traverse-two f) (unwrap W1) (unwrap W2))))

(defn wrap-in-pull [W]
  (let [point (extract W)
        pullW (analyze (::ast/lexical-env point) [(::ast/form point)])]
    (rose-tree (extract pullW)
      [(traverse-two (fn [_W1 W2] (extract W2))
         (first (unwrap pullW)) W)])))


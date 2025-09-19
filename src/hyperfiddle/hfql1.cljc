;; * What is it
;;   - HFQL is a data navigation and querying utility.
;;   - Given a symbolic query representation, HFQL produces navigation functions.
;;   - The query language is an extension of Datomic Pull syntax.
;;     - It is a goal for HFQL to support any valid Datomic pull expression.
;;
;;
;;   - Technically, HFQL is
;;     - a comonad: because HFQL-generated step functions carries and model the
;;       navigation context, e.g. lexical scope, parent/sibling dependencies,
;;       etc.
;;     - a cofree comonad: because calling an HFQL generated function returns a
;;       pair `[result continuation]`, where `continuation` is itself an HFQL
;;       generated function, modeling "how to navigate further". It is possible
;;       to extend the navigation plan without running it or reanalyzing it.
;;     - a coalgebra producing a cofree comonad: because repeated applications
;;       of `continuation` over `result` (coiteration) produces the whole cofree
;;       structure. Eagerly realizing this structure gives us half of
;;       datomic.api/pull. The other half is in charge of producing a nested structure.
;; * Rules of HFQL – internal
;;
;; ** Don't force an evaluation model ①
;;    HFQL can be lazy or eager. But it's on the user to decide how to run it.
;;    HFQL should produce values step by step and provide continuations for next
;;    steps. The user can then run all eagerly or consume one step at a time.
;;
;; ** Don't resolve ②
;;    Resolving in clojure is easy, but not simple. Resolving in clojurescript is
;;    neither at compile time, and impossible at runtime in practice. Gathering the
;;    knowledge to resolve correctly (in context) causes headaches and makes code
;;    brittle. Let the reader and compiler resolve for you, don't mess with the
;;    natural resolving process, or be doomed to reimplement more parts of the
;;    compiler than you'd like to. How?
;;    ```
;;    (defmacro capture-form+val [form]
;;    `{:form '~form, :evaluated ~form})
;;    ```
;;
;; ** Don't eval ③
;;    Eval is resolve's angry big brother. Eval in clojure is easy, but so hard to
;;    get right out of dummy cases that it's not practical. Eval in cljs is
;;    borderline impossible out of toy projects. Eval does resolve, so it requires
;;    at least as much context as resolve. It also requires knowledge of bindings
;;    (e.g. lexicals, ...). Just like resolve, collecting all those and calling
;;    eval just-the-right-way will make you rewrite a non negligible part of an
;;    interpreter. How to avoid eval? Your code already run inside of eval, so
;;    instead of calling a new eval context, leverage the current one. Don't
;;    capture code and then inject a runtime interpreter, instead rewrite code and
;;    let it be interpreted by the current -already running- interpreter :
;;
;;    ```
;;    (defmacro applicative-let [bindings & body]
;;      ; don't do that
;;      `(eval-ordered-let '~bindings '~body)
;;      ; instead do
;;      `(let [~@(toposort bindings)]
;;        ~@body))
;;    ```
;;
;; ** Capture all you can ④
;;    The need for resolve or eval often indicates a lack of information, and an
;;    attempt at recovering it. By capturing as much as you can at compile time and
;;    passing it to runtime, you can minimize the need to reconstruct information.
;;    Never throw away/forget information. Example:
;;    ```
;;    (defmacro capture [form]
;;      `(with-meta ~form ; will be evaluated
;;         {'~'&env ~&env, ; pay attention to quoting
;;          '~'&form '~&form, ; meta is preserved and contains e.g. line/col
;;          :ns (ns-name *ns*) ; some objects are not valid at read time (cljs) and must be turned into their symbolic form (e.g. #namespace[user])
;;        }))
;;    ```
;;
;; ** Pass raw forms to userland ⑤
;;
;;    Some values must be wrapped to be annotated. Keywords are famously annoying
;;    to annotate because they don't support metadata, compare by reference
;;    equality, and cannot be subclassed (no they really cannot, even if
;;    Keyword.class isn't final). Wrapping a keyword (e.g. with a deftype) will
;;    work for all use cases but two: `case` and `=`.
;;    - `case` expands to a low level lookup table with direct reference equality
;;      check. And can't be customized or overloaded.
;;    - `=` will call Keyword/.equals. So (= wrapped-foo :foo) will return true,
;;      but (= :foo wrapped-foo) will return false, breaking equality symmetry.
;;
;;    As a consequence of using a wrapper type, an `unwrap` function will appear
;;    (also often named `original-form`, or `form`). Calling `unwrap` is essential
;;    complexity. Forcing users to call `unwrap` is leaking internals, and turning
;;    essential complexity into accidental complexity. Having to call `unwrap` as a
;;    user, knowing when to call it, and why, is a real burden.
;;
;;    How to mitigate? Pass two values to userland: the already-unwrapped one
;;    first, and the wrapped one (or even better: it's annotations directly) as an
;;    extra, optional value.
;;
;;    ```
;;    ; don't
;;    (user-callback wrapped-foo-kw)
;;    ; do
;;    (user-callback
;;      (unwrap wrapped-foo-kw)
;;      (meta wrapped-foo-kw))
;;    ```
;;
;;    Idea: One could even cheat and overload `cc/meta` to look for a a value in
;;    dynamic scope.
;;
;; ** Don't assume information will be available ⑥
;;    Clojure read is destructive. One example is keyword alias expansion: it
;;    happens at read time. At compile time (macro time) the original keyword form
;;    (the one made of chars in source file on disk) is already gone, and we can't
;;    tell if the keywords was fully qualified or aliased. ns-alias is available,
;;    sure, but the presence of a matching alias doesn't necessarily mean the
;;    keyword was aliased. It could have been fully qualified.
;;
;; ** Always quote (meta &form) ⑦
;;    You can never know what's in there, libraries and devtools may mess with it,
;;    and it's never meant to be evaluated (e.g. :tag, or :arglists).
;;
;; ** Avoid large macros, use many small ones ⑧
;;    This advice is specific to HFQL. Avoid large macros taking one form and doing
;;    all transforms at once. Instead use many small macros so to collect as much
;;    &form instances as possible, and so augment source knowledge. You'll get more
;;    granular source mapping, less coupling and better testing. Remember
;;    macroexpand is a pre-walk: having a top-level macro sprinkle small ones over
;;    the form won't produce source mapping metadata on the sprinkled &forms.
;;
;; ** Don't use dynamic scope with lazy traversals ⑨
;;    Instead macroexpand to lexical scopes and close over it.


(ns hyperfiddle.hfql1
  (:refer-clojure :exclude [resolve eval ; ② ③
                            meta unquote replace next find concat count merge *])
  (:require [hyperfiddle.rcf :as rcf :refer [tests]]
            [clojure.core :as cc]
            [clojure.string :as str]
            [clojure.datafy :as datafy]
            [clojure.core.protocols :as ccp]
            [contrib.data :as data]
            #?(:clj [clojure.tools.logging :as log]))
  #?(:cljs (:require-macros [hyperfiddle.hfql1 :refer [hfql edge]])))

(defn- has-meta? [object]
  #?(:clj (instance? clojure.lang.IObj object)
     :cljs (satisfies? IMeta object)))

(defn- meta [object]
  (when (has-meta? object)
    (clojure.core/meta object)))

(defn form [edge] (::form (meta edge)))
(defn short-form [edge] (or (::short-form (meta edge)) (form edge)))
(defn resolved-form [edge] (::resolved-form (meta edge) (form edge)))
(defn value [edge] (::value (meta edge)))

(def ^:private edge-types (-> (make-hierarchy) ; Experiment, I probably won't keep this
                            (derive ::getter ::edge)
                            (derive ::pull ::edge)
                            (derive ::traversal ::edge)))

(defn edge? [object] (isa? edge-types (type object) ::edge))
;; (defn- getter? [object] (isa? edge-types (type object) ::getter))
(defn traversal? [edge] (isa? edge-types (type edge) ::traversal))
(defn pull? [edge] (isa? edge-types (type edge) ::pull))
(defn- leaf? [edge] (and (edge? edge)
                      (not (traversal? edge))
                      (not (pull? edge))))

(defn count [edge-or-coll]
  (if (edge? edge-or-coll)
    (cc/count (value edge-or-coll))
    (cc/count edge-or-coll)))

(defn- quoted? [form]
  (and (seq? form) (= 'quote (first form))))

(defn- var-form? [form]
  (and (seq? form)
    (= 'var (first form))
    (= 2 (count form))))

(defn unquote [form] (if (quoted? form) (second form) form))

(defn- quoted-var-form? [form] (and (quoted? form) (var-form? (unquote form))))

(defn edge-name [edge-form]
  (cond
    (map? edge-form) (edge-name (first edge-form))
    (map-entry? edge-form) (key edge-form)
    (seq? edge-form) (first edge-form)
    (ident? edge-form) edge-form
    ;; ?
    :else edge-form))

(defn- replace
  ([replacements form] (replace identity replacements form))
  ([not-found-f replacements form]
   (cond (seq? form) (map (fn [item] (get replacements item (not-found-f item))) form)
         (ident? form) (get replacements form (not-found-f form))
         :else form)))

(defn- with-% [f]
  (fn
    ([] (f nil nil))
    ([object] (f {'% object} object))
    ([env object] (f (assoc env '% object) object))
    ([env object & args] (apply f (assoc env '% object) object args))))

(defn- zip-by
  ([coll-a coll-b] (zip-by (constantly true) coll-a coll-b))
  ([pred coll-a coll-b]
   (let [max-size (max (count coll-a) (count coll-b))
         coll-a (take max-size (cc/concat coll-a (repeat nil)))
         coll-b (take max-size (cc/concat coll-b (repeat nil)))]
     (map (fn [a b] (if (pred a) b a)) coll-a coll-b))))

(tests
  (zip-by [1 2 3] [:a :b :c])
  := '(:a :b :c)

  (zip-by '#{_} '[1 _ 3] [:a :b :c])
  := '(1 :b 3)

  )

(defmulti symbolic-form (fn [form value] (if (or (::symbolic (meta form)) (::symbolic (meta value))) ::symbolic (type value))))
(defmethod symbolic-form ::symbolic [form _value] form)
(defmethod symbolic-form :default [_form value] value)

(defn- add-point-to-call-form [symbolic-call-form]
  (if (seq? symbolic-call-form)
    symbolic-call-form
    (list symbolic-call-form '%)))

(defn- template-call-from-env [lexical-env symbolic-call-form call-form]
  (let [[f & args] (add-point-to-call-form call-form)
        [_symbolic-f & symbolic-args] (add-point-to-call-form symbolic-call-form)
        symbolic-args (map unquote symbolic-args)
        templated-args (replace (constantly ::hole) lexical-env symbolic-args)
        args (zip-by #{::hole} templated-args args)
        args (map symbolic-form symbolic-args args)]
    (cons f args)))

(defn- touch [object] (if (seqable? object) (doall object) object))
(def ^:public * (with-% (fn star [_lexical-env object] (touch object))))
(defn star? [edge-or-sym] (if (edge? edge-or-sym) (star? (form edge-or-sym)) (= '* edge-or-sym)))

(defn star-traversal? [edge] ; splat?
  (and (traversal? edge)
    (star? (ffirst (value edge)))))

(defn- lift-traversal [fn-like _metadata]
  {:pre [(map? fn-like)]}
  (-> (fn traverse [lexical-env object]
        (let [edge-fn (ffirst fn-like)

              traversed (edge-fn lexical-env object)]
          (if (star? edge-fn)
            (let [next-edge-fn (val (first fn-like))]
              (eduction (map next-edge-fn) traversed))
            traversed)))
    (with-%)
    (with-meta {:type ::traversal})))

(defn- next3 [edge]
  (cond
    (traversal? edge) (val (first (value edge)))
    (pull? edge)      (seq (value edge))
    (leaf? edge)      nil
    :else             (throw (ex-info "Unknown edge" {:type (type edge), :edge edge}))))

(defn- lift-traversal2 [traversal _metadata]
  {:pre [(map? traversal)]}
  (-> (fn traverse [lexical-env object]
        (let [[edge-fn continuation] (first traversal) ; we assume all traversal have been normalized
              traversed (first (edge-fn lexical-env object))]
          (if (star? edge-fn)
            [(eduction (map continuation) (map first) traversed) ; eductions composes without realizing sequences
             (next3 continuation)]
            [traversed continuation])))
    (with-%)
    (with-meta {:type ::traversal})))


(defn safe-ccp-nav [object key value]
  (if (satisfies? ccp/Navigable object)
    (datafy/nav object key value)
    value))

(def ^:dynamic *navigate* safe-ccp-nav)

(defn- getter [fn-like] (-> (fn [_lexical-env object] (*navigate* object fn-like (get object fn-like)))
                          (with-%)
                          (with-meta {:type ::getter})))
(defn- caller [fn-like] (with-% (fn [_lexical-env & args] (apply fn-like args))))

(defn- get-or-apply [f object & args]
  ;; {:pre [(ifn? f)]}
  (if (try (contains? object f) ; FIXME find a better check
           (catch #?(:clj Throwable, :cljs :default) _
             (#?(:clj log/debug, :cljs js/console.debug) "object doesn't support `contains?`" (type object))
             false))
    (*navigate* object f (get object f))
    (apply f object args)))

(defn literal? [obj]
  (or (nil? obj)
    (number? obj)
    (string? obj)
    (keyword? obj)))

#_(defn- lift
  ([edge] (lift edge (meta edge)))
  ([edge metadata]
   (let [metadata (-> (meta edge)
                    (cc/merge metadata)
                    (update ::value #(or % edge))
                    (update ::form #(or % (if (literal? edge) edge nil)))
                    (assoc :type ::edge))
         lifted-fn
         (cond
           (star? edge)   *
           (ident? edge)  (getter edge)
           (string? edge) (getter edge)
           (var? edge) (caller edge) ; must wrap, putting meta on a var is a side effect
           (fn? edge) (with-% (fn [_lexical-env object & args]
                                (try (apply get-or-apply edge object args)
                                     (catch #?(:clj Throwable, :cljs :default) err
                                       (#?(:clj log/warn, :cljs js/console.warn) "Failed to apply" (::form metadata) "onto" (cons object args))
                                       err))))
           (var-form? edge) (with-% (fn [_lexical-env object] (*navigate* object (::value metadata) (get object (::value metadata))))) ; quoted var
           (map? edge) (lift-traversal edge metadata)
           (vector? edge) (let [fns edge]
                              (let [keys (map edge-name (::form metadata))
                                    pull (if (empty? fns) (constantly []) (apply juxt fns))]
                                (-> (fn edges [_lexical-env map-object]
                                      (zipmap keys (cc/concat (pull map-object) (repeat nil))))
                                  (with-%)
                                  (with-meta {:type ::pull}))))
           (seq? edge) (with-% (fn call [lexical-env & _args] ; optional
                                 (let [[f & args] (add-point-to-call-form (::value metadata))
                                       args (replace (cc/merge (::lexical-env metadata) lexical-env) args)]
                                   (apply f args))))
           :else (with-% (fn [_lexical-env object & args] (apply get-or-apply edge object args))))
         ]
     (vary-meta lifted-fn #(cc/merge metadata %)))))

(defn- leaf [f]
  (with-meta
    (fn leaf [& args]
      [(apply f args) nil])
    (meta f)))

(reduce conj (array-map) (map vector (range 9) (map str "abcdefghijklmnop")))

(defn- ordered-map
  "A simple, naive ordered map impl preserving order of keys given at creation
  time, and behaving as a sorted-map on keys assoced after creation.
  Performances aside, one can always re-build the map to freeze the keyset
  order."
  [& keyvals]
  (let [order (into {} (comp (take-nth 2) (map-indexed #(vector %2 [%1 %2])))
                keyvals)
        max-static-key (count order)
        position #(get order % [max-static-key %])]
    (into (sorted-map-by #(compare (position %1) (position %2)))
      (map vec) ; odd but required: partition returns pairs as lazy seqs and `conj` on a map expects vector-pairs.
      (partition 2 keyvals))))

(tests
  (keys (conj (apply ordered-map
                (interleave
                  (map str "abcdefghijklmnop")
                  (range 9) ; larger than clojure.lang.PersistentArrayMap/HASHTABLE_THRESHOLD
                  ))
         [:z 'whatever]
         [:y 'whatever]))
   := '("a" "b" "c" "d" "e" "f" "g" "h" "i" :y :z)
  )

(defn- lift-pull [edges metadata]
  (let [keys (map edge-name (::form metadata))
        pull-fns (map #(comp first %) edges)
        pull (if (empty? edges)
               (constantly [])
               (apply juxt pull-fns))]
    (-> (fn edges [_lexical-env map-object]
          (let [pulled (pull map-object)]
            [(apply ordered-map (interleave keys (cc/concat pulled (repeat nil))))
             (seq (::value metadata))]))
      (with-%)
      (with-meta {:type ::pull}))))

(defn lift2
  ([edge] (lift2 edge (meta edge)))
  ([edge metadata]
   (let [metadata (-> (meta edge)
                    (cc/merge metadata)
                    (update ::value #(or % edge))
                    (update ::form #(or % (if (literal? edge) edge nil)))
                    (assoc :type ::edge))
         lifted-fn
         (cond
           (star? edge)   (leaf *)
           (ident? edge)  (leaf (getter edge))
           (string? edge) (leaf (getter edge))
           (var? edge)    (leaf (caller edge)) ; must wrap, putting meta on a var is a side effect
           (fn? edge) (leaf (with-% (fn [_lexical-env object & args]
                                      (try (apply get-or-apply edge object args)
                                           (catch #?(:clj Throwable, :cljs :default) err
                                             (#?(:clj log/warn, :cljs js/console.warn) "Failed to apply" (::form metadata) "onto" (cons object args))
                                             err)))))
           (var-form? edge) (leaf (with-% (fn [_lexical-env object]
                                            (*navigate* object (::value metadata) (get object (::value metadata)))))) ; quoted var
           (map? edge) (lift-traversal2 edge metadata)
           (vector? edge) (lift-pull edge metadata)
           (seq? edge) (leaf (with-% (fn call [lexical-env & _args] ; optional
                                       (let [[f & args] (add-point-to-call-form (::value metadata))
                                             args (replace (cc/merge (::lexical-env metadata) lexical-env) args)]
                                         (apply f args)))))
           :else (leaf (with-% (fn [_lexical-env object & args] (apply get-or-apply edge object args)))))
         ]
     (vary-meta lifted-fn #(cc/merge metadata %)))))

(def lift lift2)

#?(:clj
   (defmethod print-method ::edge [object writer]
     (.write writer (str "#" `hfql "[" (short-form object) "]"))))


;; (pr-str (hfql {identity [:hello]}))

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
        (= arg-sym (second last-form))
        #_(= 2 (count last-form))))))

(defn- get-kw-fn-form-kw [kw-fn-form]
  (when (is-kw-fn? kw-fn-form)
    (let [[_fn* _args & body] kw-fn-form]
      (first (last-form body)))))

;; ----------------------

(defn- edge-raw-form [edge]
  (cond (quoted-var-form? edge) edge ; this trick/special case prevents a later call to `resolve`
        (is-kw-fn? edge) (get-kw-fn-form-kw edge)
        :else (::form (meta edge) edge)))

(defn- edge-raw-value [edge]
  (cond (quoted-var-form? edge) (unquote edge)  ; this trick/special case prevents a later call to `resolve`
        :else edge))

(defn symbolic-call? [form]
  (and (seq? form)
    (let [[f & _args] form]
      (and (symbol? f) (not (#{'fn `fn 'fn* 'quote 'var 'hfql `hfql} f))))))

(defn- escape-calls [form]
  (if (symbolic-call? form)
    `(list ~@form)
    form))

(defn- wrap-hfql [form]
  (if (and (seq? form) (#{'hfql `hfql} (first form)))
    form
    (list `hfql form)))

(defn- escape-traversals [form]
  (if (map? form)
    (-> form
      (update-keys wrap-hfql #_#(list `hfql %))
      (update-vals wrap-hfql #_#(list `hfql %)))
    form))

(defn- lexical-bindings-map [lexical-env]
  (zipmap (map #(list 'quote %) (keys lexical-env))
    (keys lexical-env)))

(defn- escape-star [form] (if (star? form) ''* form))

(defn- quote "Quote a form. Invoke it as `(#'quote x)` to avoid conflict with the 'quote special form."
  [form]
  (if (or (literal? form) (quoted? form))
    form
    (list 'quote form)))

(defn- emit-merge [& maps]
  (let [non-empties (filter (comp not-empty unquote) maps)]
    (case (count non-empties)
      0 nil
      1 (first non-empties)
      (cons `cc/merge non-empties))))

(defn- filter-kv [pred map] (reduce-kv (fn [r k v] (if (pred k v) r (dissoc r k))) map map))
(defn- remove-kv [pred map] (filter-kv (complement pred) map))

(defn- prune-metamap [map]
  (remove-kv (fn [k v]
               (case k
                 ::lexical-env (empty? v)
                 (nil? v)))
    map))

(defn- vary-meta* [obj f & args]
  (if-not (has-meta? obj)
    obj
    (apply vary-meta obj f args)))

(defn- indirect-method-call? [sym]
  (and (simple-symbol? sym)
    (str/starts-with? (name sym) ".")))

(defn- direct-method-call? [sym]
  (and (qualified-symbol? sym)
    (str/starts-with? (name sym) ".")))

#?(:clj
    (defn- supports-direct-method-syntax? []
      (>= 1.12 (parse-double (re-find #"^[0-9]+\.[0-9]+" (clojure-version))))))

#?(:clj (defn- escape-method-calls [form]
          (cond
            (indirect-method-call? form) `#(~form %)

            (and (direct-method-call? form) (not (supports-direct-method-syntax?)))
            , (escape-method-calls (symbol (name form)))

            :else form)))

(defmacro resolve-direct-method [sym] ; No call to resolve, respects ②
  {:pre [(direct-method-call? sym)]}
  `(symbol (.getName ~(symbol (namespace sym))) ~(name sym)))

;; (hfql (identity '%))
(defmacro edge [fn-like-form & [metadata]]
  (let [raw-form      (edge-raw-form fn-like-form) ; ②
        form          (#'quote raw-form)
        short-form    (#'quote (attempt-shorten-form raw-form))
        lexical-env   (lexical-bindings-map &env)
        ;; _ (prn 'lexical {'&env &env, 'map lexical-env})
        lexical?      #(do #_(prn 'lexical? % (contains? &env %)) (contains? &env %)) ;; (contains? lexical-env %)
        resolved-form (cond
                        (lexical? fn-like-form) form
                        (indirect-method-call? fn-like-form) nil
                        (direct-method-call? fn-like-form) `(resolve-direct-method ~fn-like-form)
                        (star? fn-like-form)   `(symbol (var *))
                        (symbol? fn-like-form) `(symbol (var ~fn-like-form))
                        (symbolic-call? fn-like-form) `(list* ~(if (lexical? (first fn-like-form))
                                                                form
                                                                `(symbol (var ~(first fn-like-form)))) 
                                                         '~(rest fn-like-form))
                        :else nil)
        resolved-var (cond
                       (lexical? fn-like-form) nil
                       (indirect-method-call? fn-like-form) nil
                       (direct-method-call? fn-like-form) nil
                       (star? fn-like-form)   `(var *)
                       (symbol? fn-like-form) `(var ~fn-like-form)
                       (symbolic-call? fn-like-form) (if (lexical? (first fn-like-form))
                                                       nil
                                                       `(list* (var ~(first fn-like-form)) '~(rest fn-like-form)))
                       :else nil)
        value         (as-> fn-like-form %                   ; ②
                        (edge-raw-value %)
                        (escape-method-calls %)
                        (escape-traversals %)
                        (escape-calls %)
                        ({'* `*} % %))
        symbolic-form (-> fn-like-form
                        (escape-method-calls)
                        (escape-traversals)
                        (escape-calls)
                        (escape-star)
                        (vary-meta* (comp not-empty dissoc) ::form))
        value         (if (= value symbolic-form) nil value)
        metadata      (emit-merge
                        (#'quote (meta &form))                                     ; ⑦
                        (prune-metamap
                          (cc/merge metadata
                            {::form        (if (and (literal? form) (not (is-kw-fn? fn-like-form))) nil form) ; no need to repeat it
                             ::short-form  short-form
                             ::resolved-form resolved-form
                             ::resolved-var resolved-var
                             ::value       value
                             ::lexical-env lexical-env}))
                        (#'quote (dissoc (meta fn-like-form) ::form)) ; ⑦
                        )]
    (if (not-empty metadata)
      `(lift2 ~symbolic-form ~metadata)
      `(lift2 ~symbolic-form))))



(defmacro hfql [pull-expr & [metadata]]
  (cond
    ;; (and (seqable? pull-expr) (empty? pull-expr)) `(with-meta (constantly {}) ~(emit-merge {::form (#'quote pull-expr) :type ::edge} metadata))
    (vector? pull-expr)
    `(edge ~(with-meta
              (mapv (fn [fn-like-form] `(edge ~fn-like-form)) pull-expr) ; ⑧
              (cc/merge
                (meta pull-expr)
                {::form (list 'quote (edge-raw-form pull-expr))}
                metadata)))
    :else (if (not-empty metadata)
            `(edge ~pull-expr ~metadata)
            `(edge ~pull-expr))))

(defn next [edge-fn]
  (if-let [edge-fns (value edge-fn)]
    (cond
      (map? edge-fns) (vals edge-fns)
      (vector? edge-fns) (seq edge-fns)
      :else ())
    ()))

(tests
  (next nil) := ()
  (next (hfql 1)) := ()
  (next (hfql [:a])) := [_]
  (next (hfql {identity [:a]}) ) := [_]
  )

(defn next2 [edge]
  (cond
    (star-traversal? edge) (let [next-edge (val (first (value edge)))]
                             (if (pull? next-edge) next-edge (next2 next-edge)))
    (traversal? edge) (val (first (value edge)))
    (pull? edge) (seq (value edge))
    :else nil))

(tests ; TODO port to CofreeT
  (form (hfql #(:bar %))) := :bar
  (:foo (meta (hfql ^:foo #(:bar %)))) := true
  )





(defn- call [f & args] (apply f args))

#?(:clj (defn is-array? [x]
          {:post [(boolean? %)]}
          (boolean (some-> x class .isArray))))

(defn- supports-nth? [object] ; from clojure.lang.RT
  #?(:clj (or (instance? clojure.lang.Indexed object)
            (nil? object)
            (instance? CharSequence object)
            (is-array? object)
            (instance? java.util.RandomAccess object)
            (instance? java.util.regex.Matcher object)
            (map-entry? object)
            (sequential? object)
            )
     :cljs (or (nil? object) ; from cljs.core
             (implements? IIndexed object)
             (array? object)
             (string? object)
             (implements? ISeq object)
             (implements? ISequential object)
             (native-satisfies? IIndexed object))))

(defn- infer-nav-value [object key value] ; convenience
  (if (and (some? key) (some? object) (or (nil? value) (= object value)))
    (try
      (cond
        (associative? object)  (get object key value)
        (supports-nth? object) (nth object key value)
        :else value)
      (catch #?(:clj Throwable, :cljs :default) _ value))
    value))

(defn- left "Variadic opposite of `{}`." [a & _] a) ; almost the K combinator
(defn- right "Variadic `{}`." [& args] (last args)) ; almost (K I)

(defn- nav2
  ([edge] (nav2 nil edge))
  ([object edge]
   (nav2 nil object edge (infer-nav-value object edge nil)))
  ([object edge value] (nav2 {'% object} object edge value))
  ([env object edge value]
   (if-not (edge? edge)
     (safe-ccp-nav object edge value) ; REPL convenience
     (if (leaf? edge)
       (binding [*navigate* right]
         (first (call edge (assoc env '% object) object)))
       (first (call edge (assoc env '% object) object))))))



(defn nav
  ([edge] (nav nil edge))
  ([object edge]
   (nav nil object edge (infer-nav-value object edge nil)))
  ([object edge value] (nav nil object edge value))
  ([env object edge value]
   (nav2 env object edge value)
   #_(first (nav* env object edge value))))

(defn pull3
  ([edge] (pull3 edge nil))
  ([edge object] (pull3 edge object {'% object}))
  ([edge object env]
   (cond
     (leaf? edge) (nav env object edge object)
     (pull? edge) (into {} (map #(vector (edge-name (form %)) (pull3 % object env))) (value edge))
     (star-traversal? edge) (let [[objects edges] (call edge env object)]
                              (doall (for [object objects]
                                       (if (seq? edges)
                                         (into {}
                                           (for [edge edges]
                                             [(edge-name (form edge)) (pull3 edge object (assoc env '% object))]))
                                         (pull3 edges object (assoc env '% object))))))
     :else
     {(edge-name (form edge))
      (let [[object continuation] (call edge env object)]
        (pull3 continuation object (assoc env '% object)))})))





(declare pull)

(defn- multiplicity-many? [object]
  (or (and (sequential? object)         ; lists, vectors, lazy seqs, ranges
        (not (map-entry? object)))      ; don't treat map-entries as collections of size 2
    (set? object)                       ; typical Datomic :many materialization
    #?(:clj (instance? java.util.Iterator object)) ; raw Java iterators
    (and (seqable? object)              ; arrays, Iterable, eduction, etc.
      (not (associative? object))       ; reject maps, records, EntityMap
      (not (map-entry? object))         ; don't treat map-entries as collections of size 2
      (not (string? object))            ; treat strings as scalars
      ;; NOTE nth supports java.util.regex.Matcher but Matches is not seqable?. So no need to account for it here.
      )))

(tests
  (multiplicity-many? (data/map-entry 1 2)) := false
  (multiplicity-many? "foo") := false
  )

(defn- traverse [object edge env]
  {:pre [(traversal? edge)]}
  ;; (prn `traverse edge object)
  (if (and (multiplicity-many? object) (pull? (first (next edge))))
    (->> object
      (map (fn [collection-element] (traverse (datafy/nav object edge collection-element) edge env #_(assoc env '% object))))
      (reduce (fn [r traversed] (merge-with (fn [a b] (if (vector? a) (conj a b) [a b])) r traversed)) {}))
    (reduce-kv (fn [r edge next-edge]
                 (let [[f & args] (template-call-from-env env (form edge) (value edge))
                       [symbolic-f & _symbolic-args] (add-point-to-call-form (form edge))]
                   (assoc r (if (seq? (form edge)) (cons symbolic-f args) (form edge))
                     (pull next-edge object (assoc env '% object))))) {} (value edge))))

(defn- alist [& keyvals]
  (assert (even? (count keyvals)))
  (with-meta
    (mapv data/map-entry (take-nth 2 keyvals) (take-nth 2 (rest keyvals)))
    {:type ::alist}))

(defn- alist->map [object]
  (if (= ::alist (type object))
    (with-meta (into {} object) (assoc (meta object) ::alist object))
    object))

(defn pull "Eagerly navigates, realizing the entire pull expression." ; ①
  ([edge] (pull edge nil))
  ([edge object] (pull edge object {'% object}))
  ([edge object env]
   (assert (edge? edge) "The given pull expression must be pre-processed with `(hfql <pull-expr>)`.")
   (pull3 edge object env)
   #_(try
     (let [env (assoc env '% object)
           do-nav #(nav env object edge nil)]
       (cond
         (star-traversal? edge) (pull (first (next edge)) (do-nav) env) ;; (map #(pull (first (next edge)) % env) (do-nav))
         (traversal? edge) (traverse (do-nav) edge env)
         (pull? edge) (alist->map (reduce (fn [r edge]
                                            (let [pulled (pull edge object env)]
                                              (cond
                                                (traversal? edge) (into r pulled)
                                                (star? edge) (into r pulled)
                                                :else (conj r (data/map-entry (edge-name (form edge)) pulled)))))
                                    (alist) (value edge)))
         :else (do-nav)))
     (catch #?(:clj Throwable, :cljs :default) ex
       (prn ex)
       (ex-info (ex-message ex) {})))))

(comment
  (pull (hfql {* {val [:a]}}) [(data/map-entry :k {:a 1 :b 2})])
  )

(defn pull2
  ([edge object] (pull2 edge object {'% object}))
  ([edge object env]
   (cond
     (traversal? edge) {(edge-name (form edge)) (pull2 (first (next edge)) (nav env object edge nil))}
     (pull? edge)
     (let [naved (nav env object edge nil)]
       )
     :else
     (nav env object edge nil))
   #_   (let [navigated (nav env object edge nil)]
          (if (leaf? edge)
            navigated
            ::todo)
          #_(if-some [next-edges (seq (next edge))]
              {(edge-name (form edge))
               (reduce (fn [r edge] (assoc r (edge-name (form edge)) (pull2 edge (get navigated (edge-name (form edge)))))) {} next-edges)}
              navigated
              ))
   ))

(defn pull-leaf
  "Pull and prune intermediate traversal from pulled result.
  E.g., in {identity {:a :db/ident}}, return the value for :db/ident, skipping identity and :a."
  [object edge]
  (cond
    (traversal? edge) (pull-leaf (nav object edge) (first (next edge)))
    (pull? edge)      (pull edge   object)
    :else             (nav  object edge)))

(tests ; TODO port to CofreeT
  (hfql (identity 1)) := _

  ((hfql (identity 1)))
  := [1 nil]
  ((hfql (identity '%)) "hello")
  := ["hello" nil]
  ((hfql (str "value is: " '%)) 'foo)
  := ["value is: foo" nil]

  ((hfql (str "value is: " '%)) {'% 'foo} 'bar)
  := ["value is: bar" nil]

  ((let [x 1] (hfql (identity x))))
  := [1 nil]

  ((let [x 2] (hfql (vector 1 x 3))))
  := [[1 2 3] nil]

  ((let [x 2] (hfql (vector 1 x 3 'y))) {'y 4} nil)
  := [[1 2 3 4] nil]

  ((let [x 2] (hfql (vector 1 x 3 'y '%))) {'y 4} 5)
  := [[1 2 3 4 5] nil]
  )


(comment "props"
  (hfql {identity (hfql [] {::select '(identity %)})})
  (edge {identity (hfql [] {:hyperfiddle.hfql1/select (identity '%)})})
  )

(defn- arglists [edge]
  (some-> edge meta ::resolved-var meta :arglists))

(defn context-free-edge? [edge]
  (let [form (form edge)]
    (cond
      (seq? form)
      (let [[f & args] form]
        (if (and (some? f) (empty? args))
          true
          (let [lexical-env (::lexical-env (meta edge))]
            (every? #(or (literal? %) (contains? lexical-env (unquote %))) args))))

      (ident? form)
      (if-let [symbolic-arities (arglists edge)]
        (= 0 (apply min (map cc/count symbolic-arities)))
        false)

      :else false)
    #_(cond
      (ident? form) true
      (seq? form) (let [[f & args] form]
                    (if (and (some? f) (empty? args))
                      true
                      (let [lexical-env (::lexical-env (meta edge))]
                        (every? #(or (literal? %) (contains? lexical-env (unquote %))) args))))
      :else false)))

(defn context-free-edges [edge]
  (if (traversal? edge)
    (filter context-free-edge? (keys (value edge)))
    ()))


(defn find-by [pred edge-fn]
  (when (map? (form edge-fn))
    (when-let [found (first (filter (fn [traversal] (pred (form traversal))) (keys (value edge-fn))))]
      (first {found (get (value edge-fn) found)}))))

(defn- normalize-traversal-lookup [symbolic-call-form]
  (let [symbolic-call-form (unquote symbolic-call-form)]
    (cond (seq? symbolic-call-form) symbolic-call-form
          (ident? symbolic-call-form) (list symbolic-call-form '%)
          :else symbolic-call-form)))

(defn find-traversal
  [edge-fn symbolic-call-form]
  (when (some? symbolic-call-form)
    (when (map? (form edge-fn))
      (let [[f & args] (seq (normalize-traversal-lookup (unquote symbolic-call-form)))
            argc (count args)]
        (or
          ;; look for exact symbolic match
          (first (filter-kv (fn [k v] (= symbolic-call-form (form k))) (value edge-fn)))

          ;; look for same symbolic fn and arg count match
          (first (filter-kv (fn [k v] (let [k-form (form k)]
                                        (and (seq? k-form)
                                          (or (= f (first k-form)) (= f (edge-name (resolved-form k))))
                                          (= argc (count (rest k-form)))))) (value edge-fn)))
          ;; look for point free call
          (first (filter-kv (fn [k v]
                              (and (or (= f (form k))
                                     (= f (resolved-form k)))
                                (or (= 0 argc)
                                  (= 1 argc))))
                   (value edge-fn)))
          )))))


(comment
  (def _lookup_sitemap (hfql {identity []
                              (identity) []
                              (identity '%) []
                              (str 'a 'b) []}))
  ;; (find-traversal _lookup_sitemap 'identity)
  (find-traversal _lookup_sitemap '(identity))
  (find-traversal _lookup_sitemap '(identity 1))
  (find-traversal _lookup_sitemap '(str))
  (find-traversal _lookup_sitemap '(str "a" "b"))
  (find-traversal _lookup_sitemap ''(identity))
  )

#_
(defn- distinct-by
  "Returns a lazy sequence of the elements of coll with duplicates removed by key function kf.
  Returns a stateful transducer when no collection is provided.
  (distinct-by identity) ≣ (clojure.core/distinct)."
  ([kf]
   (fn [rf]
     (let [seen (volatile! #{})]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [key (kf input)]
            (if (contains? @seen key)
              result
              (do (vswap! seen conj key)
                  (rf result input)))))))))
  ([kf coll]
   (sequence (distinct-by kf) coll)))

#_
(tests
  (distinct-by identity [1 2 1 2 3]) := '(1 2 3)
  (distinct-by :key [{:key 1}, {:key 2}, {:key 1} {:key 3}]) := '({:key 1} {:key 2} {:key 3})
  )

(defn concat [pull-expr-A pull-expr-B]
  (assert (and (edge? pull-expr-A) (edge? pull-expr-B)))
  (assert (vector? (value pull-expr-A)) (str "Can only concat two pull expressions (i.e. vectors). Given " (form pull-expr-A)))
  (assert (vector? (value pull-expr-B)) (str "Can only concat two pull expressions (i.e. vectors). Given " (form pull-expr-B)))
  (lift2 (into (value pull-expr-A) (value pull-expr-B))
    (-> (meta pull-expr-A)
      (cc/merge (meta pull-expr-B))
      (assoc ::lexical-env (cc/merge (::lexical-env (meta pull-expr-A)) (::lexical-env (meta pull-expr-B))))
      (assoc ::form (into (form pull-expr-A) (form pull-expr-B)))
      (dissoc ::value))))

(defn merge
  ([traversal] traversal)
  ([traversalA traversalB]
   (assert (traversal? traversalA) (str "Can only merge two traversal expressions (i.e. maps). Given " (form traversalA)))
   (assert (traversal? traversalB) (str "Can only merge two traversal expressions (i.e. maps). Given " (form traversalB)))
   (lift2 (cc/merge (value traversalA) (value traversalB))
     (-> (meta traversalA)
       (cc/merge (meta traversalB))
       (assoc ::lexical-env (cc/merge (::lexical-env (meta traversalA)) (::lexical-env (meta traversalB))))
       (assoc ::form (cc/merge (form traversalA) (form traversalB)))
       (dissoc ::value))))
  ([traversalA traversalB & traversals]
   (reduce merge (merge traversalA traversalB) traversals)))

(defn build-pull [edges & [metadata]]
  (assert (every? edge? edges) (str "Not an edge" (first (remove edge? edges))))
  (lift2 (vec edges)
    (cc/merge metadata
      {::lexical-env (reduce cc/merge {} (map ::lexical-env (map meta edges)))
       ::form (vec (map form edges))})))

(defn- map-pull [f pull-expr]
  (let [mapped (filter some? (map f (value pull-expr)))]
    (vary-meta (build-pull mapped (meta pull-expr)) assoc ::form (mapv form mapped), ::value (vec mapped))))

(defn filter-pull [pred pull-expr]
  (map-pull (fn [edge] (when (pred edge) edge)) pull-expr))

(defn pull-has-star? [pull-edge] (some? (seq (value (filter-pull star? pull-edge)))))

;;;;;;;;;;;;;;;
;; Protocols ;;
;;;;;;;;;;;;;;;


(defprotocol Identifiable :extend-via-metadata true
  (-identify [o])) ; local-symbolize

(extend-protocol Identifiable
  #?(:clj Object, :cljs default) (-identify [_])
  nil (-identify [_]))

(defn identifiable [identify-fn object]
  (vary-meta object assoc `-identify (#?(:clj bound-fn*, :cljs identity) identify-fn))) ; #?(:clj bound-fn* :cljs identity)

(defn identify [obj] "
Produces a symbolic identity for `obj`, or nil if `obj` does not explicitly
implement `Identifiable`. Serializable and uniquely resolvable symbolic
identities should be favored.
All values being self-identical, `(or (identify x) x)` always yields a valid
identifier, though it might not be serializable."
  (-identify obj))

(comment
  (identify (Object.)) := nil
  (let [obj (Object.)] (pr-str (or (identify obj) obj))) := "#object[java.lang.Object 0x75bf525d \"java.lang.Object@75bf525d\"]"
  (identify nil) := nil
  (identify (identifiable (constantly `hash-map) {})) := `hash-map)

(defn identifiable? [object]
  #_(extends? Identifiable (type object))
  #?(:clj (extends? Identifiable object)
     :cljs (and (not (nil? object))
             (object? object)
             (satisfies? Identifiable object))))


(defprotocol Suggestable :extend-via-metadata true
             (-suggest [o]))

(extend-protocol Suggestable
  #?(:clj clojure.lang.IPersistentMap, :cljs IMap) ; not sure if IMap is the right protocol, this is assuming all maps are seqable
  (-suggest [m] (keys m) #_(build-pull (map lift (keys m))))
  #?(:clj Object, :cljs default)
  (-suggest [_] #_(hfql) [identity identify type])
  nil (-suggest [_]))

#?(:clj (extend-type Class
          Suggestable (-suggest [_] (hfql [Class/.getCanonicalName Class/.getMethods Class/.getFields]))))


(defmulti suggest type)
(defmethod suggest :default [object] (-suggest object))
(defmethod suggest #?(:clj clojure.lang.IRecord, :cljs IRecord) [record]
  (build-pull
    (map lift2
      (map keyword
        #?(:clj
           (-> (type record)
             (.getMethod "getBasis" nil)
             (.invoke nil nil))
           :cljs
           (.getBasis (type record)))))))

(def ^:dynamic *hierarchy* #?(:clj @#'clojure.core/global-hierarchy :cljs (cljs.core/get-global-hierarchy)))
;; (def ^:dynamic *hierarchy* @#'clojure.core/global-hierarchy)

(defmulti -resolve (fn ([object] (type object)) ([object opts] (type object))) :hierarchy #'*hierarchy*)
(defmethod -resolve :default [_object & _opts] nil)

(defn resolve
  ([object] (-resolve object))
  ([object {::keys [hierarchy] :as opts}]
   (binding [*hierarchy* (or hierarchy *hierarchy*)]
     (-resolve object (dissoc opts ::hierarchy)))))



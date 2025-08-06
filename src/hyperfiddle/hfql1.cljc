;; * Rules of HFQL
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
  (:refer-clojure :exclude [meta unquote replace next find concat count merge])
  (:require #?(:clj [hyperfiddle.hfql0 :as hfql0])
            [hyperfiddle.rcf :as rcf :refer [tests]]
            [clojure.core :as cc]
            [clojure.string :as str]
            [clojure.datafy :as datafy]
            [clojure.core.protocols :as ccp]
            #?(:clj [clojure.tools.logging :as log])
            #_[clojure.spec.alpha :as s])
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
(defn edge? [object] (= ::edge (type object)))

(defn count [edge-or-coll]
  (if (edge? edge-or-coll)
    (cc/count (value edge-or-coll))
    (cc/count edge-or-coll)))

(defn- get-or-apply [f object & args]
  {:pre [(ifn? f)]}
  (if (try (contains? object f)
           (catch #?(:clj Throwable, :cljs :default) _
             (#?(:clj log/debug, :cljs js/console.debug) "object doesn't support `contains?`" (type object))
             false))
    (get object f)
    (apply f object args)))

(defn- quoted? [form]
  (and (seq? form) (= 'quote (first form))))

(defn- var-form? [form]
  (and (seq? form)
    (= 'var (first form))
    (= 2 (count form))))

(defn- unquote [form] (if (quoted? form) (second form) form))

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
    ([env object & args] (apply f env object args))))

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
(defmethod symbolic-form ::symbolic [form value] form)
(defmethod symbolic-form :default [form value] value)

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

(defn- lift-traversal [fn-like metadata]
  {:pre [(map? fn-like)]}
  #_(assert (= 1 (count fn-like)) (str "A traversal can only contain one edge. Received: " (keys fn-like) "."))
  (with-% (fn traverse [lexical-env object] ; optional
            (let [[f & args] (add-point-to-call-form (value (ffirst (::value metadata))))
                  args (replace (cc/merge (::lexical-env metadata) lexical-env {'% object}) args)]
              (apply f args)))))

(def ^:private star? #{'*})

(defn- touch [object] (if (seqable? object) (doall object) object))
(def ^:private star (with-% (fn star [_lexical-env object] (touch object))))
(defn- getter [fn-like] (with-% (fn [_lexical-env object] (get object fn-like))))
(defn- caller [fn-like] (with-% (fn [_lexical-env & args] (apply fn-like args))))

(defn litteral? [obj]
  (or (nil? obj)
    (number? obj)
    (string? obj)
    (keyword? obj)))

(defn lift
  ([edge] (lift edge (meta edge)))
  ([edge metadata]
   (let [metadata (-> metadata
                    (update ::value #(or % edge))
                    (update ::form #(or % (if (litteral? edge) edge nil)))
                    (assoc :type ::edge))
         lifted-fn
         (cond
           (star? edge)  star
           (ident? edge)  (getter edge)
           (string? edge) (getter edge)
           (var? edge) (caller edge) ; must wrap, putting meta on a var is a side effect
           (fn? edge) (with-% (fn [_lexical-env object & args]
                                (try (apply get-or-apply edge object args)
                                     (catch #?(:clj Throwable, :cljs :default) err
                                       (#?(:clj log/warn, :cljs js/console.warn) "Failed to apply" (::form metadata) "onto" (cons object args))
                                       err))))
           (var-form? edge) (with-% (fn [_lexical-env object] (get object (::value metadata)))) ; quoted var
           ;; TODO class method calls?
           (map? edge) (lift-traversal edge metadata)
           (vector? edge) (let [fns edge]
                              (if (empty? fns)
                                 (constantly {})
                                 (let [keys (map edge-name (::form metadata))
                                       pull (apply juxt fns)]
                                   (with-% (fn edges [_lexical-env map-object]
                                             (zipmap keys (cc/concat (pull map-object) (repeat nil))))))))
           (seq? edge) (with-% (fn call [lexical-env & _args] ; optional
                                    (let [lexical-env (cc/merge (::lexical-env metadata) lexical-env)
                                          [f & args] (template-call-from-env lexical-env (::form metadata) edge)]
                                      (apply f args))))
           :else (with-% (fn [_lexical-env object & args] (apply get-or-apply edge object args))))
         ]
     (with-meta lifted-fn metadata))))

#?(:clj
   (defmethod print-method ::edge [object writer]
     (.write writer (str "#" `hfql "[" (form object) "]"))))


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

(comment
  (is-kw-fn? '#(do (:foo % :default)))
  (get-kw-fn-form-kw '#(do (prn 1) (:foo % :default)))
  ^:foo #(:bar %)
  )

;; ----------------------

(defn- edge-raw-form [edge]
  (cond (quoted-var-form? edge) edge ; this trick/special case prevents a later call to `resolve`
        (is-kw-fn? edge) (get-kw-fn-form-kw edge)
        :else (::form (meta edge) edge)))

(defn- edge-raw-value [edge]
  (cond (quoted-var-form? edge) (unquote edge)  ; this trick/special case prevents a later call to `resolve`
        :else edge))

(defn- escape-calls [form]
  (if (seq? form)
    (let [[f & _args] form]
      (if (and (symbol? f) (not (#{'fn `fn 'fn* 'quote 'var 'hfql `hfql} f)))
        `(list ~@form)
        form))
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
  (if (or (litteral? form) (quoted? form))
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

#?(:clj
   (defn aliased-form [ns-sym ident]
     (if-not (qualified-ident? ident)
       ident
       (if (= (namespace ident) (name ns-sym))
         (if (keyword? ident)
           (str "::" (name ident))
           (symbol (name ident)))
         (let [ns (find-ns ns-sym)
               aliases (ns-aliases ns)
               ns->alias (zipmap (vals aliases) (keys aliases))
               ns-str->alias-str (-> ns->alias
                                   (update-keys (comp str ns-name))
                                   (update-vals str))
               alias (or (get ns-str->alias-str (namespace ident)) (namespace ident))]
           (if (keyword? ident)
             (str "::" alias "/" (name ident))
             (symbol alias (name ident))))))))

#?(:clj
   (defn- attempt-shorten-form "return aliased form for an ident or nil if form is not an ident or cannot be shortened"
     [form]
     (when (qualified-ident? form)
       (let [short-form (aliased-form (ns-name *ns*) form)]
         (when-not (= form short-form)
           short-form)))))

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
        resolved-form (cond
                        (indirect-method-call? fn-like-form) nil
                        (direct-method-call? fn-like-form) `(resolve-direct-method ~fn-like-form)
                        (symbol? fn-like-form) `(symbol (var ~fn-like-form))
                        :else nil)
        value         (as-> fn-like-form %                   ; ②
                        (edge-raw-value %)
                        (escape-method-calls %)
                        (escape-traversals %)
                        (escape-calls %)
                        ({'* `star} % %))
        lexical-env   (lexical-bindings-map &env)
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
                            {::form        (if (and (litteral? form) (not (is-kw-fn? fn-like-form))) nil form) ; no need to repeat it
                             ::short-form  short-form
                             ::resolved-form resolved-form
                             ::value       value
                             ::lexical-env lexical-env}))
                        (#'quote (dissoc (meta fn-like-form) ::form)) ; ⑦
                        )]
    (if (not-empty metadata)
      `(lift ~symbolic-form ~metadata)
      `(lift ~symbolic-form))))

(tests
  (aliased-form (ns-name *ns*) ::test) := "::test"
  (aliased-form (ns-name *ns*) :hyperfiddle.hfql1/test) := "::test"
  (aliased-form (ns-name *ns*) :clojure.core/test) := "::cc/test"
  (aliased-form (ns-name *ns*) ::cc/test) := "::cc/test"
  (aliased-form (ns-name *ns*) `asdf) := 'asdf
  (aliased-form (ns-name *ns*) 'hyperfiddle.hfql1/asdf) := 'asdf

  (short-form (hfql clojure.core/str)) := 'cc/str
  (short-form (hfql cc/str)) := 'cc/str
  (short-form (hfql :clojure.core/test)) := "::cc/test"
  (short-form (hfql ::cc/test)) := "::cc/test"
  (short-form (hfql ::test))  := "::test"
  )

(defmacro hfql [pull-expr & [metadata]]
  (cond
    ;; (and (seqable? pull-expr) (empty? pull-expr)) `(with-meta (constantly {}) ~(emit-merge {::form (#'quote pull-expr) :type ::edge} metadata))
    (vector? pull-expr)
    `(edge ~(with-meta
              (mapv (fn [fn-like-form] `(edge ~fn-like-form)) pull-expr) ; ⑧
              (cc/merge {::form (list 'quote (edge-raw-form pull-expr))}
                metadata)))
    :else (if (not-empty metadata)
            `(edge ~pull-expr ~metadata)
            `(edge ~pull-expr))))

(comment
  (hfql {identity [:a]})
  (hfql [:a])
  )

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

(tests
  ((hfql :a) {:a 1 :b 2})
  := 1

  ((hfql 'c) {:a 1, :b 2, 'c 3})
  := 3

  ((hfql "d") {:a 1, :b 2, 'c 3, "d" 4})
  := 4

  ((hfql str) {:a 1, :b 2})
  := "{:a 1, :b 2}"
  )

(tests
  ((hfql 'str) {'str 'str, str str, #'str #'str})
  := 'str
  ((hfql str) {'str 'str, str str, #'str #'str})
  := str
  ((hfql #'str) {'str 'str, str str, #'str #'str})
  := "{str str, #function[clojure.core/str] #function[clojure.core/str], #'clojure.core/str #'clojure.core/str}"
  ((hfql '#'str) {'str 'str, str str, #'str #'str})
  := #'clojure.core/str

  ((hfql 'str) {str str, #'str #'str})
  := nil
  ((hfql '#'str) {'str 'str, str str})
   := nil
  )

(tests
  (form (hfql #(:bar %))) := :bar
  (:foo (meta (hfql ^:foo #(:bar %)))) := true
  )

(tests
  ((hfql []) {:a 1})
  := {}

  ((hfql [:a]) {:a 1, :b 2})
  := {:a 1}

  ((hfql [:a :c]) {:a 1, :b 2})
  := {:a 1, :c nil}

  ((hfql [:a :c str]) {:a 1, :b 2})
   := {:a 1, :c nil, 'str "{:a 1, :b 2}"}
   )

(tests
  (let [alice {:db/id 9, :user/email "alice@example.com"}]
    ((hfql {identity [:user/email]}) alice) )
  := {:db/id 9, :user/email "alice@example.com"}
  ;; ① – We only return the edge application result. User can call `next` to nav further.
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

(defn nav ; ①
  ([edge] (nav nil edge))
  ([object edge]
   (nav nil object edge object))
  ([object edge value] (nav nil object edge value))
  ([env object edge value]
   (if (edge? edge)
     (call edge (assoc env '% object) (if (satisfies? ccp/Navigable object) (or (datafy/nav object edge value) object) object))
     (do #?(:clj (log/info `nav edge value object))
         (if (nil? object)
           value
           (datafy/nav object edge (if (or (nil? value) (= object value))
                                     (if (associative? object)
                                       (get object edge value)
                                       (if (and (supports-nth? object) (some? edge))
                                         (nth object edge value)
                                         value))
                                     value)))))))

(tests ; ①
  (nav {:a 1} :a) := 1
  (nav {:a 1} :b ::not-found) := ::not-found
  (nav [:a :b :c] 1) := :b
  (nav [:a :b :c] 3 ::not-found) := ::not-found

  (let [alice {:db/id 9, :user/name "Alice" :user/email "alice@example.com"}
        pull-expression (hfql {identity [:user/name :user/email]})]

    (nav alice pull-expression)
    := alice

    (nav alice (first (next pull-expression)))
    := {:user/name "Alice", :user/email "alice@example.com"}

    (map (partial nav alice) (next (first (next pull-expression))))
    := ["Alice" "alice@example.com"]
    ))

(defn traversal? [pull-expr] (and (edge? pull-expr) (map? (value pull-expr))))
(defn pull? [pull-expr] (and (edge? pull-expr) (vector? (value pull-expr))))

(declare pull)

(defn- multiplicity-many? [object]
  (and (seqable? object)
    (not (map? object))))

(defn- traverse [object edge env]
  {:pre [(traversal? edge)]}
  (prn `traverse edge object)
  (if (and (multiplicity-many? object) (pull? (next edge)))
    (->> object
      (map (fn [collection-element] (traverse (datafy/nav object edge collection-element) edge (assoc env '% object))))
      (reduce (fn [r traversed] (merge-with (fn [a b] (if (vector? a) (conj a b) [a b])) r traversed)) {}))
    (reduce-kv (fn [r edge pull-expr]
                 (let [[f & args] (template-call-from-env env (form edge) (value edge))
                       [symbolic-f & _symbolic-args] (add-point-to-call-form (form edge))]
                   (assoc r (if (seq? (form edge)) (cons symbolic-f args) (form edge))
                     (pull pull-expr object (assoc env '% object))))) {} (value edge))))


(comment
  (let [ns-publics2 (comp vals ns-publics)]
    (pull (hfql {(ns-publics2 '%) [symbol]}) *ns*))
  )

(defn pull "Eagerly navigates, realizing the entire pull expression." ; ①
  ([pull-expr] (pull pull-expr nil))
  ([pull-expr object] (pull pull-expr object {'% object}))
  ([pull-expr object env]
   (assert (edge? pull-expr) "The given pull expression must be prepossessed by `(hfql <pull-expr>)`.")
   (let [env (assoc env '% object)
         do-nav #(nav env object pull-expr nil)]
     (cond
       (traversal? pull-expr) (traverse (do-nav) pull-expr env)
       (pull? pull-expr) (reduce (fn [r pull-expr]
                                   (let [pulled (pull pull-expr object env)]
                                     (cond
                                       (traversal? pull-expr) (into r pulled)
                                       (star? (form pull-expr)) (into r pulled)
                                       :else (assoc r (edge-name (form pull-expr)) pulled)))) {} (value pull-expr))
       :else (do-nav)))))

(tests
  (nav {:a 1} (hfql :a))
  := 1

  (let [alice {:db/id 9, :user/email "alice@example.com"}]
    (nav alice (hfql {identity [:user/email]}))
    := alice)

   (let [alice {:db/id 9, :user/name "Alice", :user/email "alice@example.com"}]
     (pull (hfql {identity [:user/email]}) alice) )
    := {'identity {:user/email "alice@example.com"}}

   (let [ns-publics2 (comp vals ns-publics)]
      (pull (hfql {(ns-publics2 '%) [symbol]}) *ns*))

  )

(tests
  (hfql (identity 1) )

  ((hfql (identity 1)))
  := 1
  ((hfql (identity '%)) "hello")
  := "hello"
  ((hfql (str "value is: " '%)) 'foo)
  := "value is: foo"

  ((hfql (str "value is: " '%)) {'% 'foo} 'bar)
  := "value is: bar"

  ((let [x 1] (hfql (identity x))))
  := 1

  ((let [x 2] (hfql (vector 1 x 3))))
  := [1 2 3]

  ((let [x 2] (hfql (vector 1 x 3 'y))) {'y 4} nil)
  := [1 2 3 4]

  ((let [x 2] (hfql (vector 1 x 3 'y '%))) {'y 4} 5)
   := [1 2 3 4 5]
  )


(defn navigable-indexed [nav-fn collection]
  (vary-meta collection assoc `ccp/nav (#?(:clj bound-fn, :cljs fn) [_coll key value] (nav-fn key value)))) ; #?(:clj bound-fn :cljs fn)

(defn navigable [nav-fn collection] (navigable-indexed (fn [_index value] (nav-fn value)) collection))


(def ^:private _alice_    {:db/id 9, :user/email "alice@example.com", :user/name "Alice" :user/address {:address/number 42}})
(def ^:private _bob_      {:db/id 10, :user/email "bob@example.com", :user/name "Bob"})
(def ^:private _charlie_  {:db/id 11, :user/email "chalie@example.com", :user/name "Charlie"})
(def ^:private _db_       ^::symbolic {9 _alice_, 10 _bob_, 11 _charlie_})
(def ^:private _d-entity_ (fn [db eid] (get db eid)))
(defn- _people_ []
  (->> (list 9 10 11)
    (navigable (partial _d-entity_ _db_) )))

(tests
  (let [user-summary #(str (:user/name %) " – " (:user/email %))
        selected 9
        expr (hfql {(_d-entity_ _db_ selected) [:db/id :user/email user-summary keys clojure.core/keys cc/keys]})]
    (pull expr))
  := {'(_d-entity_ _db_ 9)
      {:db/id 9,
       :user/email "alice@example.com",
       'user-summary "Alice – alice@example.com",
       'keys '(:db/id :user/email :user/name :user/address),
       'clojure.core/keys '(:db/id :user/email :user/name :user/address)
       'cc/keys '(:db/id :user/email :user/name :user/address)}}
  )

(tests
  (let [selected 9]
    (pull (hfql {(_d-entity_ _db_ selected) [:user/name {:user/address [:address/number]}]})))
  := {'(_d-entity_ _db_ 9) {:user/name "Alice", :user/address {:address/number 42}}}
  )

(tests
  (hfql *) := _
  ((hfql *) {:a 1})
   := {:a 1}
  (pull (hfql *) _alice_)
   := _alice_
  (pull (hfql {identity [:db/id *]}) _alice_)
  := {'identity _alice_})


(tests "multiplicity many"

  ((hfql {(_people_) [:user/name]})) := (list 9 10 11)

  (let [pull-expr (hfql {(_people_) [:user/name]})
        people (nav pull-expr)]
    people := (list 9 10 11)
    (for [person-id people
          next-pull-expr (next pull-expr)]
      (nav people next-pull-expr person-id)))
  := [{:user/name "Alice"}
      {:user/name "Bob"}
      {:user/name "Charlie"}]

  (pull (hfql {(_people_) [:user/name]}))
  := {'(_people_) [{:user/name "Alice"}
                   {:user/name "Bob"}
                   {:user/name "Charlie"}]}
  )


(tests
  "keyword aliases"

  (form (hfql :test)) := :test
  (form (hfql ::test)) := :hyperfiddle.hfql1/test
  (form (hfql ::ccp/test)) := :clojure.core.protocols/test
  (aliased-form (ns-name *ns*) ::ccp/test) := "::ccp/test"
  (aliased-form (ns-name *ns*) `ccp/test) := 'ccp/test
  )


(tests
  "props"
  (hfql {identity (hfql [] {::select '(identity %)})})
  (edge {identity (hfql [] {:hyperfiddle.hfql1/select (identity '%)})})
  )

(defn context-free-edge? [edge]
  (let [form (form edge)]
    (cond
      (ident? form) true
      (seq? form) (let [[f & args] form]
                    (if (and (some? f) (empty? args))
                      true
                      (let [lexical-env (::lexical-env (meta edge))]
                        (every? #(or (litteral? %) (contains? lexical-env (unquote %))) args))))
      :else false)))

(defn context-free-edges [edge]
  (let [form (form edge)]
    (if-not (map? form)
      ()
      (filter context-free-edge? (keys (value edge))))))


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
                                          (= f (first k-form))
                                          (= argc (count (rest k-form)))))) (value edge-fn)))
          ;; look for point free call
          (first (filter-kv (fn [k v]
                              (and (= f (form k))
                                (or (= 0 argc)
                                  (= 1 argc))))
                   (value edge-fn)))
          )))))


(tests
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


(defn concat [pull-expr-A pull-expr-B]
  (assert (and (edge? pull-expr-A) (edge? pull-expr-B)))
  (assert (vector? (value pull-expr-A)) (str "Can only concat two pull expressions (i.e. vectors). Given " (form pull-expr-A)))
  (assert (vector? (value pull-expr-B)) (str "Can only concat two pull expressions (i.e. vectors). Given " (form pull-expr-B)))
  (lift (into (value pull-expr-A) (value pull-expr-B))
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
   (lift (cc/merge (value traversalA) (value traversalB))
     (-> (meta traversalA)
       (cc/merge (meta traversalB))
       (assoc ::lexical-env (cc/merge (::lexical-env (meta traversalA)) (::lexical-env (meta traversalB))))
       (assoc ::form (cc/merge (form traversalA) (form traversalB)))
       (dissoc ::value))))
  ([traversalA traversalB & traversals]
   (reduce merge (merge traversalA traversalB) traversals)))

(defn build-pull [edges]
  (assert (every? edge? edges) (str "Not an edge" (first (remove edge? edges))))
  (lift (vec edges)
    {::lexical-env (reduce cc/merge {} (map ::lexical-env (map meta edges)))
     ::form (vec (map form edges))}))

(defn- map-pull [f pull-expr]
  (let [mapped (filter some? (map f (value pull-expr)))]
    (vary-meta (build-pull mapped) assoc ::form (mapv form mapped))))

(defn filter-pull [pred pull-expr]
  (map-pull (fn [edge] (when (pred edge) edge)) pull-expr))


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
  (-suggest [m] (build-pull (map lift (keys m))))
  #?(:clj Object, :cljs default)
  (-suggest [_] (hfql [identity identify type]))
  nil (-suggest [_]))

#?(:clj (extend-type Class
          Suggestable (-suggest [_] (hfql [Class/.getCanonicalName Class/.getMethods Class/.getFields]))))


(defmulti suggest type)
(defmethod suggest :default [object] (or (-suggest object) #?(:clj (hfql0/suggest object))))
(defmethod suggest #?(:clj clojure.lang.IRecord, :cljs IRecord) [record]
  (build-pull
    (map lift
      (map keyword
        #?(:clj
           (-> (type record)
             (.getMethod "getBasis" nil)
             (.invoke nil nil))
           :cljs
           (.getBasis (type record)))))))
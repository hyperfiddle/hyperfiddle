;; * Rules of HFQL
;;
;; ** Don't resolve
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
;; ** Don't eval
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

;;    ```
;;    (defmacro applicative-let [bindings & body]
;;      ; don't do that
;;      `(eval-ordered-let '~bindings '~body)
;;      ; instead do
;;      `(let [~@(toposort bindings)]
;;        ~@body))
;;    ```
;;
;; ** Capture all you can
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
;; ** Pass raw forms to userland
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
;; ** Don't use dynamic scope with lazy traversals
;;    Instead macroexpand to lexical scopes and close over it.
;;
;;
;; ** Don't assume information will be available
;;    Clojure read is destructive. One example is keyword alias expansion: it
;;    happens at read time. At compile time (macro time) the original keyword form
;;    (the one made of chars in source file on disk) is already gone, and we can't
;;    tell if the keywords was fully qualified or aliased. ns-alias is available,
;;    sure, but the presence of a matching alias doesn't necessarily mean the
;;    keyword was aliased. It could have been fully qualified.
;;
;; ** Always quote (meta &form)
;;    You can never know what's in there, libraries and devtools may mess with it,
;;    and it's never meant to be evaluated (e.g. :tag, or :arglists).
;;
;; ** Avoid large macros, use many small ones
;;    This advice is specific to HFQL. Avoid large macros taking one form and doing
;;    all transforms at once. Instead use many small macros so to collect as much
;;    &form instances as possible, and so augment source knowledge. You'll get more
;;    granular source mapping, less coupling and better testing. Remember
;;    macroexpand is a pre-walk: having a top-level macro sprinkle small ones over
;;    the form won't produce source mapping metadata on the sprinkled &forms.

(ns hyperfiddle.hfql1
  (:refer-clojure :exclude [meta unquote])
  (:require #?(:clj [hyperfiddle.hfql0 :as hfql0])
            [clojure.string]
            [hyperfiddle.rcf :as rcf :refer [tests]]
            [clojure.string :as str]
            #?(:clj [clojure.tools.logging :as log])))

(defn- has-meta? [object]
  #?(:clj (instance? clojure.lang.IObj object)
     :cljs (satisfies? IMeta object)))

(defn- meta [object]
  (when (has-meta? object)
    (clojure.core/meta object)))

(defn- form [edge] (::form (meta edge)))
(defn- value [edge] (::value (meta edge)))

(defn- get-or-apply [f object]
  {:pre [(ifn? f)]}
  (if (try (contains? object f) (catch #?(:clj Throwable, :cljs :default) _
                                  (log/debug "object doesn't support `contains?`" (type object))
                                  false))
    (get object f)
    (f object)))

(defn- quoted? [form]
  (and (seq? form) (= 'quote (first form))))

(defn- var-form? [form]
  (and (seq? form)
    (= 'var (first form))
    (= 2 (count form))))

(defn- unquote [form] (if (quoted? form) (second form) form))

(defn- quoted-var-form? [form] (and (quoted? form) (var-form? (unquote form))))

(defn- edge-name [edge-form]
  (cond
    (map? edge-form) (edge-name (first edge-form))
    (map-entry? edge-form) (key edge-form)
    (seq? edge-form) (first edge-form)
    (ident? edge-form) edge-form
    ;; ?
    :else edge-form))

(defn- lift-traversal [fn-like metadata]
  {:pre [(map? fn-like)]}
  (assert (= 1 (count fn-like)) (str "A traversal can only contain one edge. Received: " (keys fn-like) "."))
  (let [[edge-fn edges] (first fn-like) ; only keep first key/val pair
         ]
    (fn traverse [& [lexical-context]] ; optional
      {(replace lexical-context (ffirst (::form metadata)))
       (edges (edge-fn lexical-context))})))

(defn lift
  ([fn-like] (lift fn-like {}))
  ([fn-like metadata]
   (let [lifted-fn
         (cond
           (ident? fn-like) #(get % fn-like)
           (string? fn-like) #(get % fn-like)
           (var? fn-like) #(apply fn-like %&) ; must wrap, putting meta on a var is a side effect
           (fn? fn-like) #(get-or-apply fn-like %)
           (var-form? fn-like) #(get % (::value metadata)) ; quoted var
           ;; TODO class method calls?
           (map? fn-like) (lift-traversal fn-like metadata)
           (vector? fn-like) (let [fns fn-like]
                              (if (empty? fns)
                                 (constantly {})
                                 (let [keys (map edge-name (form fns))
                                       vals (apply juxt fns)]
                                   (fn edges [map] (zipmap keys (concat (vals map) (repeat nil)))))))
           (seq? fn-like) (let [[f & args] fn-like]
                            (fn call [& [lexical-context]] ; optional
                              (apply (value f) (replace lexical-context args))))
           :else #(apply fn-like %&))]
     (with-meta lifted-fn metadata))))

;;;;;;;;;;;;;;;;;;;;;
;; KW special form ;;
;;;;;;;;;;;;;;;;;;;;;

(defn- generated-symbol? [sym]
  {:pre [(symbol? sym)]}
  (and (simple-symbol? sym)
    (clojure.string/ends-with? (name sym) "#")))

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
        :else (list 'quote edge)))

(defn- edge-raw-value [edge]
  (cond (quoted-var-form? edge) (unquote edge)  ; this trick/special case prevents a later call to `resolve`
        :else edge))

(defn- escape-calls [form]
  (if (seq? form)
    (let [[f & args] form]
      (if (and (symbol? f) (not ('#{fn fn* quote var} f)))
        `(list* (vary-meta ~f assoc ::value ~f) ~(vec args))
        form))
    form))

(def ^:private traversal? map?)
(defn- escape-traversals [form]
  (if (traversal? form)
    (-> form
      (update-keys #(list `edge %))
      (update-vals #(list `pull-expr %)))
    form))

(defmacro edge [fn-like-form]
  (def _env &env)
  `(lift ~(escape-calls (escape-traversals fn-like-form))
     ~(merge (meta &form)
        {::form (edge-raw-form fn-like-form)
         ::value (escape-calls (escape-traversals (edge-raw-value fn-like-form)))}
        (meta fn-like-form))))


(tests
  ((edge :a) {:a 1 :b 2})
  := 1

  ((edge 'c) {:a 1, :b 2, 'c 3})
  := 3

  ((edge "d") {:a 1, :b 2, 'c 3, "d" 4})
  := 4

  ((edge str) {:a 1, :b 2})
  := "{:a 1, :b 2}"
  )


(tests
  ((edge 'str) {'str 'str, str str, #'str #'str})
  := 'str
  ((edge str) {'str 'str, str str, #'str #'str})
  := str
  ((edge #'str) {'str 'str, str str, #'str #'str})
  := "{str str, #function[clojure.core/str] #function[clojure.core/str], #'clojure.core/str #'clojure.core/str}"
  ((edge '#'str) {'str 'str, str str, #'str #'str})
  := #'clojure.core/str

  ((edge 'str) {str str, #'str #'str})
  := nil
  ((edge '#'str) {'str 'str, str str})
   := nil
  )


(tests
  (form (edge #(:bar %))) := :bar
  (:foo (meta (edge ^:foo #(:bar %)))) := true
  )

(defmacro pull-expr [pull-expr]
  (cond
    (empty? pull-expr) `(constantly {})
    (vector? pull-expr)
    `(edge ~(with-meta (mapv (fn [fn-like-form] `(edge ~fn-like-form)) pull-expr)
              {::form (edge-raw-form pull-expr)})
       #_~(merge (meta pull-expr) (meta &form) {::form (list 'quote pull-expr)}))
    :else `(edge ~pull-expr)))

(tests
  ((pull-expr []) {:a 1})
  := {}

  ((pull-expr [:a]) {:a 1, :b 2})
  := {:a 1}

  ((pull-expr [:a :c]) {:a 1, :b 2})
  := {:a 1, :c nil}

  ((pull-expr [:a :c str]) {:a 1, :b 2})
   := {:a 1, :c nil, 'str "{:a 1, :b 2}"}
   )


(tests
  (edge (identity 1))

  ((edge (identity 1)))
  := 1
  ((edge (identity '%)) {'% "hello"})
  := "hello"
  ((edge (str "value is: " '%)) {'% 'foo})
  := "value is: foo"

  ((let [x 1] (edge (identity x))))
  := 1

  ((let [x 2] (edge (vector 1 x 3))))
  := [1 2 3]

  ((let [x 2] (edge (vector 1 x 3 'y))) {'y 4})
  := [1 2 3 4]
  )

(tests
  (let [alice   {:db/id 9, :user/email "alice@example.com", :user/first-name "Alice"}
        bob     {:db/id 10, :user/email "bob@example.com", :user/first-name "Bob"}
        charlie {:db/id 11, :user/email "chalie@example.com", :user/first-name "Charlie"}
        db      {9 alice, 10 bob, charlie 11}
        d-entity (fn [db eid] (get db eid))
        user-summary #(str (:user/first-name %) " – " (:user/email %))
        selected 9]
    ((edge {(d-entity db selected) [:db/id :user/email user-summary keys clojure.core/keys]})))
  := '{(d-entity db selected) ; FIXME
       {:db/id 9,
        :user/email "alice@example.com",
        user-summary "Alice – alice@example.com",
        keys (:db/id :user/email :user/first-name),
        clojure.core/keys (:db/id :user/email :user/first-name)}})


(tests
  (let [alice {:db/id 9, :user/email "alice@example.com", :user/first-name "Alice" :user/address {:address/number 42}}
        bob {:db/id 10, :user/email "bob@example.com", :user/first-name "Bob"}
        charlie {:db/id 11, :user/email "chalie@example.com", :user/first-name "Charlie"}
        d-entity {9 alice, 10 bob, charlie 11}
        selected 9]
    ((edge {(d-entity selected) [{:user/address [:address/number]}]})))
   := {:user/address #:address{:number 42}} ; FIXME
 )
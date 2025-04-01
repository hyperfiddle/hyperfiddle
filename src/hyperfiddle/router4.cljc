(ns hyperfiddle.router4
  "A reactive tree router. navigation and replacestate are synchronous. Doesn't support navigation cancellation."
  (:refer-clojure :exclude [set pop])
  (:require
   [clojure.core :as cc]
   [contrib.data]
   [contrib.sexpr-router :as sexpr]
   [hyperfiddle.electric3 :as e :refer [$]]
   [hyperfiddle.electric3-contrib :as ex]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.token-zoo0 :refer [TokenNofail]]
   [hyperfiddle.rcf :refer [tests]]
   [hyperfiddle.history4 :as h]
   [missionary.core :as m]
   [clojure.string :as str])
  #?(:cljs (:import [hyperfiddle.history4.HTML5History]))
  #?(:cljs (:require-macros hyperfiddle.router4)))

(comment
  (hyperfiddle.rcf/enable! false))

;;; Minimal Lenses implementation

;; Slightly different than Haskell's
;; Haskell lenses's `view` is implemented with the Const functor.
;; We don't have fmap, so we use a two-arity function to separate read from write

;; Goal is to focus on one route, subroute, sub-subroute, etc. in the route tree.
;; So we can define a path and focus at that point.
;; We also want `.`, `..` (up) and `/` (root/absolute):
;; - `.` is the identity lens
;; - lenses cannot model `..` and `/`:
;;   - even profunctor-based lenses: they would produce the right structure but would loose context.
;;   - a "Path" monad would work
;;   - pragmatism: have a path as a stack in dynamic scope
(defn lens [getter-f setter-f]
  (fn [next-step-f]
    (fn ([value] (next-step-f (getter-f value)))
      ([value f] (setter-f value (fn [value] (next-step-f value f)))))))

(defn view [lens structure] ((lens identity) structure))
(defn identity-setter [structure f] (f structure))
(defn over [lens f structure] ((lens identity-setter) structure f))
(defn set [lens value structure] (over lens (constantly value) structure))
(def id-lens (lens identity identity-setter))

(defn key-lens [key]
  (lens
    (fn [structure] (get structure key))
    (fn [structure f] (update structure key f))))

(tests
  (view id-lens :foo)     := :foo
  (set id-lens :bar :foo) := :bar

  (view (key-lens :foo) {:foo 1})     := 1
  (set (key-lens :foo) 42 {:foo 1})   := {:foo 42}
  (over (key-lens :foo) inc {:foo 1}) := {:foo 2}

  (view (comp (key-lens :foo) (key-lens :bar)) {:foo {:bar 1}})     := 1
  (set (comp (key-lens :foo) (key-lens :bar)) 42 {:foo {:bar 1}})   := {:foo {:bar 42}}
  (over (comp (key-lens :foo) (key-lens :bar)) inc {:foo {:bar 1}}) := {:foo {:bar 2}}
)

(defn pad [n coll] (vec (contrib.data/pad (max n (count coll)) nil coll)))

(defn rest-lens
  "Return a lens focusing on and setting the rest of a seq (cdr). The rest of a
  list is always a seq – it is never nil – as per (rest (seq ())) := (). Setting
  the rest of a seq keeps the first of the seq (car) and replaces the rest of
  it (cdr) with the provided value. The provided value must be a seq, as the cdr
  of a seq is always a seq (even if empty).

  (view (rest-lens) '(:a :b :c) := '(:b :c)
  (view (rest-lens) '(:a) := ()
  (view (rest-lens) () := ()

  (set (rest-lens) '() (:a)) := (:a)
  (set (rest-lens) () ()) := '(nil)
  (set (rest-lens) '(:b) '(:a)) := '(:a :b)
  "
  []
  (lens rest (fn [value f] (cons (first value) (f (rest value))))))

(tests
  (view (rest-lens) nil) := () ; (cdr ()) := ()
  (view (comp (rest-lens) (rest-lens)) nil) := () ; (cddr ()) := (cdr (cdr ())) := ()
  (view (rest-lens) '(:a)) := ()
  (view (rest-lens) '(:a :b)) := '(:b)
  (view (rest-lens) '(:a :b :c)) := '(:b :c)
  (view (comp (rest-lens) (rest-lens)) '(:a :b :c)) := '(:c)

  (set (rest-lens) () '(:a)) := '(:a)
  (set (rest-lens) '(:b) '(:a)) := '(:a :b)
  (set (rest-lens) '(:b :c) '(:a)) := '(:a :b :c)
  (set (rest-lens) '(:c) '(:a :b)) := '(:a :c)
  (set (comp (rest-lens) (rest-lens)) '(:c) '(:a :b)) := '(:a :b :c)

  (set (rest-lens) ':a '()) :throws #?(:clj java.lang.Throwable, :cljs js/Error) ; rest of a seq must be a seq
  (set (rest-lens) () ()) := '(nil)
  (set (rest-lens) '(:a) '()) := '(nil :a)
  (set (comp (rest-lens) (rest-lens)) '(:a) '()) := '(nil nil :a)
  )

(defn adaptive-key
  "Return a lens focusing on the value at `key` in an associative data structure.
  When updating the focused value by a function `f`:
  - if value is a map, then the result is equivalent to `{key (f value)}` (trivial case),
  - if value is vector, then `key` must be a natural integer. The value at
  position key will be updated by f. If `key` is out of the array upper bound,
  the vector will be resized and padded with `nil` .
  - if value is neither a map nor a vector, then value will become {value (f
  nil)}.
  - if value is `nil`:
    - if `key` is a natural integer, then value will default to a vector,
    - otherwise value will default to a map.
  "
  [key]
  (if (= ::rest key)
    (rest-lens)
    (lens
      (fn [value] ((if (seq? value) nth get) value key nil))
      (fn [value f]
        (cond
          (map? value)    (update value key f)
          (vector? value) (update (pad key value) key f)
          (seq? value)    (seq (update (pad key value) key f))
          (nil? value)    (if (nat-int? key)
                            (update (pad key []) key f)
                            {key (f nil)})
          :else           (if (nat-int? key)
                            (update (pad key [value]) key f)
                            {value (f nil)}))))))

(defn path-lens
  "Given a sequence of keys in an associative data structure (e.g. [:foo :bar
  0 :baz]), return a lens focusing on the value at the end of the path. An empty
  path (e.g. []) is an identity."
  [path]
  ;; TODO resolve path here too?
  (cond
    (nil? path)    id-lens
    (vector? path) (reduce comp (map adaptive-key path))
    :else          (adaptive-key path)))

(tests
  (set (path-lens nil) :bar :foo)                := :bar
  (set (path-lens []) :bar :foo)                 := :bar
  (set (path-lens :foo) :baz {:foo :bar})        := {:foo :baz}
  (set (path-lens [:foo :bar]) :baz {:foo nil})  := {:foo {:bar :baz}}
  (set (path-lens [:foo 0]) :bar {:foo []})      := {:foo [:bar]}
  (set (path-lens [:foo 1]) :bar {:foo []})      := {:foo [nil :bar]}
  (set (path-lens [:foo 0]) :bar {:foo nil})     := {:foo [:bar]}
  (set (path-lens [:foo 1]) :bar {:foo nil})     := {:foo [nil :bar]}
  (set (path-lens [:foo :bar]) :baz {:foo :bar}) := {:foo {:bar :baz}}
  (set (path-lens [:foo 0]) :bar {:foo :bar})    := {:foo [:bar]}
  (set (path-lens [:foo 1]) :baz {:foo :bar})    := {:foo [:bar :baz]}
  (set (path-lens [0]) `Bar `(Foo)) := `(Bar)
  (set (path-lens [1]) :arg1 `(Foo)) := `(Foo :arg1)
  (set (path-lens [2]) :arg2 `(Foo)) := `(Foo nil :arg2)
  (set (path-lens [1 :key]) :value `(Foo)) := `(Foo {:key :value})
  (set (path-lens [1 0]) :value0 `(Foo)) := `(Foo [:value0])
  (set (path-lens [1 1]) :value1 `(Foo)) := `(Foo [nil :value1])
  (set (path-lens [1 1]) :value1 `(Foo [:value0])) := `(Foo [:value0 :value1])

  (view (path-lens [0]) `(Foo 1)) := `Foo
  (view (path-lens [1]) `(Foo 1)) := 1
  (view (path-lens [2]) `(Foo 1)) := nil
  )

;;; Relative and absolute paths

(defn safe-pop [coll] (if (empty? coll) coll (cc/pop coll)))

(defn resolve-path
  "Given a path eventually containing relative path components (e.g. `'.`, `'..`, or `'/`), resolve the final path.
  - `.` is a noop,
  - `..` navigates one level up,
  - `/` navigates to the root.
  e.g.
  [:foo '. :bar]       => [:foo :bar]
  [:foo '. '. :bar]    => [:foo :bar]
  [:foo :bar '..]      => [:foo]
  [:foo :bar '.. :baz] => [:foo :baz]
  ['.. :foo]           => [:foo]
  ['/]                 => []
  [:foo '/]            => []
  [:foo :bar '/ :baz]  => [:baz]
  "
  ([path]
   (if (seq path)
     (resolve-path [] path)
     []))
  ([stack [x & xs]]
   (if (seq xs)
     (case x
       .  (resolve-path stack xs)
       .. (resolve-path (safe-pop stack) xs)
       /  (resolve-path [] xs)
       (resolve-path (conj stack x) xs))
     (case x
       .  stack
       .. (safe-pop stack)
       /  []
       (conj stack x)))))

(tests
  (resolve-path [])                        := []
  (resolve-path [:foo])                    := [:foo]
  (resolve-path [:foo :bar :baz])          := [:foo :bar :baz]
  (resolve-path ['. :foo '. :bar '. :baz]) := [:foo :bar :baz]
  (resolve-path [:foo :bar '.. :baz])      := [:foo :baz]
  (resolve-path ['.. :foo])                := [:foo]
  (resolve-path ['.. '.. :foo])            := [:foo]
  (resolve-path [:foo '/ :bar])            := [:bar]
  (resolve-path ['/])                      := []
  (resolve-path [:foo '/])                 := []
  )

;;; human-friendly representation

(defn simplify [route]
  (cond
    (nil? route)                      ()
    (and (map? route) (empty? route)) ()

    (and (map? route) (= 1 (count route)))
    (let [k (key (first route))
          head (if k [k] [])
          child-route (simplify (val (first route)))]
      (if ((some-fn seq? vector?) child-route)
        (concat head child-route)
        (list k child-route)))

    (map? route) route

    ((some-fn seq? vector? set?) route) route
    :else (vector route)))

(tests
  "base case"
  (simplify nil) := ()
  (simplify {nil nil}) := ()
  (simplify {})  := ()
  (simplify ())  := ()
  (simplify [])  := '[]
  (simplify #{}) := '#{}

  "single branch tree"
  (simplify '{page nil})                  := '(page)
  (simplify '{page subpage})              := '(page subpage)
  (simplify '{page {subpage subsubpage}}) := '(page subpage subsubpage)

  "branching at the root"
  (simplify '{foo 1, bar 2}) := '{foo 1, bar 2}

  "branching at level x"
  (simplify '{pages {1 foo, 2 bar}}) := '(pages {1 foo, 2 bar})
  (simplify '{revisions {diff {left-sha  1234,
                               right-sha 4321}}})
  := '(revisions diff {left-sha 1234, right-sha 4321})

  "Sexprs as keys"
  (simplify '{(f x) y})      := '((f x) y)
  (simplify '{hfql {(f x) y}}) := '(hfql (f x) y)
  (simplify '{hfql {(f x) y
                    (g x) z}}) := '(hfql {(f x) y, (g x) z})

  )

(defn normalize [x]
  (cond
    (nil? x) nil
    (map? x) (not-empty (update-vals x (fn [x]
                                         (if (seq? x)
                                           {x nil}
                                           (normalize x)))))
    (seq? x) (cond
               (empty? x)       nil
               (map? (first x)) (normalize (first x))
               :else            {(first x) (normalize (next x))})
    :else    {x nil}))


(tests
  "base case"
  (normalize ()) := nil
  (normalize nil) := nil
  (normalize {})  := nil
  (normalize [])  := {[] nil}
  (normalize #{}) := {#{} nil}

  "single branch tree"
  (normalize '(page))                    := '{page nil}
  (normalize '(page subpage))            := '{page {subpage nil}}
  (normalize '(page subpage subsubpage)) := '{page {subpage {subsubpage nil}}}

  "branching at the root"
  (normalize '({page 1})) := '{page {1 nil}}

  "sexpr routes with nesting"
  (normalize '({(foo bar) baz})) := '{(foo bar) {baz nil}}
  (normalize '((foo bar) baz))   := '{(foo bar) {baz nil}}

  "branching at level 1"
  (normalize '((foo) {1 bar, 2 baz})) := '{(foo) {1 {bar nil}, 2 {baz nil}}}
  (normalize '(revisions diff {left-sha  1234,
                               right-sha 4321}))
  := '{revisions {diff {left-sha {1234 nil}, right-sha {4321 nil}}}}

  ;; TODO revise if we can use vectors in the route shape
  ;; (normalize '[revisions diff [1234 4321]]) := '{revisions {diff [{1234 nil} {4321 nil}]}}
  (normalize '(revisions diff [1234 4321])) := '{revisions {diff {[1234 4321] nil}}}

  (normalize '(pages {1 foo} {2 bar})) := '{pages {1 {foo nil}}} ; only keep first tree line

  "Sexprs as keys"
  (normalize '({(f x) y}))      := '{(f x) {y nil}}
  (normalize '(hfql {(f x) y})) := '{hfql {(f x) {y nil}}}
  (normalize '((f x) {(g y) (h z)})) := '{(f x) {(g y) {(h z) nil}}}
  )

(tests
  (normalize (simplify nil)) := nil
  (normalize (simplify {}))  := nil
  (normalize (simplify []))  := {[] nil}
  (normalize (simplify #{})) := {#{} nil}

  (normalize (simplify '{page nil}))                  := '{page nil}
  (normalize (simplify '{page subpage}))              := '{page {subpage nil}}
  (normalize (simplify '{page {subpage subsubpage}})) := '{page {subpage {subsubpage nil}}}
  (normalize (simplify '{foo 1, bar 2}))         := '{foo {1 nil}, bar {2 nil}}
  (normalize (simplify '{pages {1 foo, 2 bar}})) := '{pages {1 {foo nil}, 2 {bar nil}}}
  (normalize (simplify '{revisions {diff {left-sha  1234,
                                          right-sha 4321}}}))
  := '{revisions {diff {left-sha  {1234 nil}, right-sha {4321 nil}}}}

  (normalize (simplify '{hfql {(f x) y (g x) z}}))
  := '{hfql {(f x) {y nil}, (g x) {z nil}}}
  )


;;; URL encoding

(defn encode* [route] (sexpr/encode (not-empty (simplify route))))
(defn decode* [path] (sexpr/decode path))

(tests
  (decode* (encode* nil)) := nil
  (decode* (encode* {}))  := nil
  (decode* (encode* []))  := nil
  (decode* (encode* #{})) := nil

  (normalize (decode* (encode* '{page nil})))              := '{page nil}
  (normalize (decode* (encode* '{page subpage})))          := '{page {subpage nil}}
  (normalize (decode* (encode* '{page {subpage subsubpage}}))) := '{page {subpage {subsubpage nil}}}
  (normalize (decode* (encode* '{foo 1, bar 2})))          := '{foo {1 nil}, bar {2 nil}}
  (normalize (decode* (encode* '{pages {1 foo, 2 bar}})))  := '{pages {1 {foo nil}, 2 {bar nil}}}
  (normalize (decode* (encode* '{revisions {diff {left-sha  1234,
                                                  right-sha 4321}}})))
  := '{revisions {diff {left-sha  {1234 nil},
                        right-sha {4321 nil}}}}
  (normalize (decode* (encode* '{hfql {(f x) y (g x) z}}))) := '{hfql {(f x) {y nil} (g x) {z nil}}}
  )

;;; Electric

(e/declare ^{:doc "The router base path. All links will resolve under the base. Defaults to the current browsing context `baseURI`'s pathname if the document contains an explicit <base>, \"/\" otherwise."} basis)
(e/declare ^{:doc "A stack of paths"} paths)
(e/declare ^{:doc "The current path"} path)
(e/declare ^{:doc "Top level route"} root-route)
(e/declare ^{:doc "Current route in the scope of a router"} route)

(def encode encode*)
(def decode decode*)

(defn current-path [paths] (into [] cat paths))

(defn as-vec [x] (if (vector? x) x [x]))

(e/defn Focus [path Body-fn]
  (let [paths (conj hyperfiddle.router4/paths (as-vec path))
        path (resolve-path (current-path paths))]
    (binding [hyperfiddle.router4/paths paths
              hyperfiddle.router4/path  path
              route (view (path-lens path) root-route)]
      ($ Body-fn))))

(defmacro focus [path & body] `($ Focus ~path (e/fn [] ~@body))) ;; TODO find a better name
(defmacro pop [& body] `(focus [::rest] ~@body))

(defn split-link-path [path]
  (cond (not (vector? path)) [['.] path]
        (= 1 (count path))   [['.] (first path)]
        :else                [(vec (butlast path)) (last path)]))

(e/defn Route-for
  ([path] (e/apply Route-for (split-link-path path)))
  ([path value]
   (over (path-lens (resolve-path (into hyperfiddle.router4/path path))) (constantly value) root-route)))


#_ ;; l/local expects l/def which we should only use in tests
(tests ;; static routing
  (with ((l/local
           (binding [root-route nil]
             (focus 'index
               (tap path)
               (focus 'foo
                 (tap path)
                 (tap ($ Route-for "foo")))
               (focus 'bar
                 (tap path)
                 (tap ($ Route-for "bar"))))))))
  % := '[index]
  % := '[index foo]
  % := '{index {foo "foo"}}
  % := '[index bar]
  % := '{index {bar "bar"}}
  )

;; (tests ;; dynamic routing
;;   (binding [root-route '{users {12 profile, 42 profile}}]
;;     (focus 'users
;;       (fn []
;;         (tap route)
;;         (doall
;;           (for [[user-id _] route]
;;             (focus [user-id 'profile]
;;               (fn []
;;                 (tap path)
;;                 (tap (route-for "settings")))))))))
;;   % := '{12 profile, 42 profile}
;;   % := '[users 12 profile]
;;   % := '{users {12 {profile "settings"}, 42 profile}}
;;   % := '[users 42 profile]
;;   % := '{users {12 profile, 42 {profile "settings"}}}
;;   )

(e/defn Route-at [path]
  (view (path-lens (resolve-path (into hyperfiddle.router4/path path))) root-route))

;; (tests
;;   (binding [root-route '{index {foo 1, bar 2}}]
;;     (focus 'index
;;       (fn []
;;         (tap path)
;;         (tap route)
;;         (focus 'foo
;;           (fn []
;;             (tap path)
;;             (tap route)
;;             (tap (route-at '[.. bar]))
;;             (tap (route-for '[.. bar] "altered-by-sibling")))))))
;;   % := '[index]
;;   % := '{foo 1, bar 2}
;;   % := '[index foo]
;;   % := 1
;;   % := 2
;;   % := '{index {foo 1, bar "altered-by-sibling"}}
;;   )


(e/defn Current-route? [target-route]
  (= (resolve-path (current-path paths))
    (resolve-path (current-path (conj paths (as-vec target-route))))))

;; (tests
;;   (focus '[page subpage]
;;     (fn []
;;       (tap (current-route? '.))
;;       (tap (current-route? 'subsubpage))
;;       (focus 'subsubpage
;;         (fn []
;;           (tap (current-route? '[.. .. .. page subpage subsubpage]))))))
;;   % := true
;;   % := false
;;   % := true)

;;; Link

#?(:cljs
   (defn on-link-click [next-route ^js node ^js e]
     ;; enrich click event with:
     ;; - the route
     ;; - is the link internal or external (soft vs hard nav)
     (let [target (.getAttribute node "target")]
       (set! (.-hyperfiddle_router_route e) next-route)
       (set! (.-hyperfiddle_router_external_nav e)
         (or (some? (.getAttribute node "download"))
           (and (some? target) (not= "_self" target))))
       nil)))

#?(:cljs
   (defn link-click-handler [node next-route]
     (m/reductions {} nil
       (dom/listen-some node "click"
         (fn [e]
           (on-link-click next-route node e))))))

(e/declare current-route?)

(defn normalize-route-value [x]
  (if (seq? x)
    (normalize {x nil})
    (normalize x)))

(e/defn Link [path Body]
  (e/client
    (let [[path' value] (split-link-path path)]
      (dom/a
        (e/input (link-click-handler dom/node (into hyperfiddle.router4/path path))) ; TODO replace with Token/directive
        (dom/props {::dom/href (add-document-basis basis (encode ($ Route-for path' value)))})
        (binding [current-route? ($ Current-route? path')]
          ($ Body))))))

(defmacro link [path & body]
  `($ Link ~path (e/fn [] ~@body)))

;;; History integration

#?(:cljs
   (defn- internal-nav-intent? [^js e]
     (and (some? (.-hyperfiddle_router_route e))
       (not (.-hyperfiddle_router_external_nav e)))))

#?(:cljs
   (defn -get-event-route [^js event]
     (.-hyperfiddle_router_route ^js event)))

#?(:cljs (defn as-directory [path] (-> path (str/replace #"/+$" "") (str "/"))))
#?(:cljs (defn path-name [url-str] (-> (new js/URL url-str) (.-pathname) (str/replace #"/[^\/]+$" ""))))

#?(:cljs
   (defn document-basis [current-node]
     (if (some? (.querySelector js/document "base")) ; return first found node in document. HTML spec says browsers should ignore subsequent <base> nodes.
       (-> current-node .-baseURI path-name as-directory) ; a base is only defined if it has children, so it's always a directory.
       "/")))

#?(:cljs
   (defn strip-document-basis [basis path]
     (if (str/starts-with? path basis)    ; default basis is "/"
       (str/replace-first path basis "/") ; noop for default basis
       ;; Edge case: Document is served at a URL not matching the <base>. This
       ;; is counter intuitive but not forbidden on the web. Use case: decouple
       ;; serving a document from resolving relative links of this document. We
       ;; decide to match on the path as-is anyway and eventually let userland
       ;; match or show a "not found" page. Redirecting will account for the
       ;; basis. React-Router has a different approach: it doesn't leverage
       ;; <base> but require developers to provide a `baseName` prop in jsx,
       ;; treated as a prefix to all routes. While `baseName` ensures relative
       ;; links all share a common prefix, it also forces all routes to match
       ;; the common prefix, thus forces the document to be served at the same
       ;; location as where it resolves relative links.
       path)
     ))

#?(:cljs (defn add-document-basis [basis path] (str basis (str/replace path #"^/" ""))))

(e/defn OnNavigate ; TODO replace with Service/directive
  "Will call `Callback` on internal `router/Link` click. `Callback` takes 2 arguments:
   - the route to navigate to
   - the dom click js event."
  ([Callback] (e/client ($ OnNavigate (.-document js/window) Callback)))
  ([node Callback]
   (e/client
     ;; navigation by link click (also supports keyboard nav)
     ;; only intercepts internal links. See `hyperfiddle.router4/Link`.
     (when-let [^js event (dom/On node "click" #(when (internal-nav-intent? %) (.preventDefault %) %) nil nil)]
       ($ Callback (-get-event-route event) event)))))

(e/defn Navigate!
  ([path]
   (h/navigate! h/history (add-document-basis basis (encode ($ Route-for path)))))
  ([path delay-ms] ; G: questionable feature – matches html redirect with <meta> – is there a use case?
   (case (e/Task (m/sleep delay-ms))
     (Navigate! path))))

#_
(e/defn Navigate2! ; proposed impl
  ([value] (Navigate2! ['.] value))
  ([path value] (Navigate! (conj path value)))
  ([path value delay-ms] (Navigate! (conj path value) delay-ms)))

(e/defn ReplaceState! [path] ;; TODO find a better name
  (e/client (h/replace-state! h/history (add-document-basis basis (encode ($ Route-for path))))))

(e/defn ReplaceState2!
  ([value] (ReplaceState2! ['.] value))
  ([path value] (ReplaceState! (conj path value))))

#?(:node nil
   :default
   (e/defn HTML5-History []
     (e/client
       (let [!history (h/html5-history)]
         (dom/On js/window "popstate" #(h/set-history! !history (h/html5-path) (.-timeStamp %)) nil nil)
         !history))))

(e/defn ForAll
  "Experimental - mount a fresh F for all values of x, trashing F mounted for previous values of x."
  [x F]
  (e/for [x (e/diff-by (fn [_] (random-uuid)) (e/as-vec x))] ; G: bazooka to kill a fly?
    (F x)))

(e/defn Router [history BodyFn]
  (binding [basis (e/Reconcile (or basis (document-basis dom/node)))]
    (binding [h/history  history
              root-route (decode (strip-document-basis basis (e/watch history)))
              path       []
              paths      []]
      ($ OnNavigate dom/node (e/fn [route e] (ForAll e (e/fn [_e] (Navigate! route)))))
      (focus '/
        ($ BodyFn)))))

(defmacro router [history & body]
  `($ Router ~history (e/fn [] ~@body)))

(defn update-key
  "Update a key in a map. Replaces key `k` in map `m` by the result of `(f k)`.
  The corresponding value is preserved."
  ;; Useful for HFQL and `demo-colors`
  [m k f]
  (let [k' (f k)
        m' (assoc m k' (get m k))]
    (if (= k' k)
      m'
      (dissoc m' k))))

(defn set-key "Replaces `old-key` in map `m` by `new-key` without altering the corresponding value.
  Similar to `clojure.set/rename-keys`, but faster and for a single key."
  ;; Useful for HFQL and `demo-colors`
  [m old-key new-key]
  (update-key m old-key (constantly new-key)))

(tests
  (set-key nil :a :b)          := {:b nil}
  (set-key {} :a :b)           := {:b nil}
  (set-key nil :a :a)          := {:a nil}
  (set-key {:a 1} :a :b)       := {:b 1}
  (set-key {:a 1, :b 2} :a :c) := {:c 1, :b 2}
  )

(tests
  (update-key nil 1 inc) := {2 nil}
  (update-key {} 1 inc) := {2 nil}
  (update-key {1 :one} 1 inc) := {2 :one}
  (update-key '{(foo 1) bar} '(foo 1) (partial cons 'baz)) := '{(baz foo 1) bar}
  )

(e/defn Apply "
(e/Apply F r/route) with fallback to supplied args when route is nil."
  [F args] ; gotcha: argv damages site, make sure it's on client
  (e/client
    (case (ReplaceState! ['. (or (seq route) args)]) ; ensure
      (e/Apply F route))))

(ns hyperfiddle.hfql2
  (:refer-clojure :exclude [format])
  (:require
   [clojure.core :as cc]
   [clojure.core.protocols :as ccp]
   [clojure.datafy]
   [clojure.pprint :refer [cl-format]]
   [clojure.set :as set]
   [dustingetz.str :as strx]
   [hyperfiddle.rcf :refer [tests]]
   [hyperfiddle.ct.monoid :refer [mappend]]
   [hyperfiddle.ct.functor :refer [fmap]]
   [hyperfiddle.ct.function :refer [flip]]
   [hyperfiddle.ct.bifunctor :as bf]
   [hyperfiddle.ct.pair :refer [pair fst snd swap]]
   [hyperfiddle.ct.comonad :refer [extract]]
   [hyperfiddle.ct.monad :refer [bind =<<]]
   [hyperfiddle.ct.cofree :refer [tree-seqT unwrap runCofreeT]]
   [hyperfiddle.ct.rose-tree :refer [rose-tree]]
   [hyperfiddle.ct.store :refer [pos peeks]]
   [hyperfiddle.ct.experiments.thunk :refer [force-thunk]]
   [hyperfiddle.ct.either :refer [either either? left right unwrap! from-right from-left]]
   [hyperfiddle.hfql2.analyzer :as ana]
   [hyperfiddle.hfql2.coalgebra :as co :refer [unfoldT hfqlT hoistHfqlT]]
   [hyperfiddle.hfql2.protocols :refer [Identifiable Suggestable ComparableRepresentation Datafiable -hfql-resolve -resolve-dispatch
                                        Searchable -search-matches *search-max-collection-size* *search-max-str-len*]]
   [hyperfiddle.hfql2.utils :refer [has-meta? imap? original-object]]
   #?(:cljs [goog.string :as gstring])
   #?(:cljs [goog.string.format])
   #?(:cljs [cljs.stacktrace :as cljs-stacktrace])
   #?(:cljs [goog.userAgent.product :as user-agent-product]))
  #?(:cljs (:require-macros [hyperfiddle.hfql2 :refer [hfql defpointfree]])))

(defmacro defpointfree
  "Like (def foo (comp a b)) but improve stacktraces by printing `foo` instead of comp_XYZ."
  {:clj-kondo/lint-as 'clojure.core/def}
  [name f]
  `(defn ~name [point#] (~f point#)))

(def seed co/seed)
(defpointfree nav* (comp force-thunk extract))
(defpointfree nav (comp original-object unwrap! nav*)) ; not to be confused with ccp/nav nor hyperfiddle.hfql2.protocols/nav. TODO disambiguate
(defpointfree pull* (comp extract co/pull))
(defpointfree pull (comp force-thunk pull*))
(defpointfree next-steps unwrap)
(def ^:private from-either (either left identity))
(defpointfree values (comp (fmap (comp #_from-either force-thunk nav)) tree-seqT))

(defpointfree scope (comp pos snd runCofreeT))
(defpointfree value-at-point (comp force-thunk '% scope))
(defpointfree seeded? (comp (flip contains? '%) scope))
(defn update-scope [stepT f & args] (seed (apply f (scope stepT) args) stepT))
(defpointfree metas (comp fst runCofreeT))
(defpointfree pull-has-star? (comp ::ana/has-star? metas))
(defpointfree partial? (comp ::ana/partial metas))
(defpointfree fullfilled? (some-fn seeded? (complement partial?)))
(defpointfree hfql? co/hfql?)

(declare unchunk-seq)
(defn navigable [nav-fn object]
  (vary-meta (unchunk-seq object) assoc `hyperfiddle.hfql2.protocols/-nav (#?(:clj bound-fn, :cljs fn) [_coll key value] (nav-fn key value))))

(defn- safe-vary-meta [object f & args]
  (if (has-meta? object)
    (apply vary-meta object f args)
    object))

(defn filtered [object] (safe-vary-meta object assoc ::filtered true))
(defn filtered? [object] (::filtered (meta object)))

(defn hfql* [analyzed-hfql] #_(seed {}) (unfoldT hfqlT analyzed-hfql))

(defn build-hfql
  ([hfql-expression] (build-hfql {} hfql-expression))
  ([&env hfql-expression]
   (unfoldT hfqlT (ana/analyze (or &env {}) hfql-expression))))

(defmacro hfql
  ([hfql-form] `(hfql* (ana/hfql ~hfql-form)))
  ([hfql-form x] `(seed {~''% (right ~x)} (hfql ~hfql-form))))



(tests
  (set! *print-namespace-maps* false)
  )

;;; tests
(tests
  "Literal getter"
  (values (seed {'% {:a 1, :b 2}} (hfql :a)))
  := '(1)

  (values (seed {'% {:a 1, :b 2, 'c 3}} (hfql 'c)))
  := '(3)

  (values (seed {'% {:a 1, :b 2, 'c 3, "d" 4}} (hfql "d")))
  := '(4))

(tests
  "Literals – symbol, value and var do not mix."
  (values (seed {'% {'str 'str, str str, #'str #'str}} (hfql 'str)))
  := '(str)

  "Function values are looked up if they appear themselves in a map."
  (values (seed {'% {'str 'str, str str, #'str #'str}} (hfql str)))
  := (list str)

  "Quoted var is always looked up as a literal."
  (values (seed {'% {'str 'str, str str, #'str #'str}} (hfql '#'str)))
  := [#'clojure.core/str])

(tests
  "Function values are applied if they don't appear themselves in a map."
  ;; This behavior can be ambiguous but this is necessary in cases where userland retrun {fn ...}
  ;; or when the function is lexically bound and cannot be resolved through a var.
  ;; To disambiguate, use:
  ;; - 'f to always lookup
  ;; - #'f to always apply
  ;; - pass a custom getter function
  (values (seed {'% {'str 'str, :str :str #'str #'str}} (hfql str)))
  := '("{str str, :str :str, #'clojure.core/str #'clojure.core/str}"))

(tests
  "Var always mean “apply function”."
  (values (seed {'% {'str 'str, str str, #'str #'str}} (hfql #'str)))
  := '("{str str, #function[clojure.core/str] #function[clojure.core/str], #'clojure.core/str #'clojure.core/str}")
  )


(tests
  "missing literals"

  (values (seed {'% {str str, #'str #'str}} (hfql 'str)))
  := '(nil)

  (values (seed {'% {'str 'str, str str}} (hfql '#'str)))
  := '(nil)
  )



(tests
  "manual step-by-step navigation"
  (hfql {(range 3) {* inc}})
  (nav *1)
  := '(0 1 2)
  (next-steps *2)
  (count *1)
  := 3
  (map nav *2)
  := '(1 2 3))

(tests
  "Sequencing pre-order traversal"
  (values (hfql {(range 3) {* inc}}))
  := '((0 1 2) 1 2 3)

  (values (hfql {(range 3) {* {inc inc}}}))
  := '((0 1 2) 1 2 2 3 3 4)
  )


(tests ; FIXME not sure what to do here ATM. Will revisit after Datomic Browser is done, Datomic Browser has higher priority.
  ;; Goal: navigate through object graph at the REPL
  ;; Problem: in a grid UI we want scalars at leaves, not objects
  ;; technical issue: HFQL bypasess `ccp/nav` at expression leaves, causing this test to fail.
  ;; Questions:
  ;; - should this test be changed?
  ;; - should HFQL be changed and use another strategy to navigate/not navigate a leaves?
  (def _attribute  {:db/id 42, :db/ident :attribute/name})
  (def _object (with-meta {:a 42} {`ccp/nav (fn [o k v] (if (= :a k) _attribute (get o k v)))}))
  (nav (seed {'% _object} (hfql :a)))
  := _attribute

  (values (seed {'% _object} (hfql {:a :db/ident})))
  := '({:db/id 42, :db/ident :attribute/name} :attribute/name)

  )


(tests
  "pull syntax"
  (values (seed {'% {:a 1}} (hfql []) )) ; [] ≅ [*]
  := '({:a 1} 1)

  (values (seed {'% {:a 1, :b 2}} (hfql [:a])))
  := '({:a 1} 1)

  (values (seed {'% {:a 1, :b 2}} (hfql [:a :c])))
  := '({:a 1, :c nil} 1 nil)

  (values (seed '{% {:a 1, :b 2}}  (hfql [:a :c str])))
  := '({:a 1, :c nil, str "{:a 1, :b 2}"} 1 nil "{:a 1, :b 2}")

  )

(tests
  "star traversal"
  (values (seed {'% [{:a 1} {:a 2}]} (hfql {identity {* :a}})))
  := '([{:a 1} {:a 2}] 1 2)

  ;; A tree must have a single root. So in this special “top-level *” case, * returns identity and fans-out.
  (values (seed {'% [{:a 1} {:a 2}]} (hfql {* :a})))
   := '([{:a 1} {:a 2}] 1 2)
   )


(tests
  "Manual navigation"
  (let [alice {:db/id 9, :user/name "Alice" :user/email "alice@example.com"}
        step (seed {'% alice} (hfql {identity [:user/name :user/email]}))]

    (nav step)
    := alice

    (force-thunk (nav (first (next-steps step)))) ; FIXME computing next-steps should not require `step` to be seeded.
    := {:user/name "Alice", :user/email "alice@example.com"}

    (map nav (-> step next-steps first next-steps))
    := ["Alice" "alice@example.com"]
    ))


(tests
  (pull (seed {'% {:a 1}} (hfql :a)))
  := 1
  (pull (seed {'% {:a 1, :b 2}} (hfql [:a])))
  := {:a 1}
  (pull (seed {'% {:a 1, :b 2}} (hfql {identity [:a]})))
  := '{identity {:a 1}}
  (pull (seed {'% [{:a 1, :b 2} {:a 3, :c 4}]} (hfql {* [:a]}))) ; This is how we express pull-many.
  := '({:a 1} {:a 3})

  (pull (seed {'% [{:a 1, :b 2} {:a 3, :c 4}]} (hfql {identity {* [:a]}})))
  := '{identity ({:a 1} {:a 3})}

  (pull (seed {'% [{:a 1, :b 2} {:a 3, :c 4}]} (hfql {identity {* {identity [:a]}}})))
  := '{identity ({identity {:a 1}} {identity {:a 3}})}

  (defn _ns-publics [ns] (take 2 (ns-publics ns)))
  (pull  (hfql {(_ns-publics 'clojure.core) {* {val symbol}}}))
  := '{(_ns-publics clojure.core) ({val clojure.core/primitives-classnames} {val clojure.core/+'})}

  (pull (hfql {(_ns-publics 'clojure.core) {* {val [symbol]}}}))
  := '{(_ns-publics clojure.core) ({val {symbol clojure.core/primitives-classnames}}
                                   {val {symbol clojure.core/+'}})}

  )

(tests
  (pull (seed '{% {:a {:db/id 1 :db/ident :foo}}} (hfql [:a {:a :db/ident}])))
  := {:a :foo}
  )




(def ^:private _alice_    {:db/id 9, :user/email "alice@example.com", :user/name "Alice" :user/address {:address/number 42}})
(def ^:private _bob_      {:db/id 10, :user/email "bob@example.com", :user/name "Bob"})
(def ^:private _charlie_  {:db/id 11, :user/email "chalie@example.com", :user/name "Charlie"})
(def ^:private _db_       ^:symbolic {9 _alice_, 10 _bob_, 11 _charlie_})
(def ^:private _d-entity_ (fn [db eid] (get db eid)))
(defn- _people_ []
  (->> (list 9 10 11)
    (navigable (fn [_index value] (_d-entity_ _db_ value)) )))


(tests
  (let [user-summary #(str (:user/name %) " – " (:user/email %))
        selected 9
        expr (hfql {(_d-entity_ _db_ selected) [:db/id :user/email user-summary keys clojure.core/keys cc/keys]})]
    #_(values expr)
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
  := {'(_d-entity_ _db_ 9)
      {:user/name "Alice", :user/address {:address/number 42}}}
  )

(tests
  (values (seed {'% {:a 1}} (hfql *)))
  := '({:a 1})

  (pull (seed {'% _alice_} (hfql *)))
  := _alice_

  (pull (seed {'% _alice_} (hfql {identity [:db/id *]})))
  := {'identity _alice_})


(tests
  "Traversing calls into ccp/nav"
  (values (hfql {(_people_) {* [:user/name]}}))
   := '((9 10 11) {:user/name "Alice"} "Alice" {:user/name "Bob"} "Bob" {:user/name "Charlie"} "Charlie")

  (pull (hfql {(_people_) {* [:user/name]}}))
  := {'(_people_) [{:user/name "Alice"}
                   {:user/name "Bob"}
                   {:user/name "Charlie"}]})


;;; Metas

(tests
  "keyword aliases"

  (::ana/form-short (metas (hfql :test)))
  := :test

  (::ana/form (metas (hfql ::test)))
  := :hyperfiddle.hfql2/test

  (::ana/form-short (metas (hfql ::test)))
  := "::test"

  )

(tests
  "No need to seed to get static children"
  (->> (hfql {identity inc})
    (next-steps)
    (first)
    (metas)
    (::ana/form))
   := 'inc
  )

(tests
  "children metas"
  ;; G: I added this test case because hoistCofreeT can wrap metadatas like
  ;; matryoshkas. Causing every point to contain a stack of metas from root to
  ;; point. There may be a future use case for "looking up parent metas", but
  ;; for now we only want metas "at point" for every point.
  ;; This test case ensures no matryoshkas on metadata.
  (->> (hfql {identity inc})
    (next-steps)
    (first)
    (metas)
    (::ana/form))
  := 'inc
  )

(comment
  (pull (hfql {(all-ns) {* ns-name}}))
  )

#?(:clj
   (tests
     "Clojure 1.12 method functions"

     (values (seed {'% "  "} (hfql [String/.isBlank java.lang.String/.isEmpty .length])))
     := '({String/.isBlank true, java.lang.String/.isEmpty false, .length 2} true false 2)
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sitemap / Dynamic HFQL expression manipulation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn wrap-in-pull [stepT]
  (seed (scope stepT)
    (hfql* (ana/wrap-in-pull (::co/AST (metas stepT))))))

(tests
  (pull (seed {'% 1} (hfql inc)))
  := 2
  (pull (wrap-in-pull (seed {'% 1} (hfql inc))))
  := '{inc 2}
  )

(defn- merge-pull-metas [metas-a metas-b]
  (let [form-a (ana/unquote* (::ana/form metas-a))
        merged-form (vec (distinct (into (vec form-a) (ana/unquote* (::ana/form metas-b)))))]
    (assoc metas-a
      ::ana/lexical-env {}
      ::ana/form merged-form
      ::ana/symbolic-edges merged-form
      )))

(tests
  (merge-pull-metas
    (extract
      (ana/analyze {'% 1} [:a :b]))
    (extract
      (ana/analyze {'% 2} [:a :c])))
  := {:hyperfiddle.hfql2.analyzer/id _
      :hyperfiddle.hfql2.analyzer/type :hyperfiddle.hfql2.analyzer/pull,
      :hyperfiddle.hfql2.analyzer/form [:a :b :c],
      :hyperfiddle.hfql2.analyzer/lexical-env '{},
      :hyperfiddle.hfql2.analyzer/symbolic-edges [:a :b :c]
      :hyperfiddle.hfql2.analyzer/has-star? false
      :hyperfiddle.hfql2.analyzer/partial _}
  )

(defn symbolic-edge* [metas]
  (or (::label metas)
      (::ana/symbolic-edge
       (case (::ana/type metas)
         ::ana/traversal (::ana/edge metas)
         metas))))

(defn symbolic-edge [hfqlT]
  (when hfqlT
    (symbolic-edge* (metas hfqlT))))

(defn- merge-pull-children [children-A children-B]
  (let [keyf (comp symbolic-edge* extract)
        a-edges (into #{} (map keyf children-A))]
    (into children-A (remove (comp a-edges keyf) children-B))))

(defn- combine-pull [rose-a rose-b]
  (let [metas (merge-pull-metas (extract rose-a) (extract rose-b))]
    (fmap (fn [kv] (assoc kv ::ana/lexical-env (::ana/lexical-env metas)))
      (rose-tree
        metas
        (merge-pull-children
          (unwrap rose-a) (unwrap rose-b))))))

(defn combine ; TODO replace with `union`
  ([hfqlT] hfqlT)
  ([hfqlT-a hfqlT-b]
   (if (nil? hfqlT-b)
     hfqlT-a
     (do
       (assert (= (::ana/type (metas hfqlT-a)) (::ana/type (metas hfqlT-b))))
       (seed (merge (scope hfqlT-a) (scope hfqlT-b))
         (unfoldT hfqlT (case (::ana/type (metas hfqlT-a))
                          ::ana/pull (combine-pull (::co/AST (metas hfqlT-a)) (::co/AST (metas hfqlT-b)))
                          ::ana/traversal (throw (ex-info "TODO" {}))
                   (throw (ex-info "Can't combine." (::ana/type (metas hfqlT-a))))))))))
  ([hfqlT-a hfqlT-b & hfqlTs]
   (reduce combine (combine hfqlT-a hfqlT-b) (remove nil? hfqlTs))))

;; TODO
(defn- union [a b]) ; unwrap children from both `a` and `b`, and glue them together. Do not call into `analyze`.
(defn- zoom ; do we want/need full Van Laarhoven-style lenses?
  "Zoom into a CofreeT through a lens-like projection."
  [edge t] #_(extend t (fn [ctx] (derive getter ctx))))

(tests
  (->
    (combine
      (let [a 1]
        (hfql [:a :b]))
      (let [b 2]
        (hfql [:a :c])))
    (metas)
    (dissoc ::co/AST))
  := {:hyperfiddle.hfql2.analyzer/id _
      :hyperfiddle.hfql2.analyzer/type :hyperfiddle.hfql2.analyzer/pull,
      :hyperfiddle.hfql2.analyzer/form [:a :b :c],
      :hyperfiddle.hfql2.analyzer/lexical-env {},
      :hyperfiddle.hfql2.analyzer/symbolic-edges [:a :b :c],
      :hyperfiddle.hfql2.coalgebra/static-children _
      :hyperfiddle.hfql2.coalgebra/scope '{a 1, b 2}
      :hyperfiddle.hfql2.analyzer/has-star? false
      :hyperfiddle.hfql2.analyzer/partial _
      })

;; (defn filter-pull [pred pullT]
;;   (assert (= ::ana/pull (::ana/type (metas pullT))))
;;   (let [children-forms (into #{} (map ::ana/form (map metas (filter pred (unwrap pullT)))))]
;;     (seed (scope pullT)
;;       (hfql* (ana/filter-pull #(children-forms (::ana/form %)) (fmap swap (::co/AST (metas pullT))))))))

;; (tests
;;   (filter-pull #(#{:a :c} (::ana/form (metas %)))
;;     (seed {'% 1} (hfql [:a :b :c])))
;;   := (seed {'% 1} (hfql [:a :c]))
;;   )


;; (defn select-children "Like select-keys but select children of a pull expression."
;;   [pullT childrenT-seq]
;;   (let [child-matches? (into #{} (map (comp ::ana/form metas)) childrenT-seq)]
;;     (filter-pull (fn [%](child-matches? (::ana/form (metas %)))) pullT)))


;; (tests
;;   (select-children (seed {} (hfql [:a :b :c]))
;;     (unwrap (hfql [:a :c])))
;;   := (seed {} (hfql [:a :c]))
;;   )

(defn- normalize-edge [edge]
  (cond (ident? edge) `(~edge ~'%)
        :else edge))

(declare identify)
(defn symbolic-edge-computed [hfqlT]
  (or (seq
        (map (fn [part] (or (identify part) part))
          (normalize-edge
            (co/symbolic-edge hfqlT ::co/point-free false))))
    (either (comp symbol pr-str type) identify (value-at-point hfqlT))))

(defn- call-matches? [edgeA edgeB]
  (when (and (seq? edgeA) (seq? edgeB)) ; call form
    (and (= (first edgeA) (first edgeB)) ; same fn
      (= (count edgeA) (count edgeB)) ; same argc
      )))

(defn- child-matches? [symbolic-edge childT]
  (let [child-edge (hyperfiddle.hfql2/symbolic-edge childT)]
    (or (= symbolic-edge child-edge) ; direct match
      (call-matches? symbolic-edge child-edge)
      (call-matches? symbolic-edge (normalize-edge child-edge))
      )))

(defn find-child-by [pred hfqlT]
  (->> (unwrap hfqlT)
    (eduction
      #?(:clj (halt-when (fn [_] (.isInterrupted (Thread/currentThread))))
         :cljs (map identity))
      (filter pred))
    (first)))

(defn find-child [symbolic-edge hfqlT] (find-child-by (partial child-matches? symbolic-edge) hfqlT))

;; (defn focus-subtree [hfqlT] (seed (scope hfqlT) (hfql* (fmap swap (::co/AST (metas hfqlT))))))

;; (defn find-sitemap-entry [symbolic-edge hfqlT]
;;   (case (::ana/type (metas hfqlT))
;;     ::ana/pull (some-> (find-child symbolic-edge hfqlT)
;;                  ;; we want to refocus subtree because recursion in a UI is
;;                  ;; different than recursion in a textual pull In a pull we want
;;                  ;; to recur on top-level pulls In a UI we don't want to recur
;;                  ;; on the sitemap root but rather on the selected sitemap
;;                  ;; entry, according to the current top-level URL route.
;;                  focus-subtree)
;;     (if (child-matches? symbolic-edge hfqlT)
;;       hfqlT
;;       nil)))

(def id (comp ::ana/id metas))

(defn find-cell-by-column* "Lookup a cell in a set of cells."
  ([columnT] (partial find-cell-by-column* columnT))
  ([columnT cellTs]
   (when columnT
     (let [identity (id columnT)]
       (first (filter #(= identity (id %)) cellTs))))))

(defn find-cell-by-column "Lookup a column in a row."
  ([columnT] (partial find-cell-by-column columnT))
  ([columnT rowT]
   (find-cell-by-column* columnT (unwrap rowT))))

(defn find-cell-by-symbolic-column "Lookup a column in a row by its symbolic form." ; TODO isn't that somewhat a duplicate of find-child?
  ([symbolic-column] (partial find-cell-by-symbolic-column symbolic-column))
  ([symbolic-column rowT]
   (when symbolic-column
     (first (filter #(= symbolic-column (symbolic-edge %)) (unwrap rowT))))))

(comment
  (def _rows (unwrap (seed {'% (clojure.java.io/file ".")} (dustingetz.file-explorer/sitemap `dustingetz.file-explorer/dir-list))))
  (def _columnT (first (unwrap (first _rows))))
  (find-cell-by-column _columnT (second _rows))
  )

;;;;;;;;;;;;;;;
;; Protocols ;;
;;;;;;;;;;;;;;;

;; TODO disambiguate – following "nav" should be exposed here but name is already taken by `nav` above.
#_(defn nav [object k v] (hyperfiddle.hfql2.protocols/nav object k v))

(def ^{:dynamic true, :doc "HFQL will bind this var to the current collection being traversed in the context where `hfql-resolve` is called."} *collection-at-point*)
(def ^{:dynamic true, :doc "HFQL will bind this var to the current value in the context where `hfql-resolve` is called."} *value-at-point*)
(defn hfql-resolve* [symbolic-object-constructor]
  (try
    (let [resolved (-hfql-resolve symbolic-object-constructor)]
      (if (either? resolved)
        resolved
        (right resolved)))
    (catch #?(:clj Throwable, :cljs :default) t
      (left t))))

(defn hfql-resolve [symbolic-object-constructor] (unwrap! (hfql-resolve* symbolic-object-constructor)))

(defn identifiable [identify-fn object]
  (vary-meta object assoc `hyperfiddle.hfql2.protocols/-identify (#?(:clj bound-fn*, :cljs identity) identify-fn))) ; #?(:clj bound-fn* :cljs identity)

#?(:clj
   (defn- extends-protocol? [proto atype]
     (and atype
       (or (extends? proto atype)
         (boolean (some #(extends-protocol? proto %) (bases atype)))))))

(defn identifiable? [object]
  (or (and (has-meta? object) (contains? (meta object) `hyperfiddle.hfql2.protocols/-identify))
    #?(:clj (extends-protocol? Identifiable (#?(:clj class, :cljs type) object))
       :cljs (and (not (nil? object))
               (object? object)
               (satisfies? Identifiable object)))))

(defn identify
"Produces a symbolic identity for `obj`, or nil if `obj` does not explicitly
implement `Identifiable`. Serializable and uniquely resolvable symbolic
identities should be favored.
All values being self-identical, `(or (identify x) x)` always yields a valid
identifier, though it might not be serializable."
  [obj]
  (when (identifiable? obj)
    (hyperfiddle.hfql2.protocols/-identify obj)))

#?(:clj
   (tests
     (identify (Object.)) := nil
     (identify nil) := nil
     (identify (identifiable (constantly `hash-map) {})) := `hash-map)

   :cljs
   (tests
     (identify (js-obj)) := nil
     (identify nil) := nil
     (identify (identifiable (constantly `hash-map) {})) := `hash-map)
   )

(extend-protocol Identifiable
  #?(:clj java.util.Map$Entry, :cljs MapEntry)
  (-identify [entry]
    (let [key (key entry)
          identity (or
                     (identify key)
                     (and (ana/literal? key)
                       key)
                     (and (ident? key)
                       key)
                     nil)]
      (or
        (and (some? identity)
          `(find ~identity))
        nil)))
  )

(defmethod -hfql-resolve 'clojure.core/find [[_ key]]
  (bind *value-at-point*
    (fn [value]
      (fmap (fn [resolved-key] (clojure.core/find (original-object value) resolved-key))
        (or (hfql-resolve key) (right key)) ; Hum, isn't it the wrong place to resolve the inner value?
        ))))
(defmethod -hfql-resolve `current-point [[_]] *value-at-point*)
(defmethod -hfql-resolve 'hyperfiddle.hfql2/nth [[_ index]]
  (try (fmap #(nth % index nil) *collection-at-point*)
       (catch #?(:clj Throwable, :cljs :default) t
         (println "Failed to resolve by positional index running (nth % " index ") in collection of type:" (type (either identity identity *collection-at-point*)) "." t)
         nil)))

(def DEFAULT-SUGGESTION (hfql ^::default [identity identify type]))

(extend-protocol Suggestable
  #?(:clj java.util.Map$Entry, :cljs MapEntry)
  (-suggest [_] (hfql [key val]))
  #?(:clj Object, :cljs default)
  (-suggest [_] DEFAULT-SUGGESTION)
  nil (-suggest [_]))

#?(:clj (extend-type Class
          Identifiable (-identify [klass] `(java.lang.Class/forName ~(.getCanonicalName klass)))
          Suggestable (-suggest [_] (hfql [Class/.getCanonicalName Class/.getMethods Class/.getFields]))))

#?(:clj
   (defmethod -hfql-resolve 'java.lang.Class/forName [[_ class-canonical-name]]
     ;; FIXME This should be in userland because there are security concerns/trade-offs here.
     ;; TODO remove this for HFQL core. This is safe to our use case, But may not be acceptable to some users.
     ;; I keeping this here for now because of demos.
     (try
       (java.lang.Class/forName class-canonical-name
         false ; don't run static initializers, they aren't pure.
         ;; scope resolution to the Thread Context Class Loader (TCCL).
         ;; In a clojure app the TCCL is *usually* a clojure.lang.DynamicClassLoader and *may* default to clojure.lang.RT/baseLoader
         ;; (type (clojure.lang.RT/baseLoader)) := clojure.lang.DynamicClassLoader
         ;; The TCCL is often set/configured by the app server, container or managed environment setup.
         ;; I picked the TCCL here because:
         ;; - in our demos the TCCL is Clojure's DynamicClassLoader – this is safe to our use case.
         ;; - users should be able to customize the class loader. An idiomatic way to do so is to set the TCCL with Thread/.setContextClassLoader.
         (.getContextClassLoader (Thread/currentThread))
         )
       (catch ClassNotFoundException _
         nil))))


;; Exceptions
#?(:cljs
   (defn- get-stack-trace [ex]
     (when-let [stack (.-stack ^js/Error ex)]
       (cljs-stacktrace/parse-stacktrace {} stack
         (cond
           user-agent-product/CHROME  :chrome
           user-agent-product/FIREFOX :firefox
           user-agent-product/SAFARI  :safari
           user-agent-product/IE      :ie
           :else        :unknown)
         nil))))

(extend-protocol Suggestable
  #?(:clj Throwable, :cljs js/Error)
  (-suggest [_] (hfql [type ex-message ex-data {ex-cause ...} #?(:clj .getStackTrace :cljs get-stack-trace)])))

#?(:clj
   (extend-type java.lang.StackTraceElement
     Suggestable
     (-suggest [_] (hfql [.getClassName .getMethodName .getFileName .getLineNumber .isNativeMethod .getModuleName .getModuleVersion .getClassLoaderName]))))

(defmulti -suggest type)

(defn suggest* [object]
  (try (right (-suggest (original-object object)))
       (catch #?(:clj Throwable, :cljs :default) err
         (left err))))

(defn suggest [object] (unwrap! (suggest* object)))

(defn suggest-map [object]
  ;; TODO render the type in UI if the map-like object is of an interesting type (e.g. IRecords)
  ;; See edge case in `suggest-record` for records with no static fields.
  (hfql ^{::value-type ::map} {* ^{::value-type ::map-entry} [key val]} (seq object)))

(defn- suggest-record [record]
  (let [basis #?(:clj
                 (-> (class record)
                   (.getMethod "getBasis" nil)
                   (.invoke nil nil))
                 :cljs
                 (.getBasis (type record)))
        record-entries (map keyword basis)]
    (if (empty? basis)
      ;; Edge case of a record with no static fields but potentially some extra keys associated to it.
      ;; Real-world example: datomic.fulltext_index.PersistentFulltext accessible under d/db > :memidx > :fulltext
      (suggest-map record)
      (->> (keys record)
        (map (fn [key] (cond (number? key) (list 'quote key) ; raw numbers are interpreted as recursion, not colums.
                             :else key)))
        (remove (set record-entries)) ; won't clash with numbers mapped to quoted numbers – record fields must be symbols.
        (concat record-entries)
        (vec)
        (ana/analyze {})
        (hfql*)))))

(defn- multiplicity-many-value? [object] ; TODO promote to HFQL
  ;; `coll?` is not enough. E.g. datomic.api.EntityMap is `coll?` but isn't `map?`.
  ;; Also we don't want to treat records as coll
  (and (not (record? object))
    (or (sequential? object)
      (set? object)
      (co/enumeration? object) ; TODO move implementation
      (co/iterator? object) ; TODO move implementation
      #?(:clj (contrib.data/array? object))
      #?(:clj (instance? java.util.Collection object)))))

(defn suggest-collection [coll] (hfql {* [*]} (co/to-seq coll)))

#?(:clj
   (def ^:private discover-methods-for-class
     "Memoized by class so that the same fn objects are returned for the same class.
      This ensures stable HFQL IDs (compute-id hashes ::form which contains the fn)
      across ColumnPicker (samples first item) and MatrixRow (per-row augmentation).
      See also `type-column` which must be a singleton for the same reason."
     (memoize
       (fn [^Class clazz]
         (let [obj-methods (set (map #(.getName %) (.getMethods Object)))
               ;; Prefer Method objects from public interfaces over the concrete class.
               ;; .getMethods on the impl class returns Methods bound to the declaring class;
               ;; on modular JDK classes (Java 9+), internal classes like
               ;; sun.management.BaseOperatingSystemImpl are not exported, so .invoke throws
               ;; IllegalAccessException even after .setAccessible. Methods resolved from
               ;; public interfaces are always accessible.
               interface-methods (->> (.getInterfaces clazz)
                                   (mapcat #(.getMethods ^Class %))
                                   (into {} (map (fn [^java.lang.reflect.Method m] [(.getName m) m]))))
               all-methods (->> (.getMethods clazz)
                             (into {} (map (fn [^java.lang.reflect.Method m] [(.getName m) m]))))]
           (->> (merge all-methods interface-methods) ; interface methods win
             (vals)
             (filter (fn [^java.lang.reflect.Method m]
                       (and (zero? (.getParameterCount m))
                            (not= Void/TYPE (.getReturnType m))
                            (not (obj-methods (.getName m))))))
             (mapv (fn [^java.lang.reflect.Method m]
                     (try (.setAccessible m true) (catch Exception _))
                     (let [sym (symbol (str "." (.getName m)))]
                       (with-meta
                         (fn [obj]
                           ;; Invoke on a separate thread to avoid deadlocks with framework
                           ;; internals (e.g. Jetty holds request-handling locks; a reflected
                           ;; method that acquires the same locks will deadlock on this thread
                           ;; but succeed on a fresh thread that doesn't hold those locks).
                           ;; Uses Clojure's cached daemon thread pool; timeout prevents
                           ;; permanent hangs if a true cross-thread deadlock occurs.
                           ;; Timeout returns nil; real exceptions propagate for UI display.
                           (let [result (deref (future (.invoke m obj (object-array 0))) 50 ::timeout)]
                             (when-not (= ::timeout result) result)))
                         {::label sym}))))))))))

#?(:clj
   (defn- discover-methods
     "Discover zero-arg public methods on a Java object, returning callable accessor
      functions with ::label metadata for column display. Uses reflection to invoke
      methods at runtime, bypassing the need for compile-time method resolution."
     [object]
     (discover-methods-for-class (class object))))

#?(:clj
   (def ^:private type-column
     "Singleton fn for the `type` column in Java object suggestions.
      Why a fn and not a symbol? `build-hfql` calls `ana/analyze` at runtime (not the
      `hfql` macro), so we can't pass the symbol 'type — the analyzer would classify it
      as ::symbol and the coalgebra would (get obj 'type) instead of (type obj).
      We need ::fn / get-or-call semantics, which requires passing a function value.
      Why a def? compute-id hashes ::form (the fn object) to produce HFQL IDs.
      ColumnPicker and each MatrixRow both call suggest-java-object independently —
      if each call created a fresh (with-meta type ...), the IDs wouldn't match and
      find-cell-by-column* would return nil (cell not found → blank column, broken nav)."
     (with-meta type {::label 'type})))

#?(:clj
   (defn- suggest-java-object
     "Build a dynamic HFQL suggestion from discovered zero-arg methods."
     [object]
     (let [methods (discover-methods object)]
       (if (seq methods)
         (build-hfql (into [type-column] methods))
         DEFAULT-SUGGESTION))))

#?(:clj
   (defn- java-object?
     "True if obj is a Java object (not a Clojure type) that may have discoverable methods."
     [object]
     (and (not (instance? clojure.lang.IObj object))
          (not (string? object))
          (not (number? object))
          (not (boolean? object))
          (not (char? object)))))

(defmethod -suggest :default [object]
  (let [user-suggestion (hyperfiddle.hfql2.protocols/-suggest object)]
    (if (or (nil? user-suggestion) (::default (metas user-suggestion)))
      (cond
        (record? object) (suggest-record object)
        (multiplicity-many-value? object) (suggest-collection object)
        (imap? object) (suggest-map object)
        #?@(:clj [(java-object? object) (suggest-java-object object)])
        :else user-suggestion)
      user-suggestion)))

(comment
  _suggested
  (runCofreeT _suggested)
  ;; (pull (fmap #(assoc % ::suggested true) _suggested))
  ;; ((fn rec [T]
  ;;    (hyperfiddle.ct.cofree/->CofreeT
  ;;      (hyperfiddle.ct.bifunctor/bimap #(assoc % ::suggested true)
  ;;        (hyperfiddle.ct.comonad/extend (comp extract rec))
  ;;        (runCofreeT T)))) _suggested)
  (hyperfiddle.ct.cofree/hoistCofreeT (bf/first #(assoc % ::suggested true))
    _suggested)
  (map (comp ::suggested metas) (unwrap *1))
  )


(defn- suggest-at-point! [hfqlT]
  (->> (value-at-point hfqlT)
    (=<< suggest*)
    (either
      (fn [err] (println "Failed to suggest columns:" (ex-message err)) nil)
      (fn [suggestedT]
        (if (some? suggestedT)
          (hoistHfqlT (bf/first #(assoc % ::suggested true))
            suggestedT) ; recursively map over CofreeT structure - lazily
          hfqlT)))))

(declare parent set-parent)

(let [id (comp ::ana/id metas)
      children-ids (comp set (fmap id) ::co/static-children metas)]
  (defn augment-with-suggestions! [hfqlT]
    (when hfqlT
      (-> (if-let [suggestedT (suggest-at-point! hfqlT)]
            (if (-> (::default (metas suggestedT))    ; default was suggested
                  (and (::ana/has-star? (metas hfqlT))) ; but static expression has a *
                  (and (> 1 (count (children-ids hfqlT))))) ; so if there's more static children than just *, skip rendering defaults
              hfqlT
              (let [suggested? (set/difference (children-ids suggestedT) (children-ids hfqlT))
                    hfqlT-type (::ana/type (metas hfqlT))
                    suggestedT-type (::ana/type (metas suggestedT))]
                (cond
                  (empty? suggested?) hfqlT

                  (= ::ana/pull hfqlT-type suggestedT-type)
                  (hoistHfqlT (bf/first (fn [metas] ; FIXME must rewalk and tag because `combine` loses static metadata.
                                          (assoc metas ::suggested (boolean (suggested? (::ana/id metas))))))
                    (combine hfqlT suggestedT)) ; FIXME loses static metadata :(
                  (and (= ::ana/pull hfqlT-type)
                    (= '[*] (::ana/form (metas hfqlT))))
                  ;; [*] pull but suggestion isn't a pull (e.g. collection → traversal).
                  ;; Sample first item to discover per-item columns (e.g. Java methods).
                  ;; Wrapped in try-catch: sampling can fail on objects with side-effectful
                  ;; methods (e.g. Jetty connectors deadlock when reflected during request handling).
                  (let [item-suggestion
                        (try
                          (let [coll (from-right nil (value-at-point hfqlT))]
                            (cond
                              (multiplicity-many-value? coll)
                              (when-let [sample (first coll)]
                                (let [s (from-right nil (suggest* sample))]
                                  (cond
                                    (and s (= ::ana/pull (::ana/type (metas s)))) s
                                    (imap? sample) (build-hfql (vec (keys sample))))))
                              ;; value-at-point is already a map (e.g. {*} traversal fanned out
                              ;; a collection of maps, [*] sees one map directly)
                              (imap? coll)
                              (build-hfql (vec (keys coll)))))
                          (catch #?(:clj Throwable, :cljs :default) _ nil))]
                    (if (and item-suggestion (= ::ana/pull (::ana/type (metas item-suggestion))))
                      (let [suggested? (set/difference (children-ids item-suggestion) (children-ids hfqlT))]
                        (hoistHfqlT (bf/first (fn [m] (assoc m ::suggested (boolean (suggested? (::ana/id m))))))
                          (combine hfqlT item-suggestion)))
                      (update-scope DEFAULT-SUGGESTION (constantly (scope hfqlT)))))
                  (= ::ana/pull hfqlT-type) hfqlT
                  (= ::ana/pull suggestedT-type) (update-scope suggestedT (constantly (scope hfqlT)))
                  :else hfqlT
                  )))
            hfqlT)
        (co/vary-static-meta #(merge (metas hfqlT) %)) ; FIXME don't lose static metadata
        (set-parent (parent hfqlT))))))

(comment
  (import [java.io File])
  (augment-with-suggestions!
    (seed {'% (clojure.java.io/file ".")}
      (hfql [File/.getName {File/.listFiles ...}])))
  (unwrap *1)
  )

(defmethod -resolve-dispatch :default [object]
  (if (seq? object)
    (first object)
    object))

;; Safe comparison protocol - prevents blowup on infinite lazy seqs and uncomparable objects.
;; Collections return [type-name, count-or-max] for grouping by type + safe ordering by size.
;; Unknown objects return type name only (never the raw object).
(defn comparable [object] (hyperfiddle.hfql2.protocols/-comparable object))

(extend-protocol ComparableRepresentation
  nil (-comparable [_] nil)

  ;; Safe primitives - direct comparison
  #?(:clj String, :cljs string) (-comparable [this] this)
  #?(:clj Number, :cljs number) (-comparable [this] this)
  #?(:clj Boolean, :cljs boolean) (-comparable [this] this)
  #?(:clj clojure.lang.Symbol, :cljs cljs.core/Symbol) (-comparable [this] (str this))
  #?(:clj clojure.lang.Keyword, :cljs cljs.core/Keyword) (-comparable [this] (str (namespace this) "/" (name this)))

  #?@(:clj [java.lang.Class (-comparable [^Class this] (.getName this))
            java.util.UUID (-comparable [this] (str this))
            java.util.Date (-comparable [this] (.getTime ^java.util.Date this))])

  #?@(:cljs [cljs.core/UUID (-comparable [this] (str this))])

  ;; Either - [group, value] to keep Rights and Lefts separate
  ;; Rights sort first (prefixed with 0), Lefts sort last (prefixed with 1)
  #?@(:clj [hyperfiddle.ct.either.Right
            (-comparable [this] ["0:right" (comparable (from-right nil this))])
            hyperfiddle.ct.either.Left
            (-comparable [this] ["1:left" (comparable (from-left nil this))])]

      :cljs [hyperfiddle.ct.either/Right
             (-comparable [this] ["0:right" (comparable (from-right nil this))])
             hyperfiddle.ct.either/Left
             (-comparable [this] ["1:left" (comparable (from-left nil this))])])

  ;; Collections - [type-name, count-or-max] for grouping + safe ordering
  ;; Counted collections sort by count within type group, uncounted sort last
  #?@(:clj [clojure.lang.Seqable
            (-comparable [coll]
              [(.getName (class coll))
               (if (counted? coll) (count coll) Long/MAX_VALUE)])]

      :cljs [cljs.core/PersistentVector
             (-comparable [coll] [(pr-str (type coll)) (count coll)])
             cljs.core/PersistentHashMap
             (-comparable [coll] [(pr-str (type coll)) (count coll)])
             cljs.core/PersistentArrayMap
             (-comparable [coll] [(pr-str (type coll)) (count coll)])
             cljs.core/PersistentHashSet
             (-comparable [coll] [(pr-str (type coll)) (count coll)])
             cljs.core/List
             (-comparable [coll] [(pr-str (type coll)) (count coll)])
             cljs.core/LazySeq
             (-comparable [coll] [(pr-str (type coll)) js/Number.MAX_SAFE_INTEGER])])

  ;; Default: type name only - safe, groups by type, never inspects object
  ;; Counted objects and arrays get [type-name, count] like collections
  #?(:clj Object, :cljs default)
  (-comparable [this]
    #?(:clj (let [clazz (class this)
                  type-name (.getName clazz)]
              (cond
                (counted? this) [type-name (count this)]
                (.isArray clazz) [type-name (alength this)]
                :else type-name))
       :cljs (let [type-name (pr-str (type this))]
               (cond
                 (counted? this) [type-name (count this)]
                 (array? this) [type-name (alength this)]
                 :else type-name)))))

(defn- describe-dispatch-fn [object]
  (cond (satisfies? hyperfiddle.ct.cofree/ComonadCofree object)
        (::ana/type (metas object))
        (map? object) (or (::ana/type object) (type object))
        :else (type object)))

(defmulti describe describe-dispatch-fn)
(defmethod describe :default [_] nil)
(defmethod describe ::ana/symbol  [stepT] (metas stepT))
(defmethod describe ::ana/keyword [stepT] (metas stepT))
(defmethod describe ::ana/string  [stepT] (metas stepT))
(defn- describe-call [metas] (assoc metas ::symbolic-edge (symbolic-edge* metas)))
(defmethod describe ::ana/call   [stepT] (describe-call (metas stepT)))
(defmethod describe ::ana/traversal [stepT] (describe-call (::ana/edge (metas stepT))))

(comment
  (describe nil)
  (describe "")
  (describe (hfql ns-name))
  )

(defn- format [fmt & args]
  #?(:clj (apply clojure.core/format fmt args)
     :cljs (apply gstring/format fmt args)))

(defmulti describe-formatted describe-dispatch-fn)
(defmethod describe-formatted :default [_] "")

(defmethod describe-formatted ::ana/symbol [hfqlT]
  (let [{::ana/keys [form form-resolved var]} (describe hfqlT)]
    (cl-format nil "~a~%~{~a~^~%~}~@[~%~a~]" (or form-resolved form) (some-> var meta :arglists) (some-> var meta :doc))))

(defmethod describe-formatted ::ana/keyword [hfqlT] (::ana/form (describe hfqlT)))
(defmethod describe-formatted ::ana/string  [hfqlT] (::ana/form (describe hfqlT)))
(defmethod describe-formatted ::ana/lambda  [hfqlT] (symbolic-edge hfqlT))

(defmethod describe-formatted ::ana/traversal [hfqlT]
  (let [{::keys [symbolic-edge] ::ana/keys [var]} (describe hfqlT)]
    (cl-format nil "~a~%~{~a~^~%~}~@[~%~a~]" symbolic-edge (some-> var meta :arglists) (some-> var meta :doc))))

(comment
  (describe (hfql :db/ident))
  (describe-formatted (hfql :db/ident))
  (describe (get dustingetz.namespace-explorer/sitemap `all-ns))
  (describe-formatted (get dustingetz.namespace-explorer/sitemap `all-ns))
  )

(defpointfree parent (comp ::parent metas))
(defn set-parent [childT parentT] (co/vary-static-meta childT assoc ::parent parentT))
(defpointfree multiplicity-many? (comp #(= ::ana/many %) ::ana/multiplicity metas))

(defn lookup
  "Given an HFQL step object, lookup for a symbolic point in the reachable HFQL scope.
  The reachable HFQL scope is made of:
  - the current step (passed in),
  - ancestor steps, up to the root,
  - multiplicity-1 children steps of ancestors steps.

  Example, in the following HFQL expression:
  ```
  {(shirt-orders) {* [:person/email
                      {[:person/shirt-size ::hfql/options (shirt-sizes :person/gender)] [:db/ident]}
                      {:person/gender [[:db/ident :as 'gender-ident]]}]}}
  ```
  - `(shirt-sizes ...)` can refer to `:person/gender`, but only under the same multiplicity branch.
  - Refering to `:db/ident` from `:person/shirt-size` would unambiguously resolve to the `:db/ident`
    under `:person/gender`. Because it is the sole candidate at the same level as `:person/shirt-size`
    or part of the multiplicity-1 ancestry of `:person/shirt-size`.
  - Refering to `:db/ident` under `:person/shirt-size` would be ambiguous because `:db/ident` is reachable
    through both `:person/shirt-size` and `:person/gender`. In such case, one can refer to the `:as` label,
    assuming the label is unique in the reachable scope.

  Image:
                                                          (shirt-orders) Ⓐ
                                                                │
                                                                * fan out
                        ┌───────────────────────────────────────┴──────────────────────────┬──────   ─   ─   ─
┌───────────────────────┼────────────────────────────────────────┐ ┌───────────────────────┼────────────────────────────────────────┐
│        ┌──────────────┼────────────────────────────────┐       │ │        ┌──────────────┼────────────────────────────────┐       │
│        │              │                                │       │ │        │              │                                │       │
│        │              │          (shirt-sizes x )      │       │ │        │              │          (shirt-sizes x )      │       │
│        │              │             ▲         │        │       │ │        │              │             ▲         │        │       │
│        │              │             │         │        │       │ │        │              │             │         │        │       │
│ :person/email  :person/shirt-size ──┘         └▶:person/gender │ │ :person/email  :person/shirt-size ──┘         └▶:person/gender │
│                       │                                │       │ │                       │                                │       │
│                       │                                │       │ │                       │                                │       │
│                    :db/ident                        :db/ident  │ │                    :db/ident                        :db/ident  │
│                                                                │ │                                                                │
└────────────────────────────────────────────────────────────────┘ └────────────────────────────────────────────────────────────────┘
                        Ⓑ                                                                  Ⓒ

  - Ⓐ produces a collection and `*` fans-out, creating multiple branches.
  - Ⓑ and Ⓒ are branches, isolated from each others.
  - All nodes in Ⓑ can reference Ⓐ.
  - All nodes in Ⓒ can reference Ⓐ.
  - Nodes in Ⓑ cannot reference nodes in Ⓒ, and vice-versa.
  - `:person/shirt-size` references `shirt-sizes`, in turn referencing `:person/gender`.
  - `shirt-sizes` cannot reference `:person/shirt-size`
  - `:person/gender` cannot reference `shirt-sizes`.
  "
  ([hfqlT symbolic-edge]
   (lookup hyperfiddle.hfql2/symbolic-edge* hfqlT symbolic-edge))
  ([keyfn hfqlT symbolic-edge]
   (lookup keyfn hfqlT hfqlT symbolic-edge))
  ([keyfn previously-visitedT currentT symbolic-edge]
   #_(prn "visiting" (::ana/form (metas currentT)))
   (cond
     ;; check if current step has a matching `:as` label.
     ;; `:as` has priority.
     (= symbolic-edge (:as (metas currentT)))
     currentT

     ;; check if current step has the same symbolic-edge
     (= symbolic-edge (keyfn (metas currentT)))
     currentT

     ;; IF current step hasn't been visited already
     ;; AND current step doesn't match neither by symbolic edge nor by :as label
     ;; AND current step is of multiplicity many
     ;; THEN search amongst children
     ;; EXCLUDING previously visited step – to prevent a parent -> child -> parent loop
     (and (not= previously-visitedT currentT)
       (not (multiplicity-many? currentT))
       ;; (not (= :map (::value-type (metas currentT)))) ; FIXME should not apply anymore. Verify FIXME looks like a wart – why do we need this special case? Maps are reshaped as a seq of kvs, and should probably be flagged as multiplicity many. But are they really?
       )
     (->> (unwrap currentT) ; children
       (remove #{previously-visitedT})
       (some #(lookup keyfn previously-visitedT % symbolic-edge)))

     ;; No match and no matching child. Look up the parent scope.
     :else
     (when-let [parentT (parent currentT)]
       (lookup keyfn currentT parentT symbolic-edge)))))


(tests
  (defn _f [x] (prn "IO – f") x)
  (defn _g [x] (prn "IO – g") x)
  (defn _h [x] (prn "IO – h") x)
  (def _hfql (hfql {_f [:a {_g :b} {_h ^{:as 'c} #(:c %)}]} {:a 1, :b 2, :c 3}))
  (def _a (-> _hfql unwrap first unwrap first))               ; no expected IO – check the REPL for a print.
  (def _c (-> _hfql unwrap first unwrap (nth 2) unwrap first)) ; no expected IO – check the REPL for a print.
  (symbolic-edge _a)
  := :a
  (symbolic-edge _c)
  := :c
  (lookup _a :a)
  := _a
  (lookup _a :c)                        ; no expected IO
  := _c
  (nav _a)                              ; will run _f
  (nav (lookup _a :c))                  ; will leverage cached _f and will run _h. _g isn't supposed to run.
  (nav _c)                              ; no expected IO, already cached by `(nav (lookup _a :c))`
  := 3
  )

(tests
  (def john {:person/first-name "John", :person/last-name "Doe"})
  (def query (hfql {john [:person/first-name]}))
  (values query)
  := '({:person/first-name "John", :person/last-name "Doe"}
       {:person/first-name "John"}
       "John")

  (pull query)
  := {'john {:person/first-name "John"}} ; note "john" isn't quoted
  (get-in *1 ['john :person/first-name])
  := "John"
  )

;;; Misc

(defn unchunk-seq ; keeping around for reference
  "Turn a clojure seqable into a one-step-at-a-time seq. Preserves eventual
  metadata. Must be called on the original chunked seq. Calling `unchunk-seq` on
  any descendent seq won't have any effect. E.g. `(unchunk-seq (range 3))` –
  turns a chunked LongRange into a proper lazy-seq. But `(unchunk-seq (map
  prn (range 3)))` will still print 0 1 2 because `map` eagerly preprocesses the
  first chunk.

  Use case: prevent undesired side effects while building the cofree tree. We
  don't want to realize any child value when computing fan-out cardinality (e.g. '*).
  Example: In {range {'* inc}}, given `range` receives 3, `range` will produce a
  chunked LongRange. Any downstream `map`, `filter`, `for` etc. will pre-process
  the first chunk and eventually call `inc`. This is not acceptable because in
  the general case `inc` may not be a pure function.

  Note range is a good example because: `range` may or may not return a chunked seq:
  - (type (range)) := clojure.lang.Iterate – not chucked, fully lazy
  - (type (range 1)) or (range 1 10 1) := LongRange – chunked
  - (type (range 1 1)) := PersistentList$EmptyList

  Also see `realize`.
  "
  [aseq]
  (if (or (chunked-seq? aseq) (vector? aseq)) ; calling seq on a vector produces a chunked-seq
    (with-meta
      (lazy-seq
        (when-let [s (seq aseq)]
          (cons (first s) (unchunk-seq (rest s)))))
      (meta aseq))
    aseq))

#?(:clj
   (tests
     "Static method syntax support"
     ;; Original failing attempt, now passing
     (pull (hfql {(java.lang.management.ManagementFactory/getOperatingSystemMXBean)
                  [.getArch .getAvailableProcessors]}))
     := {'(java.lang.management.ManagementFactory/getOperatingSystemMXBean)
          {'.getArch _, '.getAvailableProcessors _}}


     ;; another made up (but valid) case, 2026-01-08 G: AFAIKT no user attempt this one so far.
     (nav (hfql [identity java.util.Date/new] 1767883504555))
     := '{identity 1767883504555,
          java.util.Date/new #inst "2026-01-08T14:45:04.555-00:00"}

     ))


(defn recursion-end?! ; ! because this runs IO
  "State if the given point is a recursion point and if recursion should stop. Recursion stops on nil values.
  At the HFQL tree-seqing level, recuring forever on nil is allowed because `nil` is not treated as a special value.
  But we haven't found a valid use case to recur on nil in a UI."
  [hfqlT]
  (let [metas (metas hfqlT)]
    (or (and (::ana/recursion-root metas)
          (= (right nil) (value-at-point hfqlT)))
      (and (::ana/recursion-point metas)
        (= (right nil) (nav* hfqlT))))))

(comment

  (pull (hfql [ex-message {ex-cause ...}] (ex-info "err" {})))

  )

(defn- supports-nth? [obj]
  (and (some? obj)
    #?(:clj ;; from clojure.lang.RT/nth and nthFrom
       (or (instance? clojure.lang.Indexed obj)
         (instance? java.lang.CharSequence obj)
         (.isArray (.getClass obj))
         (instance? java.util.RandomAccess obj)
         (instance? java.util.regex.Matcher obj)
         (map-entry? obj)
         (sequential? obj))
       :cljs ;; from https://cljs.github.io/api/cljs.core/nth
       (or (satisfies? IIndexed obj)
         (string? obj)
         (array? obj)
         (or (implements? ISeq obj)
           (implements? ISequential obj))
         (native-satisfies? IIndexed obj)))))

(defmethod hyperfiddle.hfql2.protocols/--datafy :default [obj]
  (let [dispatched (hyperfiddle.hfql2.protocols/-datafy obj)] ; protocol dispatch has priority
    (if (identical? obj dispatched)
      (cond
        (and (seqable? obj) (not (supports-nth? obj)) (not (associative? obj)))
        (try
          ;; `seqable?`'s contract says: "return true if `seq` is supported".
          ;; `seqable?` is true for all libpython-clj's objects. But seq may still fail on some of them.
          ;; Libpython-clj breaks the contract by defining all objects as Iterable.
          ;; Other types may break the contract. We decide the safest thing to
          ;; do is to return the object as-is and let users decide.
          (seq obj)
          (catch #?(:clj Throwable, :cljs :default) _
            obj))
        ;; TODO what about records?
        :else obj)
      dispatched)))

#?(:clj (defmethod hyperfiddle.hfql2.protocols/--datafy java.util.Iterator [obj] (iterator-seq obj)))
#?(:clj (defmethod hyperfiddle.hfql2.protocols/--datafy java.util.Enumeration [obj] (enumeration-seq obj)))
#?(:clj (prefer-method hyperfiddle.hfql2.protocols/--datafy java.util.Iterator java.util.Enumeration)) ;; ZipFile$ZipEntryIterator is both an Iterator and Enumeration.

#?(:clj
   (extend-protocol Datafiable
     java.util.Spliterator
     (-datafy [sp] (iterator-seq (java.util.Spliterators/iterator sp)))

     java.util.stream.BaseStream
     ;; streams are Closable, who's in charge of closing them? when?
     (-datafy [s] (iterator-seq (.iterator s))) ; clojure 1.12 has `stream-seq!` but we chose retrocompat.
     ))

#?(:cljs
   (extend-protocol Datafiable
     ES6Iterator
     (-datafy [o] (es6-iterator-seq o))))

;;; Safe in-memory search

(defn search-matches [object needle] (hyperfiddle.hfql2.protocols/search-matches object needle))

(defn- safe-str-match
  "Match a string against needle, truncating to MAX-STR-LEN."
  [s needle]
  (strx/includes-str? (subs s 0 (min (count s) *search-max-str-len*)) needle))

(defn- some-interruptible
  "Like `some`, but checks for thread interruption between items on JVM."
  [pred coll]
  #?(:clj (loop [s (seq coll)]
            (when (and s (not (Thread/interrupted)))
              (or (pred (first s))
                  (recur (next s)))))
     :cljs (some pred coll)))

(defn- search-matches-map? [m needle]
  (and (counted? m)
    (boolean
      (or ; scan keyset first then scan values – reasoning is keys are quick to match. Values less so.
        (some-interruptible #(search-matches % needle) (take *search-max-collection-size* (keys m)))
        (some-interruptible #(search-matches % needle) (take *search-max-collection-size* (vals m))) ; same order as keys is guaranteed.
        ))))

(defn- search-matches-collection? [coll needle]
  (and
    (counted? coll)
    (seqable? coll)
    (boolean (some-interruptible #(search-matches % needle) (take *search-max-collection-size* coll)))))

(extend-protocol Searchable
  nil
  (-search-matches [_ _needle] #_(= "nil" needle) false) ; Enable if searching for nil is desirable.

  #?(:clj Throwable :cljs js/Error)
  (-search-matches [ex needle]
    (boolean
      (or (-search-matches (ex-message ex) needle)
          (search-matches (ex-data ex) needle)
          (search-matches (ex-cause ex) needle))))

  #?(:clj String :cljs string)
  (-search-matches [s needle] (safe-str-match s needle))

  #?(:clj clojure.lang.Keyword :cljs cljs.core/Keyword)
  (-search-matches [k needle] (-search-matches (str k) needle))

  #?(:clj clojure.lang.Symbol :cljs cljs.core/Symbol)
  (-search-matches [s needle] (-search-matches (str s) needle))

  #?(:clj Number :cljs number)
  (-search-matches [n needle] (-search-matches (str n) needle))

  #?(:clj Boolean :cljs boolean)
  (-search-matches [b needle] (-search-matches (str b) needle))

  ;; Maps: recurse into keys and values
  #?@(:clj [clojure.lang.IPersistentMap
            (-search-matches [m needle] (search-matches-map? m needle))])

  #?@(:cljs [cljs.core/PersistentHashMap
             (-search-matches [m needle] (search-matches-map? m needle))])
  #?@(:cljs [cljs.core/PersistentArrayMap
             (-search-matches [m needle] (search-matches-map? m needle))])

  #?@(:clj [java.util.UUID
            (-search-matches [u needle] (-search-matches (str u) needle))

            java.util.Date
            (-search-matches [d needle] (-search-matches (str d) needle))

            Class
            (-search-matches [c needle] (-search-matches (.getCanonicalName c) needle))

            ;; Other collections: recurse into elements
            clojure.lang.IPersistentCollection
            (-search-matches [coll needle] (search-matches-collection? coll needle))

            ;; Default: match against type name only (never prints object)
            Object
            (-search-matches [o needle] (-search-matches (class o) needle))]

      :cljs [cljs.core/UUID
             (-search-matches [u needle] (-search-matches (str u) needle))

             js/Date
             (-search-matches [d needle] (-search-matches (str d) needle))

             ;; Vectors
             cljs.core/PersistentVector
             (-search-matches [coll needle] (search-matches-collection? coll needle))

             ;; Lists
             cljs.core/List
             (-search-matches [coll needle] (search-matches-collection? coll needle))

             ;; Sets
             cljs.core/PersistentHashSet
             (-search-matches [coll needle] (search-matches-collection? coll needle))

             ;; Default: match against type name only (never prints object)
             default
             (-search-matches [o needle] (-search-matches (pr-str (type o)) needle))]))
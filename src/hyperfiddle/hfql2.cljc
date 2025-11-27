(ns hyperfiddle.hfql2
  (:refer-clojure :exclude [format])
  (:require
   [clojure.core :as cc]
   [clojure.core.protocols :as ccp]
   [clojure.datafy]
   [clojure.pprint :refer [cl-format]]
   [clojure.set :as set]
   [hyperfiddle.rcf :refer [tests]]
   [hyperfiddle.ct.monoid :refer [mappend]]
   [hyperfiddle.ct.functor :refer [fmap]]
   [hyperfiddle.ct.function :refer [flip]]
   [hyperfiddle.ct.bifunctor :as bf]
   [hyperfiddle.ct.pair :refer [pair fst snd swap]]
   [hyperfiddle.ct.comonad :refer [extract]]
   [hyperfiddle.ct.cofree :refer [tree-seqT unwrap runCofreeT]]
   [hyperfiddle.ct.rose-tree :refer [rose-tree]]
   [hyperfiddle.ct.store :refer [pos peeks]]
   [hyperfiddle.ct.experiments.thunk :refer [force-thunk]]
   [hyperfiddle.hfql2.analyzer :as ana]
   [hyperfiddle.hfql2.coalgebra :as co :refer [unfoldT hfqlT hoistHfqlT]]
   [hyperfiddle.hfql2.protocols :refer [Identifiable Suggestable ComparableRepresentation hfql-resolve resolve-dispatch]]
   [hyperfiddle.hfql2.utils :refer [has-meta?]]
   #?(:cljs [goog.string :as gstring])
   #?(:cljs [goog.string.format]))
  #?(:cljs (:require-macros [hyperfiddle.hfql2 :refer [hfql defpointfree]])))

(defmacro defpointfree
  "Like (def foo (comp a b)) but improve stacktraces by printing `foo` instead of comp_XYZ."
  {:clj-kondo/lint-as 'clojure.core/def}
  [name f]
  `(defn ~name [point#] (~f point#)))

(def seed co/seed)
(def nav* extract)
(defpointfree nav (comp force-thunk nav*))
(defpointfree pull* (comp extract co/pull))
(defpointfree pull (comp force-thunk pull*))
(defpointfree next-steps unwrap)
(defpointfree values (comp (fmap (comp force-thunk nav)) tree-seqT))

(defpointfree scope (comp pos snd runCofreeT))
(defpointfree value-at-point (comp force-thunk '% scope))
(defpointfree seeded? (comp (flip contains? '%) scope))
(defn update-scope [stepT f & args] (seed (apply f (scope stepT) args) stepT))
(defpointfree metas (comp fst runCofreeT))
(defpointfree pull-has-star? (comp ::ana/has-star? metas))
(defpointfree free-call? (comp ana/free-call? metas))
(defpointfree fullfilled? (some-fn seeded? free-call?))

(declare unchunk-seq)
(defn navigable [nav-fn object]
  (vary-meta (unchunk-seq object) assoc `hyperfiddle.hfql2.protocols/nav (#?(:clj bound-fn, :cljs fn) [_coll key value] (nav-fn key value))))

(defn- safe-vary-meta [object f & args]
  (if (has-meta? object)
    (apply vary-meta object f args)
    object))

(defn filtered [object] (safe-vary-meta object assoc ::filtered true))
(defn filtered? [object] (::filtered (meta object)))

(defn unfold [ast] (unfoldT (partial hfqlT ast) ast))
(defn hfql* [analyzed-hfql] #_(seed {}) (unfold analyzed-hfql))

(defn build-hfql
  ([hfql-expression] (build-hfql {} hfql-expression))
  ([&env hfql-expression]
   (unfold (ana/analyze (or &env {}) hfql-expression))))

(defmacro hfql
  ([hfql-form] `(hfql* (ana/hfql ~hfql-form)))
  ([hfql-form x] `(seed {~''% ~x} (hfql ~hfql-form))))



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
  (pull (seed {'% [{:a 1, :b 2} {:a 3, :c 4}]} (hfql {* [:a]})))
  := '{nil ({:a 1} {:a 3})} ; malformed pull! '* is not an edge. You may wand to (map pull) on children. Pull-many isn't implemented.

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
      :hyperfiddle.hfql2.analyzer/has-star? false}
  )

(defn symbolic-edge* [metas]
  (::ana/symbolic-edge
   (case (::ana/type metas)
     ::ana/traversal (::ana/edge metas)
     metas)))

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
         (unfold (case (::ana/type (metas hfqlT-a))
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
      :hyperfiddle.hfql2.analyzer/has-star? false})

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
    (identify (value-at-point hfqlT))))

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

(comment
  (def _rows (unwrap (seed {'% (clojure.java.io/file ".")} (dustingetz.file-explorer/sitemap `dustingetz.file-explorer/dir-list))))
  (def _columnT (first (unwrap (first _rows))))
  (find-cell-by-column _columnT (second _rows))
  )

;;;;;;;;;;;;;;;
;; Protocols ;;
;;;;;;;;;;;;;;;

(extend-protocol Identifiable
  #?(:clj clojure.lang.AMapEntry, :cljs MapEntry) (identify [entry] (key entry))
  #?(:clj Object, :cljs default) (identify [_])
  nil (identify [_]))

(defn identifiable [identify-fn object]
  (vary-meta object assoc `hyperfiddle.hfql2.protocols/identify (#?(:clj bound-fn*, :cljs identity) identify-fn))) ; #?(:clj bound-fn* :cljs identity)

(defn identify
"Produces a symbolic identity for `obj`, or nil if `obj` does not explicitly
implement `Identifiable`. Serializable and uniquely resolvable symbolic
identities should be favored.
All values being self-identical, `(or (identify x) x)` always yields a valid
identifier, though it might not be serializable."
  [obj]
  (hyperfiddle.hfql2.protocols/identify obj))

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

(defn identifiable? [object]
  #_(extends? Identifiable (type object))
  #?(:clj (extends? Identifiable (type object))
     :cljs (and (not (nil? object))
             (object? object)
             (satisfies? Identifiable object))))

(def DEFAULT-SUGGESTION (hfql [identity identify type]))

(extend-protocol Suggestable
  #?(:clj clojure.lang.IPersistentMap, :cljs IMap) ; not sure if IMap is the right protocol, this is assuming all maps are seqable
  (suggest [m]
    (co/vary-static-meta
      ;; (hfql {* [key val]}) ; not compatible – `combine` can only combine two pulls
      (hfql* (ana/analyze {} (vec (keys m))))
      #(assoc % ::value-type :map))) ; UI may interpret this to customize table layout
  #?(:clj clojure.lang.AMapEntry, :cljs MapEntry)
  (suggest [_]
    (co/vary-static-meta
      (hfql [key val])
      #(assoc % ::value-type :map-entry))) ; UI may interpret this to customize table layout
  #?(:clj Object, :cljs default)
  (suggest [_] DEFAULT-SUGGESTION)
  nil (suggest [_]))

#?(:clj (extend-type Class
          Suggestable (suggest [_] (hfql [Class/.getCanonicalName Class/.getMethods Class/.getFields]))))


(defmulti suggest type)
(defmethod suggest :default [object] (hyperfiddle.hfql2.protocols/suggest object))
(defmethod suggest #?(:clj clojure.lang.IRecord, :cljs IRecord) [record]
  (hfql*
    (ana/analyze {} (vec
                      (map keyword
                        #?(:clj
                           (-> (type record)
                             (.getMethod "getBasis" nil)
                             (.invoke nil nil))
                           :cljs
                           (.getBasis (type record))))))))

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
  (if-let [suggestedT (suggest (force-thunk (value-at-point hfqlT)))]
    (let [default? (= DEFAULT-SUGGESTION suggestedT)]
      (hoistHfqlT (bf/first #(cond-> (assoc % ::suggested true)
                               default? (assoc ::default true)))
        suggestedT)) ; recursively map over CofreeT structure - lazily
    hfqlT))

(let [id (comp ::ana/id metas)
      children-ids (comp set (fmap id) ::co/static-children metas)]
  (defn augment-with-suggestions! [hfqlT]
    (when hfqlT
      (if-let [suggestedT (suggest-at-point! hfqlT)]
        (if (-> (::default (metas suggestedT)) ; default was suggested
              (and (::ana/has-star? (metas hfqlT))) ; but static expression has a *
              (and (> 1 (count (children-ids hfqlT))))) ; so if there's more static children than just *, skip rendering defaults
          hfqlT
          (let [suggested? (set/difference (children-ids suggestedT) (children-ids hfqlT))
                hfqlT-type (::ana/type (metas hfqlT))
                suggestedT-type (::ana/type (metas suggestedT))]
            (cond
              (empty? suggested?) hfqlT

              (= ::ana/pull hfqlT-type suggestedT-type)
              (hoistHfqlT (bf/first (fn [metas] ; FIXME must rewalk and tag because `combine` looses static metadata.
                                        (assoc metas ::suggested (boolean (suggested? (::ana/id metas))))))
                (combine hfqlT suggestedT))  ; FIXME looses static metadata :(
              (= ::ana/pull hfqlT-type) hfqlT
              (= ::ana/pull suggestedT-type) (update-scope suggestedT (constantly (scope hfqlT)))
              :else hfqlT
              )))
        hfqlT))))

(comment
  (import [java.io File])
  (augment-with-suggestions!
    (seed {'% (clojure.java.io/file ".")}
      (hfql [File/.getName {File/.listFiles ...}]))) 
  (unwrap *1)
  )

(defmethod resolve-dispatch :default [object]
  (if (seq? object)
    (first object)
    object))

(extend-protocol ComparableRepresentation
  #?(:clj Object, :cljs default) (comparable [this] this)
  nil (comparable [_] nil)
  #?@(:clj [java.lang.Class (comparable [^Class this] (.getName this))])
  #?(:clj clojure.lang.Symbol, :cljs cljs.core.Symbol) (comparable [this] (str this))
  #?(:clj clojure.lang.Keyword, :cljs cljs.core.Keyword) (comparable [this] (str (namespace this) "/" (name this))) ; drop ":"
  )

(defn comparable [object] (hyperfiddle.hfql2.protocols/comparable object))

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
   (lookup hfqlT hfqlT symbolic-edge))
  ([childT hfqlT symbolic-edge]
   (cond
     ;; check current node
     (= symbolic-edge (hyperfiddle.hfql2/symbolic-edge hfqlT))
     hfqlT

     ;; if current node is not the previous node, lookup multiplicity-1 children, excluding previous node.
     (and (not= childT hfqlT)
       (not= ::ana/many (::ana/multiplicity (meta hfqlT))))
     (->> (unwrap hfqlT) ; ⚠ may not leverage cached tree – TODO check
       (remove #{childT})
       (some #(lookup childT % symbolic-edge)))

     :else ;; look up the parent
     (when-let [parentT (::parent (metas hfqlT))]
       (lookup hfqlT parentT symbolic-edge)))))


(comment
  (defn _f [x] (prn "IO – f") x)
  (defn _g [x] (prn "IO – g") x)
  (def _hfql (hfql {_f [:a {_g :b}]} {:a 1, :b 2}))
  (def _a (-> _hfql unwrap first unwrap first)) ; no expected IO
  (symbolic-edge _a)
  (lookup _a :a)
  (lookup _a :b) ; no expected IO
  (nav _a) ; will run _f
  (nav (lookup _a :b)) ; will leverage cached _f and will run _g
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
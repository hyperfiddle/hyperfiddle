(ns hyperfiddle.hfql2
  (:refer-clojure :exclude [resolve format])
  (:require
   [clojure.core :as cc]
   [clojure.core.protocols :as ccp]
   [clojure.pprint :refer [cl-format]]
   [hyperfiddle.rcf :refer [tests]]
   [hyperfiddle.hfql1.monoid :refer [mappend]]
   [hyperfiddle.hfql1.functor :refer [fmap]]
   [hyperfiddle.hfql1.bifunctor :as bf :refer [pair fst snd swap]]
   [hyperfiddle.hfql1.comonad :refer [extract]]
   [hyperfiddle.hfql1.cofree :refer [tree-seqT unwrap runCofreeT]]
   [hyperfiddle.hfql1.rose-tree :refer [rose-tree]]
   [hyperfiddle.hfql1.store :refer [pos peeks]]
   [hyperfiddle.hfql1.analyzer :as ana]
   [hyperfiddle.hfql1.coalgebra :as co :refer [unfoldT hfqlT]]
   #?(:cljs [goog.string :as gstring])
   #?(:cljs [goog.string.format]))
  #?(:cljs (:require-macros [hyperfiddle.hfql2 :refer [hfql]])))


(def seed co/seed)
(def nav* extract)
(def nav (comp force snd nav*))
(def pull* (comp extract co/pull))
(def pull (comp force snd pull*))
(def next-steps unwrap)
(def values (comp (fmap (comp force nav)) tree-seqT))

(defn scope [hfqlT] (pos (snd (runCofreeT hfqlT))))
(defn value-at-point [hfqlT] (get (scope hfqlT) '%))

(def metas (comp fst runCofreeT))

(defn navigable [nav-fn object]
  (vary-meta object assoc `ccp/nav (#?(:clj bound-fn, :cljs fn) [_coll key value] (nav-fn key value))))

(defn unfold [ast] (unfoldT (partial hfqlT ast) ast))
(defn hfql* [analyzed-hfql] #_(seed {}) (unfold (fmap swap analyzed-hfql)))
(defmacro hfql [hfql-form] `(hfql* (ana/hfql ~hfql-form)))



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


(tests
  (def _attribute  {:db/id 42, :db/ident :attribute/name})
  (def _object (with-meta {:a 42} {`ccp/nav (fn [o k v] (if (= :a k) _attribute (get o k v)))}))
  (nav (seed {'% _object} (hfql :a)))
  := _attribute

  (values (seed {'% _object} (hfql {:a :db/ident})))
  := '({:db/id 42, :db/ident :attribute/name} :attribute/name)

  )


(tests
  "pull syntax"
  (values (seed {'% {:a 1}} (hfql []) ))
  := '({})

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

    (force (nav (first (next-steps step)))) ; FIXME computing next-steps should not require `step` to be seeded.
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
    (hfql* (ana/wrap-in-pull (fmap swap (::co/AST (metas stepT)))))))

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
      ::ana/form (#'ana/quote merged-form)
      ::ana/symbolic-edges (#'ana/quote merged-form))))

(tests
  (merge-pull-metas
    (snd
      (extract
        (ana/analyze {'% 1} [:a :b])))
    (snd
      (extract
        (ana/analyze {'% 2} [:a :c]))))
  := {:hyperfiddle.hfql1.analyzer/type :hyperfiddle.hfql1.analyzer/pull,
      :hyperfiddle.hfql1.analyzer/form '(quote [:a :b :c]),
      :hyperfiddle.hfql1.analyzer/lexical-env '{},
      :hyperfiddle.hfql1.analyzer/symbolic-edges '(quote [:a :b :c])}
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
  (let [keyf (comp symbolic-edge* snd extract)
        a-edges (into #{} (map keyf children-A))]
    (into children-A (remove (comp a-edges keyf) children-B))))

(defn- combine-pull [rose-a rose-b]
  (let [metas (merge-pull-metas (snd (extract rose-a)) (snd (extract rose-b)))]
    (fmap (fmap (fn [kv] (assoc kv ::ana/lexical-env (::ana/lexical-env metas))))
      (rose-tree
        (pair (ana/unquote* (::ana/form metas)) metas)
        (merge-pull-children
          (unwrap rose-a) (unwrap rose-b))))))

(defn combine
  ([hfqlT] hfqlT)
  ([hfqlT-a hfqlT-b]
   (if (nil? hfqlT-b)
     hfqlT-a
     (do
       (assert (= (::ana/type (metas hfqlT-a)) (::ana/type (metas hfqlT-b))))
       (seed (merge (scope hfqlT-a) (scope hfqlT-b))
         (unfold (fmap swap
                   (case (::ana/type (metas hfqlT-a))
                     ::ana/pull (combine-pull (fmap swap (::co/AST (metas hfqlT-a))) (fmap swap (::co/AST (metas hfqlT-b))))
                     ::ana/traversal (throw (ex-info "TODO" {}))
                     (throw (ex-info "Can't combine." (::ana/type (metas hfqlT-a)))))))))))
  ([hfqlT-a hfqlT-b & hfqlTs]
   (reduce combine (combine hfqlT-a hfqlT-b) (remove nil? hfqlTs))))

(tests
  (->
    (combine
      (let [a 1]
        (hfql [:a :b]))
      (let [b 2]
        (hfql [:a :c])))
    (metas)
    (dissoc ::co/AST))
  := {:hyperfiddle.hfql1.analyzer/type :hyperfiddle.hfql1.analyzer/pull,
      :hyperfiddle.hfql1.analyzer/form '(quote [:a :b :c]),
      :hyperfiddle.hfql1.analyzer/lexical-env {},
      :hyperfiddle.hfql1.analyzer/symbolic-edges '(quote [:a :b :c]),
      :hyperfiddle.hfql1.coalgebra/static-children _
      :hyperfiddle.hfql1.coalgebra/scope '{a 1, b 2}})

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
            (co/symbolic-edge (extract hfqlT) ::co/point-free false))))
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

(defn find-child-by [pred hfqlT] (first (filter pred (unwrap hfqlT))))
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

(defn identify
"Produces a symbolic identity for `obj`, or nil if `obj` does not explicitly
implement `Identifiable`. Serializable and uniquely resolvable symbolic
identities should be favored.
All values being self-identical, `(or (identify x) x)` always yields a valid
identifier, though it might not be serializable."
  [obj]
  (-identify obj))

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
  #?(:clj (extends? Identifiable object)
     :cljs (and (not (nil? object))
             (object? object)
             (satisfies? Identifiable object))))

(defprotocol Suggestable :extend-via-metadata true
             (-suggest [o]))

(extend-protocol Suggestable
  #?(:clj clojure.lang.IPersistentMap, :cljs IMap) ; not sure if IMap is the right protocol, this is assuming all maps are seqable
  (-suggest [m] (hfql* (ana/analyze {} (vec (keys m)))))
  #?(:clj Object, :cljs default)
  (-suggest [_] (hfql [identity identify type]))
  nil (-suggest [_]))

#?(:clj (extend-type Class
          Suggestable (-suggest [_] (hfql [Class/.getCanonicalName Class/.getMethods Class/.getFields]))))


(defmulti suggest type)
(defmethod suggest :default [object] (-suggest object))
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

(defn suggest-at-point! [hfqlT]
  (if-let [suggestions (suggest (force (value-at-point hfqlT)))]
    (co/vary-static-meta
      (fmap (bf/first #(assoc % ::suggested true)) suggestions)
      #(assoc % ::suggested true))
    hfqlT))

(defn augment-with-suggestions! [hfqlT]
  (when hfqlT
    (combine hfqlT (suggest-at-point! hfqlT))))

(defmulti resolve-dispatch type)
(defmethod resolve-dispatch :default [object]
  (if (seq? object)
    (first object)
    object))

(defmulti resolve resolve-dispatch)
(defmethod resolve :default [object] object) ; don't know how to resolve ≡ identity

(defn- describe-dispatch-fn [object]
  (cond (satisfies? hyperfiddle.hfql1.cofree/ComonadCofree object)
        (::ana/type (metas object))
        (map? object) (or (::ana/type object) (type object))
        :else (type object)))

(defmulti describe describe-dispatch-fn)
(defmethod describe :default [_] nil)
(defmethod describe ::ana/symbol [stepT] (metas stepT))
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

(defmethod describe-formatted ::ana/traversal [hfqlT]
  (let [{::keys [symbolic-edge] ::ana/keys [var]} (describe hfqlT)]
    (cl-format nil "~a~%~{~a~^~%~}~@[~%~a~]" symbolic-edge (some-> var meta :arglists) (some-> var meta :doc))))

(comment
  (describe (get dustingetz.namespace-explorer/sitemap `all-ns))
  (describe-formatted (get dustingetz.namespace-explorer/sitemap `all-ns)))


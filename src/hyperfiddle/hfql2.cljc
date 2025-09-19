(ns hyperfiddle.hfql2
  (:require
   [clojure.core :as cc]
   [clojure.core.protocols :as ccp]
   [hyperfiddle.rcf :refer [tests]]
   [hyperfiddle.hfql1.functor :refer [fmap]]
   [hyperfiddle.hfql1.bifunctor :refer [fst snd swap]]
   [hyperfiddle.hfql1.comonad :refer [extract]]
   [hyperfiddle.hfql1.cofree :refer [tree-seqT unwrap runCofreeT]]
   [hyperfiddle.hfql1.analyzer :as ana]
   [hyperfiddle.hfql1.coalgebra :as co :refer [unfoldT hfqlT]]))


(def seed co/seed)
(def nav* extract)
(def nav (comp force snd nav*))
(def pull* (comp extract co/pull))
(def pull (comp force snd pull*))
(def next-steps unwrap)
(def values (comp (fmap (comp force nav)) tree-seqT))

(def metas (comp fst runCofreeT))

(defn navigable [nav-fn object]
  (vary-meta object assoc `ccp/nav (#?(:clj bound-fn, :cljs fn) [_coll key value] (nav-fn key value))))

(defn unfold [ast] (unfoldT (partial hfqlT ast) ast))
(defmacro hfql [hfql-form] `(unfold (fmap swap (ana/hfql ~hfql-form))))



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
  (values (seed {'% {:a 1}} (hfql [])))
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

(tests ; TODO port to CofreeT
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
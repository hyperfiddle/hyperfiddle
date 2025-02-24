(ns dustingetz.hfql11
  "no cardinality many; partially restored 20250208"
  (:require
    [clojure.datafy :refer [datafy nav]]
    [datomic.api :as d]
    dustingetz.datomic-contrib2 ; datafy EntityMap
    [meander.epsilon :as m :refer [match]]
    [hyperfiddle.nav0 :refer [nav-context]]
    [hyperfiddle.rcf :refer [tests]]
    [hyperfiddle.spec :as spec]
    [clojure.spec.alpha :as s]))

(def ^:dynamic *dynamic-scope* {})

(tests
  (require '[dustingetz.teeshirt-orders-datomic :as tod :refer [test-db alice bob]])
  (require '[dustingetz.mbrainz :refer [test-db lennon] :rename {test-db mbrainz-db}])
  (def !alice (d/entity @test-db alice))
  (def !lennon (d/entity @mbrainz-db lennon))
  )

(defn hf-nav [kf x]
  (as-> x #_(datafy x) x
    (nav x kf (kf x)))
  #_(kf (datomic.api/entity @test-db x)))

(defn hf-edge->sym [edge]
  (if (keyword? edge) (symbol edge) edge))

(defn symbolic [point]
  (cond (instance? clojure.lang.LazySeq point) (list* point)
        () point))

(defn hf-eval [edge scope]
  (merge scope
    (cond
      (= '* edge) (let [d (datafy (get scope '%))] (assoc d '* d))
      () (let [v (cond
                   (keyword? edge) (hf-nav edge (get scope '%))
                   (symbol? edge) (or (hf-nav (keyword (namespace edge) (name edge)) (get scope '%)) ; support syms as kws because they carry metas
                                    (hf-nav edge (get scope '%)))
                   (seq? edge) (let [[f & args] edge]
                                 (clojure.core/apply (clojure.core/resolve f) (replace scope args)))
                   () (println "hf-eval unmatched edge: " edge))]
           {(hf-edge->sym edge) v '% v}))))

(tests
  (hf-eval :order/gender {'% !alice})
  := {'% :order/female, 'order/gender :order/female}
  (hf-eval '(identity %) {'% !alice})
  := {'% !alice,
      '(identity %) !alice}

  (hf-eval '* {'% !alice})
  nil)

(defn context-resolve [context x]
  (if (symbol? x)
    (get context x (get context (keyword (namespace x) (name x))))
    (if-let [[_ v] (find context x)] v x)))

(declare undatafy)

(defn comparable? [a b]
  (if (and (instance? java.lang.Comparable a) (.isInstance (class a) b)) ; cheaper check than direct try/catch
    ;; Likely comparable, attempt full comparison
    (try (compare a b) true
         ;; a custom comparator could throw other exception types, but it wouldn't respect java.lang.Comparable/compareTo's contract.
         (catch ClassCastException _ false))
    false))

(defn lookup-map [m k not-found]
  (if (or (instance? clojure.lang.Sorted m)
        (instance? java.util.SortedMap m))
    (if (comparable? k (key (first m))) ; safe – `cc/key` defined of top-level java.util.Map$Entry
      (get m k not-found)
      not-found)
    (get m k not-found)))

(defn lookup-set [s k not-found]
  (if (or (instance? clojure.lang.Sorted s)
        (instance? java.util.SortedSet s))
    (if (comparable? k (first s))
      (get s k not-found)
      not-found)
    (get s k not-found)))

(defn lookup ; TODO move to contrib.data
  "Like `cc/get`, but safe to call on sorted collections (e.g. sorted-map, sorted-set).
  (-> (sorted-map :a 1) (get 'a)) => ClassCastException due to (compare 'a :a)
  Real-world example: (get (ns-publics *ns*) :foo) ; throws as ns-publics returns a PersistentTreeMap with symbol keys."
  ([associative-or-set k] (lookup associative-or-set k nil))
  ([associative-or-set k not-found]
   (let [o associative-or-set]
     (cond (or (instance? clojure.lang.Associative o) (instance? java.util.Map o))
           (lookup-map o k not-found)

           (instance? java.util.Set o)
           (lookup-set o k not-found)

           () (get o k not-found)))))

;; (lookup (sorted-set :a :b) 'a :not-found)

(defn- nav-with-fallback
  "Tries to navigate on e, accounting for various nav implementations.

  If no navigation matched, return (get datafied a).

  Example for (nav *ns* :publics (get *ns* :publics)):
  - *ns* is a native ref
  - *ns* do not implement Navigable, but implements Datafiable
  - (get *ns* :publics) return nil
  - only way to navigate to :publics is to datafy first
  - but datafied ns doesn't implement nav either
  - so we fallback to (get datafied :publics).
  "
  [e a v]
  (let [attempt (nav e a v)] ; 1. attempt native nav - nav might be implemented on type
    (if-not (identical? v attempt) ; v got transformed
      attempt
      (let [attempt (nav (nav-context e) a v)] ; 2. attempt navigation through optional NavContext - optimization to save a datafy call
        (if-not (identical? v attempt) ; v got transformed
          attempt
          (let [de (datafy e)
                attempt (nav de a v)] ; 3. attempt navigation on datafied object
            (if-not (identical? v attempt) ; v got transformed
              attempt
              (nav de a (lookup de a v)) ; 4. attempt navigation on datafied object with corresponding datafied value, or fallback to v
              )))))))

(comment
  (get (ns-publics *ns*) :wrap-log)
  (get (get (datafy *ns*) :publics) :symbolic)
  (-> (clojure.lang.PersistentTreeMap.) (assoc 'a 1) (get 'a)) := 1
  (-> (clojure.lang.PersistentTreeMap.)
    (assoc 'a 1)
    (get :a))
  ;; := class clojure.lang.Symbol cannot be cast to class clojure.lang.Keyword (clojure.lang.Symbol and clojure.lang.Keyword are in unnamed module of loader 'app')
  )

(defn fn-sym? [sym]
  (let [obj (resolve sym)]
    (and (var? obj) (fn? (deref obj)))))

(defn hf-nav2 ; WIP
  ([e a] (hf-nav2 e a {'% e}))
  ([e a ctx]
   ; hack: keyword/symbol lookups query datafy repr, sexpr calls query object repr
   (cond
     (keyword? a) (nav-with-fallback e a (lookup e a))
     (string? a)  (nav-with-fallback e a (lookup e a))
     (symbol? a)  (if (fn-sym? a)
                    (hf-nav2 e `(~a ~'%) ctx)
                    (let [k (keyword (namespace a) (name a))]
                      (or (hf-nav2 e k ctx) (nav-with-fallback e a (lookup e a)))))
     (sequential? a) (let [[fsym & arg-syms] a]
                       (with-bindings *dynamic-scope*
                         (apply (resolve fsym) (map (comp undatafy (partial context-resolve ctx)) arg-syms))))
     (nat-int? a) (cond (vector? e) (get e a)
                        (sequential? e) (nth e a)
                        () (println `hf-nav2 "don't know how to navigate through" a "onto" e))
     ()           (binding [*print-length* 1, *print-level* 2]
                    (println `hf-nav2 "don't know how to navigate through" (pr-str a) "(a" (type a) ")" "onto" (pr-str e))))))

(tests ; nav-2
  (get !alice :db/id) := 17592186045428
  (hf-nav2 !alice :db/id) := !alice ; identity
  (-> !alice (hf-nav2 :db/id) (hf-nav2 :db/id)) := !alice ; identity

  (get !alice :order/gender) := :order/female
  (hf-nav2 !alice :order/gender) := (d/entity @test-db (:order/gender !alice))

  (-> !alice (get :order/gender) (get :db/ident)) := nil ; can't navigate on scalars
  (-> !alice (hf-nav2 :order/gender) (hf-nav2 :db/ident))
  := (d/entity @test-db :order/female)

  (hf-nav2 !alice `identity) := !alice
  (hf-nav2 !alice '(identity %)) := !alice
  (type *1) := datomic.query.EntityMap

  (hf-nav2 !alice '*) := nil ; nav '* is not a thing
  )

(defn hf-pull [pat]
  (match pat

    [!pats ...] ; cardinality many, added DJG 20250208
    (fn [scope] (->> !pats (mapv (fn [pat]
                                   ((hf-pull pat) scope))
                             #_#(hf-apply % scope))
                  (apply merge)))

    {& (m/seqable [?edge ?pat])}                            ; one entry
    (fn [scope]
      {?edge ((hf-pull ?pat) (hf-eval ?edge scope))})

    *
    (fn [scope] (-> (hf-eval '* scope) (get '*)))

    ?leaf                                                   ; :order/gender '(f dustingetz/gender)
    (fn [scope]
      {(symbolic ?leaf) (-> (hf-eval ?leaf scope) (get '%))})))

#_
(tests
  ((hf-pull :db/ident) {'% (d/entity @test-db :order/male)})
  := #:db{:ident :order/male}

  ((hf-pull [:order/email :db/id]) {'% (d/entity @test-db alice)})
  := {:order/email "alice@example.com", :db/id 17592186045428}

  ((hf-pull [:db/id '*]) {'% (d/entity @test-db alice)})
  := {:db/id 17592186045428,
      :order/email "alice@example.com",
      :order/gender :order/female,
      :order/shirt-size :order/womens-large,
      :order/tags #{:c :b :a}}

  (-> ((hf-pull ['*]) {'% !lennon}) first type)
  ((hf-pull ['* `(identity ~'%)]) {'% !lennon})
  ((hf-pull [:db/id '(identity %)]) {'% !lennon})
  )



(defn hf-pull2 [pat] ; WIP
  (match pat

    [!pats ...] ; cardinality many, added DJG 20250208
    (fn [scope]
      (let [e (datafy (get scope '%))]
        (into (empty e) (map #((hf-pull2 %) scope)))
        (->> !pats (mapv (fn [pat]
                           ((hf-pull2 pat) scope))
                     #_#(hf-apply % scope))
          (apply merge))))

    {& (m/seqable [?edge ?pat])}                            ; one entry
    (fn [scope]
      {?edge ((hf-pull2 ?pat) (hf-nav2 (get scope '%) ?edge scope))})

    *
    (fn [scope] (hf-nav2 (get scope '%) '* scope))

    ?leaf                                                   ; :order/gender '(f dustingetz/gender)
    (fn [scope]
      (if false #_(seq? ?leaf)
        ()
        {(symbolic ?leaf) (-> scope (get '%) (datafy) (get ?leaf))}) ; fixme datafy called multiple times
      #_{(symbolic ?leaf) (get-in scope ['% ?leaf])})))

(defn- undatafy [obj]
  (if (instance? clojure.lang.IObj obj)
    (:clojure.datafy/obj (meta obj) obj)
    obj))

(comment (get !alice 'order/email)) ; datomic entities implement symbol attrs also lol

(defn- pull-one [context pattern e]
  (cond (= '* pattern) (datafy e)
        (keyword? pattern) {pattern (get e pattern)}
        (symbol? pattern)  {pattern (if (fn-sym? pattern)
                                      (hf-nav2 e pattern context)
                                      (get e pattern (get e (keyword (namespace pattern) (name pattern)))))} ; fallback to keyword aliased as symbol
        (seq? pattern)     {(symbolic pattern) (hf-nav2 e pattern context)}
        () (throw (let [m {:pattern pattern}]
                    (ex-info (str "pull-one, not implemented " (pr-str m)) m)))))

;; (defmulti nav-cardinality (fn [datafiable & args] (type datafiable)))
;; (defmethod nav-cardinality datomic.query.EntityMap [e edge]
;;   (let [schema (nav (datafy e) :contrib.datomic-contrib/schema {})]
;;     ({:db.cardinality/many ::many} (get-in @schema [edge :db/cardinality :db/ident]) ::one)))

#_
(defn pattern-cardinality-identifier [e pattern]
  (cond (keyword? pattern) [e pattern]
        #_#_(symbol? pattern) [e pattern] #_(hf-nav (keyword (namespace edge) (name edge)) (get scope '%))
        (seq? pattern) (let [[fsym & _arg-syms] pattern]
                         (when-let [var (resolve fsym)]
                           [(deref var) (.toSymbol var)]))
        () (throw (let [m  {:pattern pattern}]
                    (ex-info (str "pattern-cardinality-identifier, not implemented " (pr-str m)) m)))))

(defn card-many-value? [x]
  ;; Intuitive check would be (and (coll? x) (not (map? x)))
  ;; but it would consider datomic.query.EntityMap to be card-many, as EntityMap satisfies coll? but not map?
  ;; Fallback to a less intuitive but correct check:
  (or (sequential? x) (set? x)))

(defn pulled-map [e] (with-meta {} (or (nav-context e) (meta (datafy e)))))

(declare hf-pull3)
(defn hfql-search-sort [hfql-dynamics hfql-spec search xs-enriched]
  ; xs must be a collection of objects/records, i.e. not [:a :b :c]
  (with-meta
    (->> xs-enriched
      (eduction ; search all pulled cols
        (map (fn [e] [e (hf-pull3 hfql-dynamics hfql-spec
                          (clojure.datafy/nav xs-enriched nil e))]))
        (filter #(contrib.str/includes-str? (vals (nth % 1)) search))) ; exclude keys, todo revisit - problem is "clojure.core" matches `(ns-name %)
      sequence
      (sort-by (fn [[e pulled-map]] (get pulled-map (first hfql-spec))) #_(if (ident? (first hfql-spec)) (first hfql-spec) hash))
      (map first)) ; unpull so we can datafy entity in view to infer cols
    (meta xs-enriched)))

(defn hf-pull3 ; WIP
  " Only call datafy for '*, otherwise navigate over native objects. Want to skip datafy entirely?
  Expand and replace '* before pulling."
  ([pattern e] (hf-pull3 {} pattern e))
  ([dynamic-scope pattern e] (let [e (datafy e)] (hf-pull3 dynamic-scope {'% e} pattern e)))
  ([dynamic-scope context pattern e] (hf-pull3 dynamic-scope context pattern pattern e 0))
  ([dynamic-scope context root-pattern pattern e depth]
   (binding [*dynamic-scope* (or dynamic-scope {})] ; HACK band aid
     (match pattern

       [!patterns ...]
       (->> !patterns
         (reduce (fn [[context results] pattern]
                   (let [result (if (map? pattern)
                                  (hf-pull3 dynamic-scope (assoc context '% e) root-pattern pattern e depth)
                                  (pull-one context pattern e))]
                     [(assoc context (symbolic pattern) (get result pattern)) (conj results result)]))
           [context []])
         (second)
         (reduce (fn [r m] (into r (reduce dissoc m (keys r)))) (pulled-map e)))

       {& (m/seqable [?edge ?pattern])}
       (if (and (nat-int? ?pattern) (>= depth ?pattern))
         nil
         (assoc (pulled-map e) (symbolic ?edge)
           (let [next-e        (hf-nav2 e ?edge context)
                 continue (fn [e] ;; avoid #(assoc x '% %) : '% expands to a sharp-lambda gensymed '%.
                            (cond
                              (nat-int? ?pattern) (hf-pull3 dynamic-scope (assoc context '% e) root-pattern root-pattern e (inc depth))
                              (= '... ?pattern)   (hf-pull3 dynamic-scope (assoc context '% e) root-pattern root-pattern e (inc depth))
                              ()                  (hf-pull3 dynamic-scope (assoc context '% e) root-pattern ?pattern e depth)))]
             (cond (card-many-value? next-e) (map continue next-e)
                   (some? next-e) (continue next-e)
                   () nil))))

       ?pattern
       (hf-pull3 dynamic-scope context root-pattern [pattern] e depth)
       ))))


(comment
  (hf-pull3 [`(fs/file-name ~'%) {`(fs/dir-list ~'%) 2}] '…)
  )

(tests ; pull3
  (hf-pull3 :db/ident (d/entity @test-db :order/male))
  := {:db/ident :order/male}

  (hf-pull3 [:order/email :db/id] !alice)
  := {:order/email "alice@example.com", :db/id 17592186045428}

  (hf-pull3 ['*] !alice)
  := {:db/id 17592186045428,
      :order/email "alice@example.com",
      :order/gender :order/female,
      :order/shirt-size :order/womens-large,
      :order/tags #{:c :b :a}})

(tests ; pull3
  (hf-pull3 `identity !lennon) := {`identity !lennon}
  (hf-pull3 `(identity ~'%) !lennon)
  := {`(identity ~'%) !lennon}

  (hf-pull3 [:db/id `(identity ~'%)] !lennon)
  := {:db/id 527765581346058 '(clojure.core/identity %) !lennon}

  (hf-pull3 ['* `(identity ~'%)] !lennon)
  := (assoc (datafy !lennon) '(clojure.core/identity %) !lennon)
)


(tests
  ;(hf-pull '(:db/ident %) {'% :order/male})            ; interpret kw as entity nav? There's no need to, don't do this
  ;:= {(:db/ident %) (:db/ident :order/male)} ; ClassCastException

  ((hf-pull '(hf-nav :db/ident %)) {'% (d/entity @test-db :order/male)})
  := {'(hf-nav :db/ident %) :order/male}

  ((hf-pull '(identity %)) {'% (d/entity @test-db :order/male)})
  := {'(identity %) (d/entity @test-db :order/male) #_#:db{:id 17592186045418}}
  (type (get *1 '(identity %))) := datomic.query.EntityMap

  ((hf-pull :order/gender) {'% (d/entity @test-db bob)})
  := #:order{:gender :order/male}

  ;(hf-pull '(:order/gender %) {'% 17592186045441})
  ;:= {(:order/gender %) (:order/gender 17592186045441)}

  ((hf-pull {:order/gender :db/ident}) {'% (d/entity @test-db bob)})
  := #:order{:gender #:db{:ident :order/male}}

  ((hf-pull {:order/gender {:db/ident :db/id}}) {'% (d/entity @test-db bob)})
  := #:order{:gender #:db{:ident #:db{:id 17592186045418}}}

  ((hf-pull {:order/gender {:db/ident {:db/ident :db/ident}}}) {'% (d/entity @test-db bob)})
  := #:order{:gender #:db{:ident #:db{:ident #:db{:ident :order/male}}}}

  ((hf-pull {:order/gender :db/id}) {'% bob})
  := #:order{:gender #:db{:id 17592186045418}}

  ((hf-pull {:order/gender {:db/id :db/id}}) {'% bob})
  := #:order{:gender #:db{:id #:db{:id 17592186045418}}}

  ; :db/id is a self reference so this actually is coherent
  ((hf-pull {:order/gender {:db/id {:db/id {:db/id :db/id}}}}) {'% bob})
  := #:order{:gender #:db{:id #:db{:id #:db{:id #:db{:id 17592186045418}}}}})

(tests
  ;(hf-pull '(:db/ident %) {'% :order/male})            ; interpret kw as entity nav? There's no need to, don't do this
  ;:= {(:db/ident %) (:db/ident :order/male)} ; ClassCastException

  ((hf-pull2 '(hf-nav :db/ident %)) {'% (d/entity @test-db :order/male)})
  := {'(hf-nav :db/ident %) :order/male}

  ((hf-pull2 '(identity %)) {'% (d/entity @test-db :order/male)})
  := {'(identity %) (d/entity @test-db :order/male) #_#:db{:id 17592186045418}}
  (type (get *1 '(identity %))) := datomic.query.EntityMap

  ((hf-pull2 :order/gender) {'% (d/entity @test-db bob)})
  := #:order{:gender :order/male}

  ;(hf-pull '(:order/gender %) {'% 17592186045441})
  ;:= {(:order/gender %) (:order/gender 17592186045441)}

  ((hf-pull2 {:order/gender :db/ident}) {'% (d/entity @test-db bob)})
  := #:order{:gender #:db{:ident :order/male}}

  ((hf-pull {:order/gender {:db/ident :db/id}}) {'% (d/entity @test-db bob)})
  := #:order{:gender #:db{:ident #:db{:id 17592186045418}}}

  ((hf-pull {:order/gender {:db/ident {:db/ident :db/ident}}}) {'% (d/entity @test-db bob)})
  := #:order{:gender #:db{:ident #:db{:ident #:db{:ident :order/male}}}}

  ((hf-pull {:order/gender :db/id}) {'% bob})
  := #:order{:gender #:db{:id 17592186045418}}

  ((hf-pull {:order/gender {:db/id :db/id}}) {'% bob})
  := #:order{:gender #:db{:id #:db{:id 17592186045418}}}

  ; :db/id is a self reference so this actually is coherent
  ((hf-pull {:order/gender {:db/id {:db/id {:db/id :db/id}}}}) {'% bob})
  := #:order{:gender #:db{:id #:db{:id #:db{:id #:db{:id 17592186045418}}}}})

(tests ; pull3
  (comment (set! *print-namespace-maps* false) )

  (hf-pull3 '(hf-nav2 % :db/ident ) (d/entity @test-db :order/male))
  := {'(hf-nav2 % :db/ident) (d/entity @test-db :order/male)}

  (hf-pull3 '(identity %) (d/entity @test-db :order/male))
  := {'(identity %) (d/entity @test-db :order/male)}

  (type (get *1 '(identity %))) := datomic.query.EntityMap

  (hf-pull3 :order/gender (d/entity @test-db bob))
  := {:order/gender :order/male}

  (hf-pull3 {:order/gender {:db/ident :db/id}} !alice)
  := {:order/gender {:db/ident {:db/id 17592186045419}}}

  (-> (hf-pull3 :order/gender !alice)
    (hf-nav2 :db/id)) := !alice ; sideway nav, pull do not damage navigability. User can choose.

  (-> (hf-pull3 :order/gender !alice)
    (undatafy)) := !alice ; Renderer can opt for native ref or datafied version at point.

  (hf-pull3 {:order/gender {:db/ident {:db/ident :db/ident}}} !alice) ; identity
  := {:order/gender {:db/ident {:db/ident {:db/ident :order/female}}}}

  (hf-pull3 {:order/gender {:db/id {:db/id :db/id}}} !alice) ; identity
  := {:order/gender {:db/id {:db/id {:db/id 17592186045419}}}}

  )



;; (defmethod nav-cardinality clojure.lang.Fn [_ fsym]
;;   (if (spec/cardinality-many? fsym) ::many ::one))

(tests ; pull3
  (defn genders [db] (map #(datomic.api/entity db %) (tod/genders db)))
  (defn shirt-sizes [db gender-ident needle]
    (map #(datomic.api/entity db %) (tod/shirt-sizes db gender-ident needle)))

  (hf-pull3 {'db @test-db} '(genders db) nil) 

  (hf-pull3 {'db @test-db} '{(genders db) :db/ident} nil)
  := '{(genders db) ({:db/ident :order/female} {:db/ident :order/male})}

  (hf-pull3 {'db @test-db} {`(genders ~'db) [:db/ident {`(shirt-sizes ~'db ~'db/ident "") [:db/ident]}]} nil)
  := '{(dustingetz.hfql11/genders db)
       ({:db/ident :order/female,
         (dustingetz.hfql11/shirt-sizes db db/ident "")
         ({:db/ident :order/womens-large}
          {:db/ident :order/womens-medium}
          {:db/ident :order/womens-small})}
        {:db/ident :order/male,
         (dustingetz.hfql11/shirt-sizes db db/ident "")
         ({:db/ident :order/mens-large}
          {:db/ident :order/mens-medium}
          {:db/ident :order/mens-small})})}

  ;; (hf-pull3 {'db @test-db, 'needle "ali"} `(tod/orders ~'db ~'needle) nil)
  ;; := '{(dustingetz.teeshirt-orders-datomic/orders db needle) (17592186045428)}

  ;; (hf-pull3 {'db @test-db, 'needle "ali"} `{(tod/orders ~'db ~'needle) [:order/gender]} nil)

  )

(comment ; crashing 20250208

  ((hf-pull {'(submission needle) :order/gender}) {'needle "alic"})
  := '{(submission needle) #:order{:gender :order/female}}

  ((hf-pull {'(submission needle) {:order/gender :db/ident}}) {'needle "alic"})
  := '{(submission needle) #:order{:gender #:db{:ident :order/female}}}

  ((hf-pull '{(submission needle) {:order/gender (shirt-size dustingetz/gender)}}) {'needle "alic"})
  := '{(submission needle) #:order{:gender {(shirt-size dustingetz/gender) 17592186045436}}}

  ((hf-pull
     '{(submission needle)                                  ; query
       {:order/gender
        {(shirt-size dustingetz/gender)
         :db/ident}}})

   {'needle "alic"})                                        ; scope

  := '{(submission needle)                                  ; result
       {:order/gender
        {(shirt-size dustingetz/gender)
         {:db/ident :order/womens-small}}}}

  ;((hf-pull {:db/ident :db/id}) {'% 17592186045430}) := #:db{:ident #:db{:id 17592186045430}}
  )

(ns hyperfiddle.hfql0
  (:require [hyperfiddle.electric3 :as e]
            [clojure.string :as str]
            [hyperfiddle.rcf :as rcf]
            [clojure.core.protocols :as ccp]
            [clojure.datafy :as datafy])
  (:import [java.io Writer]))

(defprotocol Suggestable :extend-via-metadata true
  (-suggest [o]))

(defn suggest [o] (-suggest o))

(defn suggest-fields [^Class clazz]
  (into [] (keep
             (fn [^java.lang.reflect.Field fld]
               (when-not (java.lang.reflect.Modifier/isStatic (.getModifiers fld))
                 (let [nm (.getName fld)]
                   {:label (str ".-" nm)
                    :entry (list (symbol (str ".-" nm)) (symbol "%"))}))))
    (.getFields clazz)))

(defn suggest-methods [^Class clazz]
  (into [] (map
             (fn [^java.lang.reflect.Method meth]
               (let [nm (.getName meth), arg-count (.getParameterCount meth)
                     stub-arg* (mapv #(symbol (str "arg" (inc %))) (range arg-count))]
                 {:label (str "." nm " (" arg-count ")")
                  :entry (list* (symbol (str "." nm)) (symbol "%") stub-arg*)})))
    (.getMethods clazz)))

(defn suggest-jvm [o]
  (let [clazz (class o)]
    (into (suggest-fields clazz) (suggest-methods clazz))))

(extend-protocol Suggestable
  clojure.lang.IPersistentMap
  (-suggest [m] (into [] (map (fn [k] {:label k, :entry k}) (keys m))))
  Object
  (-suggest [o] (suggest-fields (class o)))
  nil
  (-suggest [_]))

(comment
  (suggest-fields (class (props :k {})))
  (suggest-methods (class (props :k {})))
  )

(defprotocol Viewer
  (-view [_ _o])
  (-unwrap [_])
  (-opts [_]))

(defn view [k o] (-view k o))
(defn unwrap [k] (-unwrap k))
(defn opts [k] (-opts k))

(extend-protocol Viewer
  Object
  (-view [k o] (get o k))
  (-unwrap [k] k)
  (-opts [_])
  nil
  (-view [_ o] (throw (ex-info "Cannot view on nil" {:o o})))
  (-unwrap [_] (throw (ex-info "Cannot unwrap nil" {})))
  (-opts [_]))

(deftype Props [k opts]
  Viewer
  (-view [_ o] (-view k o))
  (-unwrap [_] k)
  (-opts [_] opts))

(defmethod print-method Props [^Props props ^Writer w]
  (.write w "#Props[")
  (print-method (.-k props) w)
  (.write w " ")
  (print-method (.-opts props) w)
  (.write w "]"))

(defn props [k opts] (->Props k opts))
(defn props-update-k [props_ f]
  (let [raw-props (unwrap props_)]
    (cond-> (f raw-props) (not (identical? props_ raw-props)) (props (opts props_)))))
(defn props-update-opts [props_ f]
  (let [raw-props (unwrap props_)]
    (if (identical? raw-props props_)
      props_
      (props raw-props (f (or (opts props_) {}))))))

(defn resolve!
  ([f$] (or (resolve f$) (throw (ex-info (str "Failed to resolve " f$) {}))))
  ([f$ ns] (if (qualified-symbol? f$)
             (or (ns-resolve ns f$) (requiring-resolve f$))
             (or (ns-resolve ns f$) (throw (ex-info (str "Failed to resolve " f$) {}))))))

(defn invoke-reflective [method$ o & args]
  (clojure.lang.Reflector/invokeInstanceMethod o (subs (str method$) 1) (into-array args)))

(defn read-reflective [field$ o]
  (let [fld (.getField (class o) (subs (str field$) 2))]
    (.get fld o)))

(defn field-access? [f$] (str/starts-with? (str f$) ".-"))
(defn method-access? [f$] (str/starts-with? (str f$) "."))

(defn pull-view [scope viewer]
  (let [k (unwrap viewer), o (get scope '%)]
    (cond
      (symbol? k) (let [f$ k]
                    (cond (= '. f$) (get o f$)
                          (= '.. f$) (get o f$)
                          (field-access? f$) (read-reflective f$ o)
                          (method-access? f$) (invoke-reflective f$ o)
                          :else (or (let [v (get o f$ ::not-found)]
                                      (when (not= v ::not-found) v))
                                  (let [resolved (resolve! f$)]
                                    (when (var? resolved) (resolved o))))))
      (seq? k) (let [[f$ & args] (replace scope k)]
                 (cond (field-access? f$) (read-reflective f$ (first args))
                       (method-access? f$) (apply invoke-reflective f$ args)
                       :else (apply (resolve! f$) args)))
      (map? k) (let [[k k2] (first k), v (pull-view scope k)]
                 (pull-view (assoc scope '% (datafy/nav o k v)) k2))
      :else (view viewer o))))

(defn pull-object [scope spec]
  (with-meta
    (reduce (fn self [ac viewer] (assoc ac (unwrap viewer) (pull-view scope viewer))) {} spec)
    {::origin (get scope '%)}))

(defn pull [bindings spec o]
  (with-bindings bindings
    (if (sequential? o)
      (mapv (fn [x] (pull-object {'% x} spec)) o)
      (pull-object {'% o} spec))))

(def ^:dynamic *test* nil)
(defn test-times [n] (* *test* n))
(rcf/tests
  (pull {} [:a :b] {:a 1 :b 2})                                   := {:a 1, :b 2}
  (pull {} [:a :b] [{:a 1 :b 2}])                                 := [{:a 1, :b 2}]
  (pull {} [(props :a {}) :b] {:a 1 :b 2})                        := {:a 1, :b 2}
  (pull {} `[(inc ~'%)] 1)                                        := {`(inc ~'%) 2}
  (pull {#'*test* 10} `[(test-times ~'%)] 2)                      := {`(test-times ~'%) 20}
  (pull {} `[(inc ~'%)] [1 2])                                    := `[{(inc ~'%) 2} {(inc ~'%) 3}]
  (pull {} `[(.get ~'% :a)] {:a 1})                               := `{(.get ~'% :a) 1}
  (pull {} `[(.get ~'% :a)] [{:a 1} {:b 2}])                      := `[{(.get ~'% :a) 1} {(.get ~'% :a) nil}]
  (pull {} `[(.-width ~'%)] (new java.awt.Rectangle 10 20 30 40)) := `{(.-width ~'%) 30}
  (pull {} `[inc] 41)                                             := `{inc 42}
  (pull {} `[{:foo inc}] {:foo 41, :bar 0})                       := `{{:foo inc} 42}
  (pull {} '[{.-x inc}] (new java.awt.Point 41 2))                := '{{.-x inc} 42}
  (let [data-with-nav (with-meta {:a 1, :b 2} {`ccp/nav (fn [_this k v] (if (= k :a) {:db/ident 42} v))})]
    (pull {} [{:a :db/ident} :b] data-with-nav))                  := {{:a :db/ident} 42, :b 2}
  (pull {} '[Number] {'Number Number})                            := {'Number Number}
  (pull {} '[+] {'+ #'+})                                         := {'+ #'+}
  (pull {} '[..] {'.. :foo})                                      := {'.. :foo}
  )


(comment
  (.get {:a 1} :a)
  (clojure.lang.Reflector/invokeInstanceMethodOfClass {:a 1} (class {:a 1}) "get" (into-array [:a]))
  (invoke-reflective {:a 1} '.get :a)
  )

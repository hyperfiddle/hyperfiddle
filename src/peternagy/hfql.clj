(ns peternagy.hfql
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.rcf :as rcf]
            [contrib.debug :as dbg])
  (:import [java.io Writer]))

(defprotocol Suggestable :extend-via-metadata true
  (-suggest [o]))
(defn suggest [o] (-suggest o))

(extend-protocol Suggestable
  Object
  (-suggest [_])
  nil
  (-suggest [_]))

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
  (props (unwrap props_) (f (or (opts props_) {}))))

(defn resolve! [f$] (or (resolve f$) (throw (ex-info (str "Failed to resolve " f$) {}))))

(defn pull-object [scope spec o]
  (with-meta
    (reduce (fn [ac viewer]
              (let [k (unwrap viewer)]
                (if (seq? k)
                  (let [[f$ & args] (replace scope k)
                        v (apply (resolve! f$) args)]
                    (assoc ac k v))
                  (assoc ac k (view viewer o)))))
      {} spec)
    {::origin o}))

(defn pull [bindings spec o]
  (with-bindings bindings
    (if (sequential? o)
      (mapv (fn [x] (pull-object {'% x} spec x)) o)
      (pull-object {'% o} spec o))))

(def ^:dynamic *test* nil)
(defn test-times [n] (* *test* n))
(rcf/tests
  (pull {} [:a :b] {:a 1 :b 2})              := {:a 1, :b 2}
  (pull {} [:a :b] [{:a 1 :b 2}])            := [{:a 1, :b 2}]
  (pull {} [(props :a {}) :b] {:a 1 :b 2})   := {:a 1, :b 2}
  (pull {} `[(inc ~'%)] 1)                   := {`(inc ~'%) 2}
  (pull {#'*test* 10} `[(test-times ~'%)] 2) := {`(test-times ~'%) 20}
  (pull {} `[(inc ~'%)] [1 2])               := `[{(inc ~'%) 2} {(inc ~'%) 3}]
  )

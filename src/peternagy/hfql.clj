(ns peternagy.hfql
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.rcf :as rcf]
            [contrib.debug :as dbg]
            ))

(defprotocol Suggestable :extend-via-metadata true
  (-suggest [o]))
(defn suggest [o] (-suggest o))

(extend-protocol Suggestable
  Object
  (-suggest [_])
  nil
  (-sugget [_]))

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

(defn props [k opts] (->Props k opts))

(defn resolve! [f$] (or (resolve f$) (throw (ex-info (str "Failed to resolve " f$) {}))))

(defn pull-object [scope spec o]
  (reduce (fn [ac viewer]
            (let [k (unwrap viewer)]
              (if (seq? k)
                (let [[f$ & args] (replace scope k)
                      v (apply (resolve! f$) args)]
                  (assoc ac k v))
                (assoc ac k (view viewer o)))))
    {} spec))

(defn pull [bindings spec o]
  (with-bindings bindings
    (if (sequential? o)
      (mapv #(pull-object {'% o} spec %) o)
      (pull-object {'% o} spec o))))

(def ^:dynamic *test* nil)
(defn test-times [n] (* *test* n))
(rcf/tests
  (pull {} [:a :b] {:a 1 :b 2})              := {:a 1, :b 2}
  (pull {} [:a :b] [{:a 1 :b 2}])            := [{:a 1, :b 2}]
  (pull {} [(props :a {}) :b] {:a 1 :b 2})   := {:a 1, :b 2}
  (pull {} `[(inc ~'%)] 1)                   := {`(inc ~'%) 2}
  (pull {#'*test* 10} `[(test-times ~'%)] 2) := {`(test-times ~'%) 20}
  )

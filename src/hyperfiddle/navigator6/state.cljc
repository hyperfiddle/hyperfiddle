(ns hyperfiddle.navigator6.state
  #?(:cljs (:refer-clojure :exclude [-write]))
  (:require [hyperfiddle.electric-forms5 :as forms]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.router5 :as router])
  #?(:cljs (:require-macros [hyperfiddle.navigator6.state :refer [with-local-state]])))

(e/declare *depth *local-state)

(defmulti read-state (fn [type & _args] type))
(defmethod read-state ::route [_ route depth] (nth route depth {}))
;; (defmethod read-state ::session-storage [_ & [prefix]] ) ; example

(defmulti write-state (fn [type & _args] type))
(defmethod write-state ::route [_ & _] (throw (ex-info "Unsupported. Use router/ReplaceState from Electric." {})))

(defmulti update-state (fn [type & args] type))
(defmethod update-state ::route [_ route depth f & args]
  (into
    (conj (into [] (take depth) route)
      (apply f (read-state ::route route depth) args)) ; TODO post-walk tidy pass, pruning nil values up to the root
    (drop (inc depth) route)))

(e/defn HandleReplaceState! [f & args]
  ;; `update` doesn't work, route is a lazyseq (?)
  (e/Reconcile
    (case (router/Navigate! ['. (take (inc *depth) (apply update-state ::route router/route *depth f args))])
      [::forms/ok])))

(e/defn HandleUpdateState! [f & args]
  ;; `update` doesn't work, route is a lazyseq (?)
  (e/Reconcile
    (case (router/ReplaceState! ['. (apply update-state ::route router/route *depth f args)])
      [::forms/ok])))

(e/defn WithLocalRouteState [Body]
  (e/client
    (binding [*depth (inc (#(or *depth 0)))]
      (binding [*local-state (read-state ::route router/route *depth)]
        (forms/Interpreter {`ReplaceState! HandleReplaceState!
                            `UpdateState! HandleUpdateState!}
          (Body))))))

(defmacro with-local-state [storage-type & body]
  (case storage-type
    ::route `(WithLocalRouteState (e/fn [] ~@body))
    (throw (ex-info (str "Unknown storage type: " storage-type) {:storage-type storage-type}))))
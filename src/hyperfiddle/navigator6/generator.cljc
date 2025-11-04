(ns hyperfiddle.navigator6.generator
  (:require [cloroutine.core :refer [cr]])
  #?(:cljs (:require-macros [cloroutine.core :refer [cr]]
                            [hyperfiddle.navigator6.generator :refer [generator]])))

(def ^:dynamic *tail*)

(defn gen-seq [gen]
  (lazy-seq (binding [*tail* (gen-seq gen)] (gen))))

(defn yield [x]
  (cons x *tail*))

(defn no-op [])

(defmacro generator [& body]
  `(gen-seq (cr {yield no-op} ~@body nil)))


(def ^:private fibonacci
  (generator
    (loop [prev 0 curr 1]
      (yield curr)
      (recur curr (+ prev curr)))))

(comment
  (take 10 fibonacci))
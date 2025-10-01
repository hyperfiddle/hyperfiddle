(ns hyperfiddle.navigator6.extentions
  (:require [clojure.string :as str]
            [hyperfiddle.hfql2 :as hfql]))

(defmulti documentation-dispatch type)
(defmethod documentation-dispatch :default [x] (type x))
(defmethod documentation-dispatch (type :kw) [kw] kw)
(defmethod documentation-dispatch (type 'sym) [sym] sym)
(defmethod documentation-dispatch (type "str") [str] str)

(defmulti documentation documentation-dispatch)
(defmethod documentation :default [_] nil)
#?(:clj (defmethod documentation (type #'type) [var] (:doc (meta var))))

(defn find-documentation [edge]
  (when-let [identifier (hfql/symbolic-edge edge)]
    (str/triml
      (str (documentation
             (cond (qualified-symbol? identifier) #?(:clj (resolve identifier) :cljs nil) ; FIXME violates HFQL's rule ② – don't resolve
                   ;; FIXME have HFQL provide all info ahead of time (e.g. doc, javadoc)
                   :else nil))
        "\n\n"))))


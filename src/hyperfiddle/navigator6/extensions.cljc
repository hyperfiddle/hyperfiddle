(ns hyperfiddle.navigator6.extensions)

(defmulti documentation-dispatch type)
(defmethod documentation-dispatch :default [x] (type x))
(defmethod documentation-dispatch (type :kw) [kw] kw)
(defmethod documentation-dispatch (type 'sym) [sym] sym)
(defmethod documentation-dispatch (type "str") [str] str)

(defmulti documentation documentation-dispatch)
(defmethod documentation :default [_] nil)
#?(:clj (defmethod documentation (type #'type) [var] (:doc (meta var))))


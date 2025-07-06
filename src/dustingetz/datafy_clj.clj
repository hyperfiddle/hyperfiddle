(ns dustingetz.datafy-clj
  (:require [clojure.core.protocols :refer [Datafiable]]
            [hyperfiddle.hfql0 :refer [Identifiable]]))

(extend-type clojure.lang.Var
  Identifiable (-identify [^clojure.lang.Var x] (symbol x))
  Datafiable
  (datafy [^clojure.lang.Var x]
    {::toSymbol (.toSymbol x)
     ::meta (meta x)
     ::getTag (.getTag x)
     ::isMacro (.isMacro x)
     #_#_::deref (str @x)}))

(extend-type clojure.lang.Namespace
  Identifiable (-identify [^clojure.lang.Namespace ns] (ns-name ns))
  ;; Datafy already implemented in clojure.datafy
  )

(comment
  (ns-unmap *ns* 'nav)
  (require '[clojure.datafy :refer [datafy nav]] '[hyperfiddle.hfql0 :refer [identify]])
  (def x #'dev/DevMain)
  (def x #'*)
  (identify x)
  (datafy x)
  (as-> x x
    (datafy x) (nav x ::meta (get x ::meta))))

(comment
  (require '[clojure.datafy :refer [datafy nav]]
    '[dustingetz.hfql11 :refer [hf-pull hf-pull2 hf-pull3]])
  (datafy *ns*)
  (hf-pull3 ['*] *ns*)
  (hf-pull3 [:name :publics :imports :interns] (datafy *ns*))
  (hf-pull3 [`(ns-name ~'%) `(ns-publics ~'%) `(ns-imports ~'%) `(ns-interns ~'%)] *ns*))
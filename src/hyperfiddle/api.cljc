(ns hyperfiddle.api)

(def ^:dynamic *$*) ; legacy

; should it be in hyperfiddle.electric3 ? Does Electric have component libs?
#?(:clj (def ^:dynamic *base-static-resource-path* "/hyperfiddle/")) ; "/public/hyperfiddle/"

(comment (str *base-static-resource-path* "electric-forms.css") := "/hyperfiddle/electric-forms.css")


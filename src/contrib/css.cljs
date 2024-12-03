(ns contrib.css
  (:require [clojure.string :as string]))

(def slugify-regexp #"[:/? .)(]")

(defn css-slugify [s]
  ; in CSS, identifiers (including element names, classes, and IDs in selectors)
  ; can contain only the characters [a-zA-Z0-9] and ISO 10646 characters U+00A0 and higher,
  ; plus the hyphen (-) and the underscore (_); they cannot start with a digit, two hyphens,
  ; or a hyphen followed by a digit.
  ; http://stackoverflow.com/a/449000/959627
  ; https://mathiasbynens.be/notes/css-escapes
  (-> (if (number? s)
        (str "n" s)                                         ; "0" and "-0" is not legal css but "n0" is
        (str s))                                            ; coerce keywords etc
    (string/replace slugify-regexp "-")))

(defn css
  "&args will be flattened"
  [& args]
  (->> (flatten args)
    (remove nil?)
    (string/join " ")))

(defn add-class [props class-str]
  (update props :class (fn [old] (if (string? old)
                                   [old class-str]
                                   (conj old class-str)))))
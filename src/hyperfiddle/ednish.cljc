;; Previous EDNish impl was based on regexps and char replacement, which is not
;; enough to encode/decode nested data structures (for hfql). This new version
;; supports:
;; - arbitrary nested data structures
;; - constructor extensions (e.g. tagged literals)

(ns hyperfiddle.ednish
  "Parse and serialize EDN to and from an URL-safe, human-readable format."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [hyperfiddle.rcf :as rcf :refer [tests]]
            [instaparse.core :as instaparse])
  #?(:clj (:import [clojure.lang IPersistentVector IPersistentMap IPersistentSet Symbol Keyword])
     :cljs (:import [cljs.lang IPersistentVector IPersistentMap IPersistentSet Symbol Keyword])))

(comment
  (rcf/enable!))

(def ^:private EDNISH-GRAMMAR
  "<Ednish>    = Expr+
   <Expr>      = (String / Atom) | Sexpr | Separator
   Sexpr       = <'('> Ednish* <')'>
   Atom        = #'[^(),{}\\[\\]]+'
   String      = <\"'\"> #\"[^(),']+\" <\"'\">
   <Separator> = <#',+'> ")

(def ^:private parser (instaparse/parser EDNISH-GRAMMAR #_#_:auto-whitespace :standard))

(declare decode)

(defn parse [str]
  (instaparse/transform {:Sexpr (fn [& content]
                                  (if (empty? content) ; distinguishes (nil) from () – aka unit
                                    (decode ())
                                    (decode content)))
                         :String decode
                         :Atom (comp decode edn/read-string)}
    (instaparse/parse parser str)))


;;;;;;;;;;;;;;
;; Encoding ;;
;;;;;;;;;;;;;;

(def ^:private RFC-3986-SUB-DELIMITERS #{\$ \:})
(def ^:private SAFE-STRING-CHARS #{\( \) \$})

(declare encode -encode percent-encode unescape-string constructor)

(defn- encode-seq [object] (str "(" (str/join "," (map encode object)) ")"))
(defn- encode-vector [object] (encode-seq (cons 'vec object)))
(defn- encode-set [object] (encode-seq (cons 'set object)))
(defn- encode-map [object] (encode-seq (cons 'map (sequence cat object))))
(defn- encode-string [object] (str "'" (unescape-string SAFE-STRING-CHARS (percent-encode object)) "'"))
(defn- encode-ident [object]
  (str
    (when (keyword? object) ":")
    (when (qualified-ident? object)
      (str (percent-encode (namespace object))
        "$"))
    (percent-encode (name object))))

(defn- encode-tagged-value [object]
  (let [str-repr (pr-str object)
        tag  (first (str/split str-repr #"\s+" 0))]
    (unescape-string RFC-3986-SUB-DELIMITERS 
      (encode `(~(symbol (subs tag 1)) ~(subs str-repr (+ 2 (count tag)) (dec (count str-repr))))))))

(defmulti encode-dispatch type)
(defmethod encode-dispatch :default [object]
  (cond (seq? object) ::seq
        :else (type object)))

(defmulti encode encode-dispatch)
(defmethod encode :default [object] (-encode object))
(defmethod encode ::seq [object] (encode-seq object))


(defprotocol Encodable
  (-encode [object]))

(extend-protocol Encodable
  #?(:clj java.lang.Number, :cljs js/Number)
  (-encode [object] (pr-str object))
  #?(:clj java.lang.String, :cljs js/String)
  (-encode [object] (encode-string object))

  Symbol
  (-encode [object] (encode-ident object))
  Keyword
  (-encode [object] (encode-ident object))

  IPersistentVector
  (-encode [object] (encode-vector object))
  IPersistentMap
  (-encode [object] (encode-map object))
  IPersistentSet
  (-encode [object] (encode-set object))

  #?(:clj java.util.Date, :cljs js/Date) ; #inst
  (-encode [object] (encode-tagged-value object))

  #?(:clj java.util.UUID, :cljs UUID) ; #uuid
  (-encode [object] (encode-tagged-value object))


  #?(:clj Object, :cljs default)
  (-encode [object] (throw (ex-info (str "Don't know how to encode " (type object) " to EDNish. Please add a defmethod for hyperfiddle.ednish/encode or extend-protocol Encodable on " (type object) ".") {:object object})))
  nil
  (-encode [_] (pr-str nil)))

(defn- percent-encode [v]
  (let [s (str v)]
    #?(:clj (java.net.URLEncoder/encode s "UTF-8")
       :cljs (js/encodeURIComponent s)))) ;; encodeURIComponent encodes all but RFC-3986 unreserved chars

(defn- unescape-string [chars string]
  (->> chars
    (map str)
    (map (juxt percent-encode identity))
    (reduce (partial apply str/replace) string)))

(defn serialize [object] (encode object))

(tests
  (serialize "foo") := "'foo'"
  (serialize "foo bar") := "'foo+bar'"
  (serialize "foo,bar") := "'foo%2Cbar'"
  (serialize "\"pour\" l'amour") := "'%22pour%22+l%27amour'"

  (serialize :foo) := ":foo"
  (serialize 'foo) := "foo"
  (serialize :foo/bar) := ":foo$bar"
  (serialize 'foo/bar) := "foo$bar"
  (serialize 'foo/bar/baz) := "foo$bar%2Fbaz"
  (serialize :foo/bar$baz) := ":foo$bar%24baz"
  (serialize 'foo/bar$baz) := "foo$bar%24baz"

  (serialize ()) := "()"
  (serialize '(foo)) := "(foo)"
  (serialize '(foo bar)) := "(foo,bar)"
  (serialize '(())) := "(())"
  (serialize '(() ())) := "((),())"
  (serialize '((() ()) (() ()))) := "(((),()),((),()))"
  (serialize '(((a) (b)) ((c) (d)))) := "(((a),(b)),((c),(d)))"

  (serialize []) := "(vec)"
  (serialize [[]]) := "(vec,(vec))"
  (serialize [:a :b]) := "(vec,:a,:b)"

  (serialize #{}) := "(set)"
  (serialize #{#{}}) := "(set,(set))"
  (serialize #{:a :b}) := "(set,:b,:a)"

  (serialize {}) := "(map)"
  (serialize {:a :b}) := "(map,:a,:b)"
  (serialize {:a {:b :c}}) := "(map,:a,(map,:b,:c))"
  (serialize {:hfql/search "pour l'amour"}) := "(map,:hfql$search,'pour+l%27amour')"
  (serialize '(clojure.java.io/file "."))
  (serialize #inst "2025") := "(inst,'2025-01-01T00:00:00.000-00:00')"
  (serialize #inst "2025") := "(inst,'2025-01-01T00:00:00.000-00:00')"

  (serialize  #uuid "4380248c-4db9-4ec5-8cc5-31b982768290") := "(uuid,'4380248c-4db9-4ec5-8cc5-31b982768290')"
  )

;;;;;;;;;;;;;;
;; Decoding ;;
;;;;;;;;;;;;;;

(declare percent-decode decode-map decode -decode)

(defn- decode-map [object] (apply array-map (rest object)))
(defn- decode-string [object] (percent-decode object))
(defn- decode-ident [object]
  (if (qualified-ident? object)
    object
    (edn/read-string (percent-decode (str/replace-first (str object) "$" "/")))))

(defn- qualify [sym]
  {:pre [(symbol? sym)]}
  (if (qualified-symbol? sym)
    sym
    (symbol (namespace `_) (name sym))))

(defn- constructor [object]
  (when (seq? object)
    (let [[f & _args] object]
      (when (symbol? f)
        (let [resolved (qualify f)]
          (when-some [entry (or (find (methods decode) resolved)
                              (find (methods decode) f))]
            (key entry)))))))

(defmulti decode-dispatch type)
(defmethod decode-dispatch :default [object]
  (cond (seq? object) (or (constructor object) ::seq)
        :else (type object)))

(defmulti decode #'decode-dispatch)
(defmethod decode :default [object] (-decode object))
(defmethod decode `hyperfiddle.ednish/vec [object] (vec (rest object)))
(defmethod decode `hyperfiddle.ednish/map [object] (decode-map object))
(defmethod decode `hyperfiddle.ednish/set [object] (set (rest object)))
(defmethod decode 'inst [object] (clojure.instant/read-instant-date (second object)))
(defmethod decode 'uuid [object] #?(:clj (java.util.UUID/fromString (second object))
                                    :cljs (uuid (second object))))

(defprotocol Decodable
  (-decode [object]))

(extend-protocol Decodable
  #?(:clj String, :cljs js/String)
  (-decode [object] (decode-string object))

  Keyword
  (-decode [object] (decode-ident object))
  Symbol
  (-decode [object] (decode-ident object))

  #?(:clj Object, :cljs default)
  (-decode [object] object)
  nil
  (-decode [_] nil)
  )

(defn- percent-decode [v]
  (let [s (str v)]
    #?(:clj (java.net.URLDecoder/decode s "UTF-8")
       :cljs (-> s
               ;; URLDecoder turns "+" into space; decodeURIComponent doesn’t, so replace "+"→" " first
               (.replace #"\+" " ")
               js/decodeURIComponent))))


(tests
  (parse (serialize "foo")) := '("foo")
  (parse (serialize "foo bar")) := '("foo bar")
  (parse (serialize "foo,bar")) := '("foo,bar")
  (parse (serialize "\"pour\" l'amour")) := '("\"pour\" l'amour")

  (parse (serialize :foo)) := '(:foo)
  (parse (serialize 'foo)) := '(foo)
  (parse (serialize :foo/bar)) := '(:foo/bar)
  (parse (serialize 'foo/bar)) := '(foo/bar)
  (parse (serialize 'foo/bar/baz)) := '(foo/bar/baz)
  (parse (serialize :foo/bar$baz)) := '(:foo/bar$baz)
  (parse (serialize 'foo/bar$baz)) := '(foo/bar$baz)

  (parse (serialize ())) := '(())
  (parse (serialize '(foo))) := '((foo))
  (parse (serialize '(foo bar))) := '((foo bar))
  (parse (serialize '(()))) := '((()))
  (parse (serialize '(() ()))) := '((() ()))
  (parse (serialize '((() ()) (() ())))) := '(((() ()) (() ())))
  (parse (serialize '(((a) (b)) ((c) (d))))) := '((((a) (b)) ((c) (d))))

  (parse (serialize [])) := '([])
  (parse (serialize [[]])) := '([[]])
  (parse (serialize [:a :b])) := '([:a :b])

  (parse (serialize #{})) := '(#{})
  (parse (serialize #{#{}})) := '(#{#{}})
  (parse (serialize #{:a :b})) := '(#{:a :b})

  (parse (serialize {})) := '({})
  (parse (serialize {:a :b})) := '({:a :b})
  (parse (serialize {:a {:b :c}})) := '({:a {:b :c}})
  (parse (serialize {:hfql/search "pour l'amour"})) := '({:hfql/search "pour l'amour"})
  (parse (serialize '(clojure.java.io/file "."))) := '((clojure.java.io/file "."))

  (parse (serialize #inst "2025")) := '(#inst "2025")
  (parse (serialize  #uuid "4380248c-4db9-4ec5-8cc5-31b982768290")) := '(#uuid "4380248c-4db9-4ec5-8cc5-31b982768290")
)

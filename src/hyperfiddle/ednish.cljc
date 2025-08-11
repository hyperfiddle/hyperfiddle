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
            [clojure.set :as set]
            [hyperfiddle.rcf :as rcf :refer [tests]]
            [instaparse.core :as instaparse]
            #?(:cljs [goog.Uri]))
  #?(:clj (:import [clojure.lang IPersistentVector IPersistentMap IPersistentSet Symbol Keyword])
     :cljs (:import [cljs.lang IPersistentVector IPersistentMap IPersistentSet Symbol Keyword])))

(comment
  (rcf/enable!))

(def ^:private EDNISH-GRAMMAR
  "<Ednish>    = Expr+
   Sexpr       = <'('> Ednish* <')'>
   <Expr>      = ReaderLiteral / String / Atom / Sexpr / Separator
   ReaderLiteral = Unquote Separator+ Expr
   <Unquote>     = <'~'> Atom
   Atom        = #'^(?!\\~)[^(),{}\\[\\]]+'
   String      = <\"'\"> #\"[^']*\" <\"'\">
   <Separator> = <#',+'> ")

(def ^:private parser (instaparse/parser EDNISH-GRAMMAR #_#_:auto-whitespace :standard))

(declare decode)

(defn parse [string]
  (instaparse/transform {:Sexpr (fn [& content]
                                  (if (empty? content) ; distinguishes (nil) from () – aka unit
                                    (decode ())
                                    (decode content)))
                         :String decode
                         :ReaderLiteral (fn [tag value-repr] (decode (list (decode tag) value-repr)))
                         :Atom (comp decode edn/read-string)}
    (instaparse/parse parser string)))

;;;;;;;;;;;;;;
;; Encoding ;;
;;;;;;;;;;;;;;

(def ^:private RFC-3986-GEN-DELIMITERS #{\: \/ \? \# \[ \] \@}) ; reserved in URLs
(def ^:private RFC-3986-SUB-DELIMITERS #{\! \$ \& \' \( \) \* \+ \, \; \=}) ; URL safe, no need to %-encode
(def ^:private RFC-3986-UNRESERVED-CHARACTERS #{\- \. \~ \_}) ; URL safe, no meaning
(def ^:private EDNISH-RFC-3986-RESERVED-DELIMITERS #{\' ; string delimiter
                                                     \, ; separator
                                                     \+ ; URI space marker
                                                     \( \) ; sexpr separator
                                                     })
(def ^:private EDNISH-DELIMITERS (set/union RFC-3986-GEN-DELIMITERS RFC-3986-SUB-DELIMITERS RFC-3986-UNRESERVED-CHARACTERS EDNISH-RFC-3986-RESERVED-DELIMITERS))
(def ^:private EDNISH-RESERVED-DELIMITERS (-> (set/union RFC-3986-GEN-DELIMITERS EDNISH-RFC-3986-RESERVED-DELIMITERS)))
(def ^:priavte EDNISH-UNRESERVED-STRING-CHARS (-> (set/difference EDNISH-DELIMITERS EDNISH-RESERVED-DELIMITERS)
                                                (conj \:) ; Violates RFC-3986, but safe in URL path in practice.
                                                ))
(def ^:priavte EDNISH-UNRESERVED-ATOM-CHARS (-> (set/difference EDNISH-DELIMITERS EDNISH-RESERVED-DELIMITERS)
                                              (conj \:) ; Violates RFC-3986, but safe in URL path in practice.
                                              ))

(declare encode -encode percent-encode unescape-string constructor)

(defn encode-tagged-value [tag value]
  (let [encoded (encode (list (symbol (str "#" tag)) value))]
    (subs encoded 1 (dec (count encoded)))))

(defn- encode-seq [object] (str "(" (str/join "," (map encode object)) ")"))
(defn- encode-vector [object] (encode-tagged-value 'vec (sequence object)))
(defn- encode-set [object] (encode-tagged-value 'set (sequence object)))
(defn- encode-map [object] (encode-tagged-value 'map (sequence cat object)))
(defn- encode-string [object] (str "'" (unescape-string EDNISH-UNRESERVED-STRING-CHARS (percent-encode object)) "'"))
(defn- encode-ident [object]
  (-> (str
        (when (keyword? object) ":")
        (when (qualified-ident? object)
          (str (percent-encode (namespace object))
            "$"))
        (percent-encode (name object)))
    (str/replace #"^%23" "~") ; #inst -> ~inst
    ))

(defn encode-uuid [uuid] (encode-tagged-value 'uuid (str uuid)))

(letfn [(unescape-nested-string [str]
          (if (and (str/starts-with? str "\"") (str/ends-with? str "\""))
            (subs str 1 (dec (count str)))
            str))]
  (defn encode-inst [inst]
    (encode-tagged-value 'inst (-> inst (pr-str) (str/replace #"^#inst " "") (unescape-nested-string)))))

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
  (-encode [object] (encode-inst object))

  #?(:clj java.util.UUID, :cljs UUID) ; #uuid
  (-encode [object] (encode-uuid object))


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

  (serialize []) := "~vec,()"
  (serialize [[]]) := "~vec,(~vec,())"
  (serialize [:a :b]) := "~vec,(:a,:b)"

  (serialize #{}) := "~set,()"
  (serialize #{#{}}) := "~set,(~set,())"
  (serialize #{:a :b}) := "~set,(:b,:a)"

  (serialize {}) := "~map,()"
  (serialize {:a :b}) := "~map,(:a,:b)"
  (serialize {:a {:b :c}}) := "~map,(:a,~map,(:b,:c))"
  (serialize {:hfql/search "pour l'amour"}) := "~map,(:hfql$search,'pour+l%27amour')"
  (serialize '(clojure.java.io/file "."))
  (serialize #inst "2025") := "~inst,'2025-01-01T00:00:00.000-00:00'"

  (serialize  #uuid "4380248c-4db9-4ec5-8cc5-31b982768290") := "~uuid,'4380248c-4db9-4ec5-8cc5-31b982768290'"
  )

;;;;;;;;;;;;;;
;; Decoding ;;
;;;;;;;;;;;;;;

(declare percent-decode decode-map decode -decode)

(defn- decode-map [object] (apply array-map object))
(defn- decode-string [object] (percent-decode object))
(defn- decode-ident [object]
  (let [constructor (if (keyword? object) keyword symbol)]
    (if (qualified-ident? object) ; TODO verify if correct. Is it for the ::case? Not sure if that's a thing.
      (constructor (percent-decode (namespace object)) (percent-decode (name object)))
      (edn/read-string (percent-decode (str/replace-first (str object) "$" "/"))))))

(defn- qualify [sym]
  {:pre [(symbol? sym)]}
  (if (qualified-symbol? sym)
    sym
    (symbol (namespace `_) (name sym))))

(def ^:private INTERNAL-DATA-READERS {'vec #'vec, 'map #'decode-map, 'set #'set})

(def ^:dynamic *constructors* {})

(defn- resolve-tag-reader [symbol]
  {:pre [(symbol? symbol)]}
  (let [readers (merge default-data-readers clojure.core/*data-readers* INTERNAL-DATA-READERS *constructors*)]
    (or (get readers symbol)
      (get readers (qualify symbol)))))

(defmulti decode-dispatch type)
(defmethod decode-dispatch :default [object]
  (cond (seq? object)
        (let [[item & items] object]
          (cond (and (symbol? item) (resolve-tag-reader item)) `tagged-literal
                :else seq))
        :else (type object)))

(defmulti decode #'decode-dispatch)
(defmethod decode :default [object] (-decode object))
(defmethod decode `tagged-literal [[tag value :as obj]] ((resolve-tag-reader tag) value))

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

  (parse (serialize "")) := '("")
  (parse (serialize "foo")) := '("foo")
  (parse (serialize "foo bar")) := '("foo bar")
  (parse (serialize "foo,bar")) := '("foo,bar")
  (parse (serialize "(foo)")) := '("(foo)") 
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

  (parse (serialize #inst "2025") ) := '(#inst "2025")
  (parse (serialize  #uuid "4380248c-4db9-4ec5-8cc5-31b982768290")) := '(#uuid "4380248c-4db9-4ec5-8cc5-31b982768290")
  )

(tests
  (defmethod encode java.io.File [file] (encode (list 'clojure.java.io/file (.getPath file))))
  (defmethod encode clojure.lang.Namespace [ns] (encode (list `find-ns (ns-name ns))))
  (defmethod encode clojure.lang.Var [var] (encode (list `var (symbol var))))

  (let [file (clojure.java.io/file ".")]
    (binding [*constructors* {`clojure.java.io/file clojure.java.io/file}]
      (parse (serialize file))) := [file])

  (binding [*constructors* {`find-ns find-ns}]
    (parse (serialize *ns*))) := [*ns*]

  (binding [*constructors* {`var find-var}]
    (parse (serialize #'inc))) := [#'inc]
  )

;;;;;;;;;;;;;;;;;;;;;
;; Routing helpers ;;
;;;;;;;;;;;;;;;;;;;;;

(defn encode-uri-path [route & [home-route]]
  (if (= route home-route)
    "/"
    (str "/" (str/join "/" (map serialize route))))
  #_(str "/"
    (cond
      (empty? route) nil
      ((some-fn seq? vector?) route) ; not all sequential colls have [f & args] semantics (e.g. ordered maps)
      (let [[f & args] route]
        (condp = route
          home-route nil
          (str (serialize f)
            "/" (some->> args
                  (map serialize)
                  (str/join "/")))))
      () (serialize route))))

(defn- uri-ensure-no-implicit-authority [uri-path-string] 
  (if (str/starts-with? uri-path-string "//")
    (str "ednish://" uri-path-string)
    uri-path-string))

(defn- normalize-uri-path [uri-path-string]
  (as-> uri-path-string %
    (str %) ; nil support
    (if-not (str/starts-with? % "/") (str "/" %) %)
    (uri-ensure-no-implicit-authority %)
    ))

(defn- uri-parts
  "Return [path query fragment] (without ?/#). Works with full or relative URIs."
  [uri-path-string]
  #?(:clj  (let [u (java.net.URI. uri-path-string)]
             [(.getRawPath u) (.getRawQuery u) (.getRawFragment u)])
     :cljs (let [u (goog.Uri/parse uri-path-string)]
             [(.getPath u) (.getQuery u) (.getFragment u)])))

(tests "uri parts"
  (uri-parts (normalize-uri-path "/")) := ["/" nil nil]
  (uri-parts (normalize-uri-path "/?a")) := ["/" "a" nil]
  (uri-parts (normalize-uri-path "/#a")) := ["/" nil "a"]
  (uri-parts (normalize-uri-path "//")) := ["//" nil nil]
  (uri-parts (normalize-uri-path "//a")) := ["//a" nil nil])

(defn- uri-path-segments [uri-path-string]
  (-> uri-path-string (normalize-uri-path) (str/split #"/") (rest)))

(defn decode-uri-path [uri-path-string & [home-route]]
  {:pre [(string? uri-path-string)]}
  (let [[path query fragment] (uri-parts (normalize-uri-path uri-path-string))]
    (cond
      (empty? path) home-route
      (= "/" path)  home-route
      :else (map (comp first parse) (uri-path-segments path)))))

(tests
  "sexpr uri paths"
  (encode-uri-path `(user/hello)) := "/user$hello"
  (decode-uri-path "/user$hello") := `(user/hello)

  (encode-uri-path `(user/hello :a 1 "alice")) := "/user$hello/:a/1/'alice'"
  (decode-uri-path "/user$hello/:a/1/'alice'") := `(user/hello :a 1 "alice")

  "pass a home-route"
  (def home `(user/hello :default))
  (encode-uri-path `(user/hello :default) home) := "/"
  (decode-uri-path "/" home) := `(user/hello :default)
  (encode-uri-path `(user/hello :a 1 "alice") home) := "/user$hello/:a/1/'alice'"
  (decode-uri-path "/user$hello/:a/1/'alice'" home) := `(user/hello :a 1 "alice")

  "home-route edge cases"
  (uri-parts (normalize-uri-path "/")) := ["/" nil nil]
  (uri-parts (normalize-uri-path "/?a")) := ["/" "a" nil]
  (uri-parts (normalize-uri-path "/#a")) := ["/" nil "a"]
  (uri-parts (normalize-uri-path "//")) := ["//" nil nil]
  (uri-parts (normalize-uri-path "//a")) := ["//a" nil nil]

  (def home `(user/hello))

  "routes must pass java.net.URI"
  #?(:clj (some? (java.net.URI. (encode-uri-path `(user/hello)))) := true)

  "non-ascii characters"
  (encode-uri-path '(foo "!$&'[]()*+,;=|")) := "/foo/'!$&%27%5B%5D%28%29*%2B%2C;=%7C'"
  (decode-uri-path "/foo/'!$&%27%5B%5D%28%29*%2B%2C;=%7C'") := '(foo "!$&'[]()*+,;=|")

  "composite parameters"
  (encode-uri-path `(hyperfiddle.blog/post [:user/sub "google-oauth2|1234"] #{"events" "news"}))
  := "/hyperfiddle.blog$post/~vec,(:user$sub,'google-oauth2%7C1234')/~set,('news','events')"

  (decode-uri-path "/hyperfiddle.blog$post/~vec,(:user$sub,'google-oauth2%7C1234')/~set,('news','events')")
  := '(hyperfiddle.blog/post [:user/sub "google-oauth2|1234"] #{"news" "events"})

  (decode-uri-path "/bar/'qwerzxcv'") := '(bar "qwerzxcv")
  (decode-uri-path "/bar/((%3Fe,:foo,%3Fa))/'qwerzxcv'#blah") := '(bar ((?e :foo ?a)) "qwerzxcv")


  ;; User friendliness
  (encode-uri-path `(Toggle 1 2)) := "/hyperfiddle.ednish$Toggle/1/2"
  (encode-uri-path '("pour l'amour")) := "/'pour+l%27amour'"
  (decode-uri-path "/'pour+l’amour'") := '("pour l’amour")

  ;; nested structure is readable
  (encode-uri-path [:app.datomic-browser/attribute :abstractRelease/name {:contrib.gridsheet/search "pour l'amour"}])
   := "/:app.datomic-browser$attribute/:abstractRelease$name/~map,(:contrib.gridsheet$search,'pour+l%27amour')"

   (decode-uri-path "/:app.datomic-browser$attribute/:abstractRelease$name/~map,(:contrib.gridsheet$search,'pour+l%27amour')")
    := '(:app.datomic-browser/attribute :abstractRelease/name {:contrib.gridsheet/search "pour l'amour"})
  )
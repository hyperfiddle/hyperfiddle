(ns hyperfiddle.route
  (:require
    [cats.core :refer [>>=]]
    [cats.monad.either :as either]
    [clojure.spec.alpha :as s]
    [contrib.data :refer [orp rtrim-coll]]
    [contrib.ednish :as ednish :refer [decode-ednish encode-ednish]]
    [contrib.pprint :refer [pprint-str]]
    [contrib.rfc3986 :as rfc3986 :refer [decode-rfc3986-pchar encode-rfc3986-pchar]]
    [contrib.string :refer [empty->nil split-first]]
    [contrib.try$ :refer [try-either]]
    [cuerdas.core :as str]
    [hyperfiddle.fiddle]                                    ; for ::fiddle spec
    ))


(s/def ::fiddle (s/or
                  :ident :fiddle/ident
                  :dbid number?
                  ;:lookup ...
                  :uuid :fiddle/uuid))
(s/def ::datomic-args (s/or
                        :nil nil?
                        :seq (s/and seqable? seq)))
(s/def ::service-args any?)
(s/def ::fragment some?)

(s/def :hyperfiddle/route
  ; todo routes should be maps #243 and errors become sensible
  #_(s/keys
      :req [::fiddle]
      :opt [::datomic-args ::service-args ::fragment])
  (s/or
    :a (s/tuple ::fiddle)
    :b (s/tuple ::fiddle ::datomic-args)
    :c (s/tuple ::fiddle ::datomic-args ::service-args)
    :d (s/tuple ::fiddle ::datomic-args ::service-args ::fragment)))

(defn validate-route+ [route]
  (if (s/valid? :hyperfiddle/route route)
    (either/right route)
    (either/left (ex-info (str "Invalid route\n" (s/explain-str :hyperfiddle/route route))
                          (s/explain-data :hyperfiddle/route route)))))

(defn canonicalize "(apply canonicalize route)"
  [& [fiddle #_?fiddle-args ?datomic-args ?service-args ?initial-state]]
  (orp seq (rtrim-coll nil? [fiddle ?datomic-args ?service-args ?initial-state])))

(defn assoc-frag [[fiddle ?datomic-args ?service-args ?initial-state] frag]
  {:pre [(nil? ?initial-state)]}
  (canonicalize fiddle ?datomic-args ?service-args frag))

(defn dissoc-frag [[fiddle ?datomic-args ?service-args _]]
  (canonicalize fiddle ?datomic-args ?service-args nil))

(defn compare-routes [a b]
  (= (dissoc-frag a) (dissoc-frag b)))

(defn decoding-error [e s]
  [:hyperfiddle.system.route/decoding-error [s
                                             #?(:cljs (ex-message e) :clj (.getMessage e))
                                             (pprint-str (ex-data e))]])

(defn url-encode [route home-route]
  {:pre [(s/valid? :hyperfiddle/route route) (s/valid? :hyperfiddle/route home-route)]
   :post [(str/starts-with? % "/")]}
  (let [[fiddle datomic-args service-args frag] route
        fiddle-args []]
    (if (compare-routes route home-route)
      (str "/" (some->> frag empty->nil (str "#")))
      (case fiddle
        :hyperfiddle.system.route/decoding-error (first datomic-args)
        (str "/"
             (str/join ";" (->> (cons (ednish/encode-uri fiddle) (map ednish/encode-uri fiddle-args))))
             "/"
             (str/join "/" (map ednish/encode-uri datomic-args)) ; datomic args as path params is sensible default for userland

             ; hash and query aren't used today, todo i would prefer to encode as edn hashmap instead of k=v
             (if (seq service-args) (str "?" (str/join "&" (->> service-args (map (fn [[k v]] (ednish/encode-uri k "=" (ednish/encode-uri v))))))))
             (if (empty->nil frag) (str "#" (-> frag encode-ednish encode-rfc3986-pchar))))))))

(defn url-decode [s home-route]
  {:pre [(str/starts-with? s "/") (s/valid? :hyperfiddle/route home-route)]
   :post [(s/valid? :hyperfiddle/route %)]}
  (let [[path frag] (rfc3986/split-fragment s)]
    (if (= "/" path)
      (assoc-frag home-route frag)
      (-> (try-either
            (let [[root s] (split-first s "/")
                  [fiddle-segment s] (split-first s "/")
                  [fiddle & fiddle-args] (str/split fiddle-segment ";")
                  [s frag] (split-first s "#")
                  [datomic-args-segment query] (split-first s "?")
                  datomic-args (->> (str/split datomic-args-segment "/"))] #_"careful: (str/split \"\" \"/\") => [\"\"]"
              (canonicalize
                (ednish/decode-uri fiddle)
                ;(mapv -decode-url-ednish fiddle-args)
                (if-let [as (->> datomic-args (remove str/empty-or-nil?) seq)]
                  (mapv ednish/decode-uri as))
                (ednish/decode-uri query)
                (-> frag decode-rfc3986-pchar decode-ednish empty->nil))))
          (>>= validate-route+)
          (either/branch
            (fn [e] (decoding-error e s))
            identity)))))

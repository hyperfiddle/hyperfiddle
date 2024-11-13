(ns contrib.uri2 ; contrib.uri already taken in electric contrib. Not clear yet if hyperfiddle depends on electric or vice-versa.
  (:require
   [clojure.string :as str]))

(defn decode-query-param [^String value] (java.net.URLDecoder/decode value java.nio.charset.StandardCharsets/UTF_8))

(defn parse-query-string [query-string]
  (if (empty? query-string)
    {}
    (let [pairs (-> query-string
                  (str/split #"#")
                  (first)
                  (str/split #"&"))]
      (-> (into {} (map #(str/split % #"=" 2)) pairs)
        (update-vals decode-query-param)))))

(comment
  (parse-query-string "a=b&b===%3C&c=#foo")
  (-> (java.net.URI/create "http://localhost:8080/?token=foo")
    (.getQuery)
    (parse-query-string))
  )


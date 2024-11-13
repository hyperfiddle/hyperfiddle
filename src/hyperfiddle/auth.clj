;; Meant to be AOTed, do not ship source.
(ns hyperfiddle.auth
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [contrib.network :as net]
   [contrib.uri2 :as uri]
   [hyperfiddle.jwt :as jwt]))

(def PUBKEY (jwt/->pub-key "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAmARaIb1h5BKIurcDMHiSIXl5HsmeelvKQ4mtgBsg5E/NSSWTFUfsW9VI9bYY1lDRPugWldFiBQ6E5CI/2wJOt9vv8uU9fdPts6EOLL39J0PFb4A1dadI3qhhXypm3BurV8iS49gxCPkrc0XexMCnAlo+j04quX/DpkcysjWqsSuB2Kf2hKcd7kz81MebhN9ke5ZxeYLHpeq6vP1/zBEwRLaQeV7sMuVDzWShdcC4BbfjcjEELyTE8xi4rXKo0wLTp7utO9KhkLa1tCPl/Fflzl1jdgRbrOxxnRxWYAPWdBfHFDmLAcnN4waUQHNxVo6fpla9hM03oFD8bdo3jQkC9QIDAQAB"))

(def token (some-> (io/resource "hyperfiddle/token") slurp str/trim))

;;; Fingerprinting

(defn mac-address [network-interface]
  (when-let [mac (.getHardwareAddress network-interface)]
    (->> mac
      (map #(format "%02X" %))
      (apply str))))

(defn has-ip-address? [network-interface]
  (some #(instance? java.net.InetAddress %)
    (enumeration-seq (.getInetAddresses network-interface))))

(defn network-interface-group [network-interface]
  (second (re-find #"^[^0-9]*" (.getName network-interface))))

(defn sort-by-name-indexed
  "Given a collection of java.net.NetworkInterface, sort them by index (os detection order) then by name.
  I.e. given:
  - llw0 index 3
  - en5 index 1
  - en0 index 2
  Return '(en0 en5 llw0) because the 'enX' group contains the lowest index (en5), and en0 is the first lexicographic in the grouo."
  [network-interfaces]
  (let [groups (vals (group-by network-interface-group network-interfaces))
        sorted-groups (sort-by (fn [group] (apply min (map #(.getIndex %) group))) groups)]
    (mapcat (fn [group] (sort-by #(.getName %) group)) sorted-groups)))

(defn stable-mac-address []
  (or (->> (enumeration-seq (java.net.NetworkInterface/getNetworkInterfaces))
        ;; Filter out loopback, virtual, and inactive interfaces
        (filter #(and (not (.isLoopback %)) (.isUp %) (not (.isVirtual %)) (has-ip-address? %)))
        (sort-by-name-indexed)
        (some mac-address))
    "unknown"))

(comment
  (stable-mac-address)
  )

(defn machine-fingerprint ^bytes []
  (let [fingerprint (str (System/getProperty "os.name")
                      (System/getProperty "os.version")
                      (System/getProperty "user.name")
                      (or (stable-mac-address) "unknown"))]
    (.digest (java.security.MessageDigest/getInstance "SHA-256")
      (.getBytes fingerprint "UTF-8"))))

(comment
  (seq (machine-fingerprint)) ; := [104, -18, 46, 55, -78, 113, 25, -30, 15, -56, -87, 123, -42, 90, -88, -21, -61, -87, 42, 68, 108, -45, -1, -51, -80, -90, -96, 55, 127, -30, -36, -1]
  )

(defn sha256->str [bytes]
  (->> bytes
    (map (fn [b] (format "%02x" (bit-and b 0xff))))
    (apply str)))

(comment
  (sha256->str (machine-fingerprint)) ; := "68ee2e37b27119e20fc8a97bd65aa8ebc3a92a446cd3ffcdb0a6a0377fe2dcff"
  )

(defn xor-bytes [^bytes data ^bytes fingerprint-hash]
  (let [hash-length (count fingerprint-hash)]
    (byte-array
      (map-indexed (fn [i byte] (bit-xor byte (aget fingerprint-hash (mod i hash-length))))
        data))))

(defn base64 [str-or-bytes]
  (.encodeToString (java.util.Base64/getEncoder) (if (bytes? str-or-bytes) str-or-bytes (.getBytes str-or-bytes))))

(defn base64->str [str]
  (String. (.decode (java.util.Base64/getDecoder) str)))

(comment
  ((comp base64->str base64) "hello") := "hello"
  )

(defn encode-with-fingerprint
  ([^String base64-str] (encode-with-fingerprint base64-str (machine-fingerprint)))
  ([^String base64-str ^bytes fingerprint]
   (base64 (xor-bytes (.decode (java.util.Base64/getDecoder) base64-str) fingerprint))))

(defn decode-with-fingerprint
  ([^String base64-encoded-xored-str] (decode-with-fingerprint base64-encoded-xored-str (machine-fingerprint)))
  ([^String base64-encoded-xored-str ^bytes fingerprint]
   (String. (xor-bytes (.decode (java.util.Base64/getDecoder) base64-encoded-xored-str) fingerprint)
     "UTF-8")))

;;; Auth redirect

(defn http-server!
  "A minimalist http server. `on-request` takes a com.sun.net.httpserver.HttpServer instance, and a
  com.sun.net.httpserver.HttpExchange instance containing the request and onto
  which a response can be sent. Return the HttpServer instance. Callback return value is ignored.

  Usage:
  ```
  (def server
    (http-server! host 8080
      (fn [server exchange]
        (try
          (prn (-> exchange .getRequestURI .getQuery parse-query-string))
          (send-http-response! exchange 200 \"ok\")
          (catch Throwable t
            (send-http-response! exchange 500 \"server error\")
            (throw t))))))
  (.stop server 0) ; stop server
  ```
  "
  [host port request-callback]
  (let [server (-> host
                 (java.net.InetAddress/getByName)
                 (java.net.InetSocketAddress. port)
                 (com.sun.net.httpserver.HttpServer/create 0))
        handler (proxy [com.sun.net.httpserver.HttpHandler] []
                  (handle [^com.sun.net.httpserver.HttpExchange exchange]
                    (request-callback server exchange)))]
    (doto server
      (.createContext "/" handler)
      (.setExecutor nil)
      (.start))))

(defn send-http-response!
  ([^com.sun.net.httpserver.HttpExchange exchange response-content]
   (send-http-response! exchange 200 response-content))
  ([^com.sun.net.httpserver.HttpExchange exchange http-status-code response-content]
   (send-http-response! exchange http-status-code {} response-content))
  ([^com.sun.net.httpserver.HttpExchange exchange http-status-code headers-map response-content]
   (reduce (fn [headers [k v]] (doto headers (.set k v))) (.getResponseHeaders exchange) headers-map)
   (let [content (.getBytes response-content)]
     (.sendResponseHeaders exchange http-status-code (count content))
     (with-open [response (.getResponseBody exchange)]
       (.write response content)))))

(defn handle-auth-redirect! [callback]
  (let [host "localhost"
        port (net/find-open-port host 8081 9090) ; avoid clash on 8080, user apps usu. bind to 8080 immediately after auth.
        server (http-server! host port
                 (fn [^com.sun.net.httpserver.HttpServer server ^com.sun.net.httpserver.HttpExchange exchange]
                   (try
                     (callback (-> exchange .getRequestURI .getQuery uri/parse-query-string))
                     (send-http-response! exchange 200 {"Content-Type" "text/html"}
                       "<html><body><h1>Authentication Complete</h1><p>You can close this tab.</p><script>window.close()</script></body></html>")
                     (catch Throwable t (prn t) (throw t))
                     (finally (.stop server 0)))))]
    [(format "http://%s:%s" host port) #(.stop server 0)]))

(def AUTH-URL "https://hyperfiddle-auth.fly.dev/login")

(defn persist-token! [token-path token]
  (spit token-path (encode-with-fingerprint (base64 token))))

(defn prompt-user-for-login! [auth-url]
  (System/setProperty "apple.awt.UIElement" "true")
  (try (if (and false (java.awt.Desktop/isDesktopSupported) (.isSupported (java.awt.Desktop/getDesktop) java.awt.Desktop$Action/BROWSE))
         (.browse (java.awt.Desktop/getDesktop) (java.net.URI. auth-url))
         (throw (ex-info "Browser not available" {})))
       (catch Throwable _
         (println "Please sign up or login to activate: " auth-url))))

(defn login! [auth-url callback]
  (let [[redirect-uri cancel!]
        (handle-auth-redirect!
          (fn [query-params]
            (if-let [token (get query-params "token")]
              (callback token)
              (do (println "Failed to authenticate: incorrect authentication redirect.")
                  (callback nil)))))]
    (prompt-user-for-login! (format "%s?redirect-uri=%s" auth-url (java.net.URLEncoder/encode redirect-uri)))
    cancel!))

(defn get-token [token-path]
  (try (decode-with-fingerprint (slurp token-path))
       (catch Throwable _ nil)))

;;; Auth check

(defn valid-token? [token] (and (not-empty token) (jwt/valid-RS256? PUBKEY token)))

(defn ensure-user-auth! [token-path]
  (let [p (promise)]
    (if (valid-token? (get-token token-path))
      (deliver p true)
      (login! AUTH-URL
        (fn [token]
          (try (if (empty? token)
                 (deliver p false)
                 (do (persist-token! token-path token)
                     (deliver p true)))
               (catch Throwable t
                 (prn (ex-info "Post authentication error" {} t))
                 (deliver p false))))))
    p))

(comment
  (java.net.URI. (format "%s?redirect-uri=%s" AUTH-URL "http://localhost:8081"))
  (net/port-available? "localhost" 8080)
  (get-token "hf.token")
  (def cancel! (login! AUTH-URL prn))
  (cancel!)

  (let [ensure (ensure-user-auth! "hf.token")]
    (prn "ensuring token…")
    (prn "done: " @ensure))
  )
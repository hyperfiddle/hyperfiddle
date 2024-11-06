(ns hyperfiddle.jwt
  (:import (com.auth0.jwt JWT)
           (com.auth0.jwt.algorithms Algorithm)))

(defn auth0-domain->issuer [auth0-domain]
  (str "https://" auth0-domain "/"))

;; RS256 (asymmetric RSA keys)
(defn build-jwks-provider [auth-domain]
  (-> (new com.auth0.jwk.JwkProviderBuilder auth-domain)
    (.cached 2, 5, java.util.concurrent.TimeUnit/MINUTES)
    (.build)))

(defn get-public-key [jwks-provider token]
  (let [jwt        (com.auth0.jwt.JWT/decode token)
        key-id     (.getKeyId jwt)
        jwk        (.get jwks-provider key-id)]
    (.getPublicKey jwk)))

(defn build-RS256-verifier
  ([^java.security.PublicKey public-key]
   (build-RS256-verifier nil public-key))
  ([auth-domain ^java.security.PublicKey public-key]
   (let [alg (com.auth0.jwt.JWT/require (com.auth0.jwt.algorithms.Algorithm/RSA256 public-key nil))]
     (.build (if auth-domain (.withIssuer alg auth-domain) alg)))))

(defn parse-claims [claims]
  (-> (into {} claims)
    (update-vals #(.as % Object))
    (update-keys keyword)))

(defn verify-RS256* [verifier token] ; throws com.auth0.jwt.exceptions.TokenExpiredException
  (.verify verifier token))

(defn verify-RS256 [pubkey token] (verify-RS256* (build-RS256-verifier pubkey) token))

(defn get-RS256-token-claim [verifier token]
 (parse-claims (.getClaims (verify-RS256* verifier token))))

(defn max-age [claims] (- (:exp claims) (quot (System/currentTimeMillis) 1000)))

(def key-factory (java.security.KeyFactory/getInstance "RSA"))

(defn ->pub-key [encoded-pub-key]
  (->> (.decode (java.util.Base64/getDecoder) encoded-pub-key)
    (new java.security.spec.X509EncodedKeySpec)
    (.generatePublic key-factory)))

(defn ->private-key [encoded-private-key]
  (->> (.decode (java.util.Base64/getDecoder) encoded-private-key)
    (new java.security.spec.PKCS8EncodedKeySpec)
    (.generatePrivate key-factory)))

(defn duration->date
  "Return a java.util.Date set at `java.util.Date now` + `java.time.Duration from-now` in the future.
  `now` defaults to current machine time. Useful to set tokens `exp` expiration time, or similar token times.

  e.g. (duration->date (java.time.Duration/ofDays 1)) ; => tomorrow, same time as current machine time
       (duration->date #inst \"2024-01-01T00:00:00.000-00:00\" (java.time.Duration/ofDays 1)) ; => 2024-01-02 at midnight"
  ^java.util.Date
  ([^java.time.Duration duration-from-now]
   (duration->date (java.util.Date.) duration-from-now))
  ([^java.util.Date now ^java.time.Duration from-now]
   (java.util.Date. (+ (.getTime now) (.toMillis from-now)))))

(defn ->jwt
  "Generate a RS256 JWT token, signed with an RSA `java.security.PrivateKey`, and
  containing claims as json-encoded key-values from `claims-map`. Claims-map's
  keys must be strings. Claims-map values must be of type Boolean, Integer,
  Long, Double, String, java.time.Instant, java.util.Date, java.util.List,
  java.util.Map or java array of Integer, Long or String. List values can be of
  any listed type, plus nil. Map values can be of any listed types, but cannot
  be nil."
  ([^java.security.PrivateKey private-key claims-map]
   (-> (com.auth0.jwt.JWT/create)
     (.withPayload claims-map)
     (.sign (com.auth0.jwt.algorithms.Algorithm/RSA256 nil private-key)))))

(defn valid-RS256? [^java.security.PublicKey pubkey token]
  (try (and token (verify-RS256 pubkey token))
    (catch Throwable _ false)))

;; HS256 – legacy, signed with a secret

(defn build-HS256-verifier [secret auth0-domain]
  (let [jwt-verifier (-> (Algorithm/HMAC256 secret)
                       (JWT/require)
                       (.withIssuer (auth0-domain->issuer auth0-domain))
                       (.build))]
    (fn [token]
      (-> (.verify jwt-verifier token)
        (.getClaims)
        (parse-claims)))))

(defn sign [claims secret & [options]]
  (let [jwt-builder (JWT/create)]
    (doseq [[k v] claims]
      (.withClaim jwt-builder (name k) v))
    (.sign jwt-builder (Algorithm/HMAC256 secret))))

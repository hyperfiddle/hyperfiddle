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

(defn verify-RS256 [verifier token] ; throws com.auth0.jwt.exceptions.TokenExpiredException
  (.verify verifier token))

(defn get-RS256-token-claim [verifier token]
 (parse-claims (.getClaims (verify-RS256 verifier token))))

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

(defn ->jwt
  "Generate a RS256 JWT token, signed with an RSA `java.security.PrivateKey`,
  expiring after a given `java.time.Duration`, and containing claims as
  json-encoded key-values from `claims-map`."
  [^java.security.PrivateKey private-key
   ^java.time.Duration expire-duration
   claims-map]
  (as-> (com.auth0.jwt.JWT/create) $
    (reduce (fn [token [k v]] (.withClaim token (name k) v)) $ claims-map)
    (.withExpiresAt $ (new java.util.Date (+ (System/currentTimeMillis) (.toMillis expire-duration))))
    (.sign $ (com.auth0.jwt.algorithms.Algorithm/RSA256 nil private-key))))

(defn valid-RS256? [^java.security.PublicKey pubkey token]
  (and token
    (.verify (build-RS256-verifier pubkey) token)))

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

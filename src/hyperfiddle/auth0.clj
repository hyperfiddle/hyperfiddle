(ns hyperfiddle.auth0
  (:require
   [contrib.base64 :as base64]
   [hyperfiddle.jwt :as jwt])
  (:import
   (com.auth0.client.auth AuthAPI)))

(defn stateless-login-url [config current-uri]
  (let [{:keys [domain client-id redirect-uri]} (:auth0 config)]
    (str "https://" domain "/"
      "authorize?"
      "client_id=" client-id
      "&response_type=" "code"
      "&scope=" "openid email profile"
      "&state=" (base64/base64-encode-url-safe current-uri)
      "&redirect_uri=" redirect-uri)))

(defn fetch-id-token! [auth0 oauth-authorization-code]
  (let [{:keys [domain client-id client-secret redirect-uri]} auth0
        auth (AuthAPI. domain client-id client-secret)
        req (.exchangeCode auth oauth-authorization-code redirect-uri)
        executed (.execute req)]
    (.getIdToken (.getBody executed))))

(def build-jwks-provider (memoize jwt/build-jwks-provider)) ; one entry per auth domain, at most one per running jvm
(def build-RS256-verifier (memoize jwt/build-RS256-verifier)) ; one entry per auth domain×public-key, at most two public keys for auth0, unless there's key rotation – rare.

(defn login-RS256! [{:keys [domain] :as auth0} oauth-authorization-code]
  (let [encoded-id-token (fetch-id-token! auth0 oauth-authorization-code)
        auth-domain (jwt/auth0-domain->issuer domain)
        provider   (build-jwks-provider auth-domain)
        public-key (jwt/get-public-key provider encoded-id-token)
        verifier   (build-RS256-verifier auth-domain public-key)]
    (Thread/sleep 1000)  ; Auth0 issues a token at time T and verify will fail if we check before T – due to misaligned machine clocks
    (let [claim (jwt/get-RS256-token-claim verifier encoded-id-token)]
      [encoded-id-token ; no need to re-sign token, recipient will need to re-validate with public key
       claim
       (jwt/max-age claim)])))

(defn valid-token? [domain jwt]
  (when (some? jwt)
    (let [auth-domain (jwt/auth0-domain->issuer domain)
          provider   (build-jwks-provider auth-domain)
          public-key (jwt/get-public-key provider jwt)
          verifier   (build-RS256-verifier auth-domain public-key)]
      (some? (jwt/verify-RS256 verifier jwt)))))
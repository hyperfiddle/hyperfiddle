;;; Secret ns meant to be AOT compiled, do not ship source code.
(ns hyperfiddle.entrypoint
  (:require
   [hyperfiddle.auth :as auth]
   [hyperfiddle.electric.impl.lang3 :as lang]
   [hyperfiddle.electric.impl.runtime3 :as r]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.jwt :as jwt]))

;; Important clever macro tricks: We don't want users to see the entrypoint.
;; Even if we AOT this namespace, if we expose `boot-server` as a macro, users
;; might just macroexpand it and make their own entrypoint. How? They could look
;; at the similar client entrypoint and tweak the result. We cannot AOT the
;; client entrypoint, code must be available to shadow-cljs. Instead we expose a
;; macro quoting args and delegating macroexpand to the runtime. The server-generated code doesn't escape into userland.

(def ^:dynamic ^:macro gen) ; a macro var without impl, so users can't call it.

(let [gen-server (fn [_&form &env opts Main & args] ; macro internal arity (similar to what defmacro desugares to)
                   (let [env (merge (lang/normalize-env &env) e/web-config opts)
                         source (lang/->source env ::e/Main `(e/fn [] (e/$ ~Main ~@args)))]
                     `(clojure.core/fn [subject#]
                        (r/peer-events
                          (r/make-peer :server ~(select-keys opts [:cognitect.transit/read-handlers :cognitect.transit/write-handlers])
                            subject# (r/->defs {::e/Main ~source}) ::e/Main nil)))))]
  (defn boot-server* [[ns-sym lexicals opts Main & args]]
    (binding [gen gen-server ; set the gen macro impl
              *ns* (find-ns ns-sym)] ; ensures eval runs in original ns to resolve ns requires and aliases
      (eval `(fn [~@lexicals] (gen ~opts ~Main ~@args))) ; wrapped in fn so gen's &env contains `lexicals` as LocalBindings
      )))

(defmacro boot-server [opts Main & args]
  ;; FIXME security issue, we check token at compile time. Users can snapshot
  ;; this macro's output and get away with token verification.
  (cond
    (not auth/token) (throw (ex-info "Missing auth token, please run `./authenticate.sh`" {})) ; TODO improve message
    (not (jwt/valid-RS256? auth/PUBKEY auth/token))
    (let [ex-message (try (jwt/verify-RS256 auth/PUBKEY auth/token) nil (catch Throwable t (ex-message t)))]
      (throw (ex-info (str "Invalid token, renew it with `./authenticate.sh` " ex-message) {}))) ; TODO improve message
    () (let [lexicals (vec (keys &env))]
         `((boot-server* '[~(.name *ns*) ~lexicals ~opts ~Main ~@args]) ; capture ns and lexical bindings where macroexpand is happening
           ~@lexicals))))
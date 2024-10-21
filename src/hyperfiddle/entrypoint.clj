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

(defn authenticate [ring-request boot-fn]
  (if (jwt/valid-RS256? auth/PUBKEY (get-in ring-request [:cookies "jwt" :value]))
    boot-fn
    (throw (ex-info "Booting an electric server requires authentication" {}))
    ))

(def ^:dynamic ^:macro gen) ; a macro var without impl, so users can't call it.

(let [gen-server (fn [_&form &env opts Main & args] ; macro internal arity (similar to what defmacro desugares to)
                   (let [env (merge (lang/normalize-env &env) e/web-config opts)
                         source (lang/->source env ::e/Main `(e/fn [] (e/$ ~Main ~@args)))]
                     `(clojure.core/fn [subject#]
                        (r/peer-events
                          (r/make-peer :server ~(select-keys opts [:cognitect.transit/read-handlers :cognitect.transit/write-handlers])
                            subject# (r/->defs {::e/Main ~source}) ::e/Main nil)))))]
  (defn boot-server* [[ns-sym lexicals ring-request opts Main & args]]
    (binding [gen gen-server ; set the gen macro impl
              *ns* (find-ns ns-sym)] ; ensures eval runs in original ns to resolve ns requires and aliases
      (eval `(fn [~@lexicals] (authenticate ~ring-request (gen ~opts ~Main ~@args)))) ; wrapped in fn so gen's &env contains `lexicals` as LocalBindings
      )))

(defmacro boot-server [ring-request opts Main & args]
  (let [lexicals (vec (keys &env))]
    `((boot-server* '[~(.name *ns*) ~lexicals ~ring-request ~opts ~Main ~@args]) ; capture ns and lexical bindings where macroexpand is happening
      ~@lexicals)))
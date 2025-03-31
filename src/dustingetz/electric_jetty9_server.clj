(ns dustingetz.electric-jetty9-server
  (:import [org.eclipse.jetty.server Handler]
           [org.eclipse.jetty.server.handler ContextHandler HandlerList])
  (:require [hyperfiddle.electric-jetty9-ring-adapter :refer [proxy-ws-handler]]))

#_(def VERSION (not-empty (System/getProperty "ELECTRIC_USER_VERSION"))) ; see Dockerfile
#_(defn wrap-reject-stale-client
    "Intercept websocket UPGRADE request and check if client and server versions matches.
    An electric client is allowed to connect if its version matches the server's version, or if the server doesn't have a version set (dev mode).
    Otherwise, the client connection is rejected gracefully."
    [next-handler]
    (fn [ring-req]
      (let [client-version (get-in ring-req [:query-params "ELECTRIC_USER_VERSION"])]
        (cond
          (nil? VERSION)             (next-handler ring-req)
          (= client-version VERSION) (next-handler ring-req)
          :else (adapter/reject-websocket-handler 1008 "stale client") ; https://www.rfc-editor.org/rfc/rfc6455#section-7.4.1
          ))))

(defn electric-ws-adapter
  ([boot-fn] (fn [ring-req] (electric-ws-adapter ring-req boot-fn)))
  ([ring-req boot-fn] (hyperfiddle.electric-jetty9-ring-adapter/electric-ws-adapter ring-req boot-fn)))

(defn jetty-install-ws
  ([jetty-server path ring-middleware] (jetty-install-ws jetty-server path ring-middleware {}))
  ([jetty-server path ring-middleware
    {:as config :keys [ws-max-idle-time ws-max-text-message-size] :or {ws-max-idle-time 500000 ws-max-text-message-size 65536}}] ; copied from proxy-ws-handler for documentation
   (letfn [(create-websocket-handler [context-path handler]
             (doto (ContextHandler.)
               (.setContextPath context-path) ; matches e.g. "/"
               (.setAllowNullPathInfo false) ; FIXME can we remove this? not sure what it does for just "/". It's really up to the user to canonicalize urls. https://javadoc.jetty.org/jetty-9/org/eclipse/jetty/server/handler/ContextHandler.html#setAllowNullPathInfo(boolean)
               (.setHandler (proxy-ws-handler handler config))))
           (add-websocket-handler [server path ring-middleware]
             (let [handlers [(create-websocket-handler path ring-middleware) (.getHandler server)]]
               (.setHandler server (doto (HandlerList.) (.setHandlers (into-array Handler handlers))))))]
     (doto jetty-server
       (add-websocket-handler path ring-middleware)))))

(comment
  ;; No middleware
  (jetty-install-ws jetty-server "/" (electric-ws-adapter (fn [ring-req] (e/boot-server {} electric-starter-app.main/Main (e/server datomic-uri) (e/server ring-req)))))

  ;; With ring middlewares
  (jetty-install-ws jetty-server "/"
    (-> (electric-ws-adapter ; last in middleware stack
          (fn [ring-req] (e/boot-server {} electric-starter-app.main/Main (e/server datomic-uri) (e/server ring-req))))
      #_(ring.middleware.cookies/wrap-cookies) ; makes cookies available to Electric app
      #_(wrap-reject-stale-client)
      (ring.middleware.params/wrap-params))))

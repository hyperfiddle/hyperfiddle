(ns hyperfiddle.entrypoint
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.router4 :as r]))

(e/declare apps)

(e/defn Index []
  (dom/h1 (dom/text "Hyperfiddle"))
  (dom/menu
    (e/for [[app-name App] (e/diff-by key (dissoc apps `Index))]
      (dom/li (r/link ['/ [app-name]] (dom/text (name app-name)))))))

(e/defn NotFoundPage [& args]
  (e/client
    (dom/h1 (dom/text "Page not found"))
    (r/link ['/ []] (dom/text "index"))))

(e/defn DefaultApps []
  {`Index Index})

(defmacro rebooting [sym & body] `(e/for [~sym (e/diff-by identity (e/as-vec ~sym))] ~@body)) ; TODO remove

(e/defn Hyperfiddle [user-apps & {:keys [default] :or {default `(Index)}}]
  (r/router (r/HTML5-History)
    (let [[app-name & _] r/route]
      (if-not app-name
        (r/ReplaceState! ['. default])
        (let [apps (merge (DefaultApps) user-apps)
              App (get apps app-name NotFoundPage)]
          (set! (.-title js/document) (str (some-> app-name name (str " – ")) "Hyperfiddle"))
          (binding [hyperfiddle.entrypoint/apps apps]
            (rebooting App ; FIXME navigation can crash
              (r/pop (App)))))))))


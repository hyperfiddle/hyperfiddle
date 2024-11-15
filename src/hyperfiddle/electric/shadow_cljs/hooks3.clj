(ns hyperfiddle.electric.shadow-cljs.hooks3
  (:require [shadow.build.compiler]
            [hyperfiddle.electric.impl.lang3 :as lang]
            [hyperfiddle.electric.impl.cljs-analyzer2 :as cljs-ana]
            [hyperfiddle.auth :as auth] ; hook only runs if user is authenticated
            ))

;; Shadow-cljs doesn't expose a way to act before compiling a cljs file.
;; It filters resources in a series of functions, calling `do-compile-cljs-resource` in the end.
;; So we wrap this final step and alter the var.
(defonce original-do-compile-cljs-resource shadow.build.compiler/do-compile-cljs-resource)
(def !built-this-cycle (atom #{}))      ; build once per cycle
(defonce first-compile? true)           ; on first compile we don't need to recompile
(defn wrapped-do-compile-cljs-resource [state {ns$ :ns :as rc} source]
  (swap! lang/!a cljs-ana/purge-ns ns$)
  (when (and (not (@!built-this-cycle ns$)) (some-> (find-ns ns$) meta ::lang/has-edef?))
    (prn ::recompile-clj ns$)
    (require ns$ :reload))
  (original-do-compile-cljs-resource state rc source))

;; Users must use this hook for stable dev builds. Not required for prod builds.
;; Users must authenticate to use this hook.
(defn reload-clj "On `e/defn` change, recompile Clojure namespace (because the expression
  may contain e/client and/or e/server). Prevents double-reloads (e.g. from :require-macros)."
  {:shadow.build/stages #{:compile-finish :compile-prepare}} [build-state]
  (let [build-state
        (if (= :compile-prepare (:shadow.build/stage build-state))
          (when (deref (auth/ensure-user-auth! (str (.getPath (:cache-dir build-state)) "/hyperfiddle.electric.token")))
            (assoc build-state :hyperfiddle.electric.shadow-cljs/hooks3 #{`reload-clj})) ; inform e/client this hook is loaded
          build-state)]
    (when first-compile?
      (println "Compiling ..."); Prevent user thinking auth is bugged on first run. Because compilation takes some time after auth.
      (alter-var-root #'first-compile? not)
      (alter-var-root #'shadow.build.compiler/do-compile-cljs-resource (constantly #'wrapped-do-compile-cljs-resource)))
    (reset! !built-this-cycle #{})
    build-state))
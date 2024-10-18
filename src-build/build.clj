(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'com.hyperfiddle/hyperfiddle)
(def version (b/git-process {:git-args "describe --tags --long --always --dirty"}))
(def basis (b/create-basis {:project "deps.edn", :aliases [:build-fix]}))
(def class-dir "target/classes")
(def defaults {:src-pom "src-build/pom-template.xml" :lib lib :class-dir class-dir})

(defn clean [_opts] (b/delete {:path "target"}))

(defn jar [{:keys [version] :or {version version}}]
  (clean nil)
  (let [jar-file (format "target/%s-%s.jar" (name lib) version)
        opts (assoc defaults
               :version    version
               :basis      basis
               :class-dir  class-dir
               :jar-file   jar-file
               :scm        {:tag version}
               :src-dirs   ["src"])]
    (println "Writing pom.xml")
    (b/write-pom opts)
    (println "Copying resources to" class-dir)
    (b/copy-dir {:src-dirs ["src"], :target-dir class-dir
                 :ignores ["entrypoint.clj" "auth.clj" "jwt.clj" "auth0.clj"]})
    (b/compile-clj {:basis basis
                    :class-dir class-dir
                    :ns-compile '[hyperfiddle.entrypoint hyperfiddle.auth hyperfiddle.jwt hyperfiddle.auth0]
                    :filter-nses '[hyperfiddle.entrypoint hyperfiddle.auth hyperfiddle.jwt hyperfiddle.auth0]})
    (println "Building jar" jar-file)
    (b/jar opts)))

;; No install nor deploy tasks, we are still figuring out delivery.
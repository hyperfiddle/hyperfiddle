(ns build
  (:require [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd]))

(def lib 'com.hyperfiddle/hyperfiddle)
(def version "v0-alpha-SNAPSHOT" #_(b/git-process {:git-args "describe --tags --long --always --dirty"}))
(def basis (b/create-basis {:project "deps.edn" :aliases [:release]}))
(def class-dir "target/classes")
(def defaults {:src-pom "src-build/pom-template.xml" :lib lib :class-dir class-dir})

(defn clean [_opts] (b/delete {:path "target"}))

(defn build [{:keys [version jar-file] :or {version version}}]
  (clean nil)
  (let [jar-file (or (some-> jar-file str) (format "target/%s-%s.jar" (name lib) version))
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
    (b/copy-dir {:src-dirs ["src" "resources"], :target-dir class-dir})
    (println "Building jar" jar-file)
    (b/jar opts)))

;; No install nor deploy tasks, we are still figuring out delivery.

(defn install [{:keys [version] :or {version version}}]
  (let [jar-file (format "target/%s-%s.jar" (name lib) version)]
    (b/install {:basis      basis
                :lib        lib
                :version    version
                :jar-file   jar-file
                :class-dir  class-dir})))

(defn deploy [opts] ; clojars
  (let [{:keys [lib version class-dir installer jar-file] :as opts} (merge defaults opts)]
    (assert version ":version is required to deploy")
    (when (and installer (not= :remote installer))
      (println ":installer" installer "is deprecated -- use install task for local deployment"))
    (let [jar-file (or jar-file (format "target/%s-%s.jar" (name (or lib 'application)) version))]
      (dd/deploy (merge {:installer :remote :artifact (b/resolve-path jar-file)
                         :pom-file (b/pom-path {:lib lib :class-dir class-dir})}
                   opts)))))
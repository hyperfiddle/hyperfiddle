(ns build
  (:require
   [clojure.java.io :as io]
   [clojure.tools.build.api :as tools.build]
   [hyperfiddle.build :as build]))

(def clean #'build/clean)
(def install #'build/install)
(def deploy #'build/deploy)

(defn import-demos! [& _]
  (doseq [[demo-source-path demo-target-path] (:hyperfiddle/imported-demos (build/create-basis))]
    (println "Importing" demo-source-path "->" demo-target-path)
    (let [target (io/file demo-target-path)]
      (when-not (.exists target)
        (io/make-parents target)
        (io/copy (io/file demo-source-path) target)))))

(defn prep [& args]
  (apply clean args)
  (apply import-demos! args))

(defn build
  ([opts] (build (build/create-basis :aliases [:release]) opts))
  ([basis opts]
   (clean basis opts)
   (let [{:keys [class-dir src-dirs] :as opts} (build/defaults basis opts)]
     (tools.build/write-pom opts)
     (tools.build/copy-dir {:src-dirs src-dirs, :target-dir class-dir})
     (import-demos!)
     (tools.build/jar opts))))
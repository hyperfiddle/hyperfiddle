(ns hyperfiddle.test-runner
  (:refer-clojure :exclude [test])
  (:require [cider.nrepl.print-method]           ; activate #function[...] printing
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test]
            [clojure.tools.namespace.find :as ns-find]))

(defn test
  "Discover and run all tests in :dir, excluding namespaces matching :exclude patterns.
  Only requires namespaces matching :patterns, so unrelated code is never loaded.
  All options have defaults in :exec-args in deps.edn."
  [{:keys [dir patterns exclude]}]
  (let [exclude-pats  (map re-pattern exclude)
        excluded?     (fn [ns-sym] (some #(re-matches % (str ns-sym)) exclude-pats))
        pattern       (re-pattern (str/join "|" patterns))
        all-nses      (ns-find/find-namespaces-in-dir (io/file dir))
        nses          (->> all-nses (remove excluded?) (filter #(re-find pattern (str %))))]
    (doseq [ns nses] (require ns))
    (let [result (clojure.test/run-all-tests pattern)]
      (when (or (pos? (:fail result 0)) (pos? (:error result 0)))
        (System/exit 1)))))

(ns hyperfiddle.test-runner
  (:refer-clojure :exclude [test])
  (:require [cider.nrepl.print-method]           ; activate #function[...] printing
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test]
            [clojure.tools.namespace.find :as ns-find]))

(defn test
  "Discover and run all tests in :dir, excluding namespaces matching :exclude patterns.
  All options have defaults in :exec-args in deps.edn."
  [{:keys [dir patterns exclude]}]
  (let [exclude-pats  (map re-pattern exclude)
        excluded?     (fn [ns-sym] (some #(re-matches % (str ns-sym)) exclude-pats))
        all-nses      (ns-find/find-namespaces-in-dir (io/file dir))
        nses          (remove excluded? all-nses)
        pattern       (str/join "|" patterns)]
    (doseq [ns nses] (require ns))
    (let [result (clojure.test/run-all-tests (re-pattern pattern))]
      (when (or (pos? (:fail result 0)) (pos? (:error result 0)))
        (System/exit 1)))))

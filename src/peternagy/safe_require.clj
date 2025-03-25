(ns peternagy.safe-require)

(defonce previous-require clojure.core/require)
(defn safe-require [& args]
  (locking clojure.lang.RT/REQUIRE_LOCK
    (apply previous-require args)))

(alter-var-root #'clojure.core/require (constantly safe-require))

(prn 'require 'patched)

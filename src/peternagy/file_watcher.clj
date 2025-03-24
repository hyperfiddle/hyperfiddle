;; copied and adapted from leo.file-watcher
(ns peternagy.file-watcher
  (:require [missionary.core :as m])
  (:import [java.nio.file Path FileSystems Paths WatchEvent$Modifier StandardWatchEventKinds
            StandardWatchEventKinds$StdWatchEventKind WatchEvent]
           [com.sun.nio.file SensitivityWatchEventModifier]
           [java.io File]))

(defn path [s & ss] (Paths/get ^String s (into-array String ss)))

(def events [StandardWatchEventKinds/ENTRY_MODIFY])
(def modifiers [SensitivityWatchEventModifier/HIGH])

(defn watch-dir [^Path dir]
  (m/ap
    (let [ws (.newWatchService (FileSystems/getDefault))
          key (.register dir ws
                (into-array StandardWatchEventKinds$StdWatchEventKind events)
                (into-array WatchEvent$Modifier modifiers))]
      (try
        (loop []
          (m/? (m/via m/blk (.take ws)))
          (m/amb> (.context ^WatchEvent (m/?> (m/seed (.pollEvents key))))
            (do (.reset key) (recur))))
        (catch Throwable e
          (.cancel key)
          (throw e))))))

(defn watch-file [^File file]
  (let [path (.toPath file), name (.getFileName path)]
    (->> (m/ap
           (m/?< (->> (watch-dir (.getParent (Paths/get (.toURI file))))
                   (m/eduction (filter #{name}))
                   (m/reductions {} path)))
           file)
      (m/relieve {}))))

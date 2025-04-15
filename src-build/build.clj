(ns build
  (:require [hyperfiddle.build :as build]))

(def clean #'build/clean)
(def build (partial build/build (build/create-basis :aliases [:release])))
(def install #'build/install)
(def deploy #'build/deploy)


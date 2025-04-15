#!/bin/sh
set -eux -o pipefail

clojure -T:build build
clojure -T:build install
#env $(cat .env | xargs) clojure -T:build deploy
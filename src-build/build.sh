#!/bin/sh
set -eux -o pipefail

clojure -T:build clean
HYPERFIDDLE_HYPERFIDDLE_BUILD="v0-alpha-SNAPSHOT"
clojure -T:build build :version '"'$HYPERFIDDLE_HYPERFIDDLE_BUILD'"'
clojure -T:build install :version '"'$HYPERFIDDLE_HYPERFIDDLE_BUILD'"'
#env $(cat .env | xargs) clojure -T:build deploy :version '"'$HYPERFIDDLE_HYPERFIDDLE_BUILD'"'
#env $(cat .env | xargs) clojure -T:build deploy :version '"'v0-alpha-SNAPSHOT'"'
# Build clojars maven artifact — Hyperfiddle

```shell
git tag v0-alpha  # manually set a new tag, or skip to use commit distance from current tag
clojure -T:build clean
HYPERFIDDLE_HYPERFIDDLE_BUILD="v0-alpha-SNAPSHOT"
echo $HYPERFIDDLE_HYPERFIDDLE_BUILD
clojure -T:build jar :version '"'$HYPERFIDDLE_HYPERFIDDLE_BUILD'"'
clojure -T:build install :version '"'$HYPERFIDDLE_HYPERFIDDLE_BUILD'"'
# To test in electric-starter-app:
clj -A:dev -X dev/-main -Sdeps '{:deps {com.hyperfiddle/electric {:mvn/version "'$HYPERFIDDLE_HYPERFIDDLE_BUILD'"}}}'
# No way to test remote clojars version without rm in .m2/repositories/com/hyperfiddle
# Optional: test electric-starter-app with local maven install
env $(cat .env | xargs) clojure -T:build deploy :version '"'$HYPERFIDDLE_HYPERFIDDLE_BUILD'"'
```

- `CLOJARS_USERNAME` is your clojars username.
- `CLOJARS_PASSWORD` is not your account password, but rather a genareted token granting
  deploy rights to the target coordinates.
- idea: how to run tests cli? (No need, deployed artifacts already passed CI)

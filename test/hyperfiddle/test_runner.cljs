(ns hyperfiddle.test-runner
  (:require
    [doo.runner :refer-macros [doo-tests]]

    ; NOT clj tests, this is only for CLJS tests
    contrib.char$-test
    contrib.data-test
    #_contrib.datomic-errors-test
    contrib.datomic-test
    contrib.datomic-tx-test
    contrib.eval-test
    contrib.match-test
    contrib.reactive-test
    contrib.rfc3986-test
    contrib.string-test
    contrib.validation
    hypercrud.browser.field-test
    hypercrud.browser.nested-pull-test
    hyperfiddle.data-test
    hyperfiddle.fiddle-test
    hyperfiddle.foundation-test
    hyperfiddle.ide.console-links-test
    hyperfiddle.ide.fiddles.schema_test
    hyperfiddle.readers-test
    hyperfiddle.route-test
    ))

(defn run []
  (doo-tests
    'contrib.char$-test
    'contrib.data-test
    #_'contrib.datomic-errors-test
    'contrib.datomic-test
    'contrib.datomic-tx-test
    'contrib.eval-test
    'contrib.match-test
    'contrib.reactive-test
    'contrib.rfc3986-test
    'contrib.string-test
    'contrib.validation
    'hypercrud.browser.field-test
    'hypercrud.browser.nested-pull-test
    'hyperfiddle.data-test
    'hyperfiddle.fiddle-test
    'hyperfiddle.foundation-test
    'hyperfiddle.ide.console-links-test
    'hyperfiddle.ide.fiddles.schema_test
    'hyperfiddle.readers-test
    'hyperfiddle.route-test
    ))

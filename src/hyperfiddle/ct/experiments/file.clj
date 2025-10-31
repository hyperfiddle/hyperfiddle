(ns hyperfiddle.ct.experiments.file
  (:require
   [clojure.java.io]
   [hyperfiddle.ct.pair :refer [pair snd]]
   [hyperfiddle.ct.cofree :refer [->CofreeF cofree CofreeFunctor headF synthesize tailF tree-seqT unwrap]]
   [hyperfiddle.ct.comonad :refer [extract]]
   [hyperfiddle.ct.functor :refer [fmap]])
  (:import
   (java.io File)))

(extend-type java.io.File
  ;; This implementation teaches Cofree how to traverse directories.
  ;; java.io.File is not a Functor – we cannot fmap it – but it still forms a Cofree Functor, i.e. we can ask for thing at point + children.
  ;; This implementation may seem useless, but pay close attention to `(map cofree ...)` in tailF.
  CofreeFunctor ; CofreeF f a b = CofreeF List File (CofreeF List File) = [File ([File (...)] ...)] . This is different from [File (File...)] !
  (headF [this] this) ; current file - headF :: CofreeF f a b -> a
  (tailF [this] (lazy-seq (map cofree (.listFiles this)))) ; children – tailF :: Cofree f a b -> f b
  )

;; Given a root file, simulate a traversable file system.
(def filesystem-tree ":: File -> Cofree List File" cofree) ; cofree :: CofreeF f a (Cofree f a) -> Cofree f a

(def list-files (comp (fmap extract) tree-seqT)) ; fmap extract ∘ tree-seqT
;; tree-seqT is preorder traversal of the Cofree tree, defined as (defn tree-seqT [t] (cons t (lazy-seq (mapcat tree-seqT (unwrap t)))))
;; Note the similarity with `clojure.core/tree-seq`.

(comment
  (list-files (filesystem-tree (clojure.java.io/file ".")))
  :=
  ;; (#object[java.io.File 0x1720229a "."]
  ;;  #object[java.io.File 0x58887960 "./shadow-cljs.edn"]
  ;;  #object[java.io.File 0x4305502d "./src-build"]
  ;;  #object[java.io.File 0x5ae72089 "./src-build/README.md"]
  ;;  #object[java.io.File 0x4300f259 "./src-build/build.clj"]
  ;;  #object[java.io.File 0xb75b1e "./.clj-kondo"]
  ;;  #object[java.io.File 0x695af71d "./.clj-kondo/config.edn"]
  ;;  #object[java.io.File 0xada61d7 "./.clj-kondo/imports"]
  ;;  #object[java.io.File 0x4e0cd8ef "./.clj-kondo/imports/hyperfiddle"]
  ;;  #object[java.io.File 0x7459059b "./.clj-kondo/imports/hyperfiddle/electric"]
  ;;  ...)
  )

(defn prune-cofree-tree "Lazily filter a Cofree tree as a prewalk. I.e. do not visit pruned branches. 𝓞(1) thank to laziness."
  [pred? W] ; valid for any Cofree, not just over files.
  (let [v (extract W)]
    (when (pred? v)
      (cofree
        (->CofreeF
          v
          (keep #(prune-cofree-tree pred? %) (unwrap W)))))))

(comment
  "List all files in directory, excluding .clj-kondo"
  (->>
    (clojure.java.io/file ".")
    (filesystem-tree)
    (prune-cofree-tree #(not= ".clj-kondo" (.getName %)))
    (list-files))
  :=
  ;; (#object[java.io.File 0x710cfc73 "."]
  ;;  #object[java.io.File 0x34fdb3f6 "./shadow-cljs.edn"]
  ;;  #object[java.io.File 0x1668cdc4 "./src-build"]
  ;;  #object[java.io.File 0x12b54fcf "./src-build/README.md"]
  ;;  #object[java.io.File 0x4cbe3794 "./src-build/build.clj"]
  ;;  #object[java.io.File 0x10382d78 "./.cljfmt.edn"]
  ;;  #object[java.io.File 0x40eed456 "./Dockerfile"]
  ;;  #object[java.io.File 0x781d20b3 "./deps.edn"]
  ;;  #object[java.io.File 0x4522bab3 "./resources"]
  ;;  #object[java.io.File 0x14200c82 "./resources/public"]
  ;;  ...)
  )

(def count-recursive-children ; valid for any Cofree, not just over files.
  "Recursively count how may children does a Cofree node have. Eager due to the
  \"count\" reduction. Post walks the entire tree."
  ;; Could be made lazy by wrapping the eager reduction in a `delay`.
  (synthesize (fn [thing-at-point children] (pair thing-at-point (transduce (map snd) + 1 children)))))
;; pair :: a -> b -> (a, b)
;; snd :: (a, b) -> b
;; Not using clojure [] and `second` for good reasons. Off topic. Ask G if you're curious.

(comment
  "Count total files in directory and subdirs"
  (-> (clojure.java.io/file ".")
    (filesystem-tree)
    (count-recursive-children) ; 𝓞(1)
    (extract) ; Θ(n)
    )
  := ;; [#object[java.io.File 0x47246f13 "."] 1718] ; 1718 children
  )

(comment
  "Count total files in directory and subdirs, excluding .clj-kondo"
  (->> (clojure.java.io/file ".")
    (filesystem-tree)
    (prune-cofree-tree #(not= ".clj-kondo" (.getName %)))
    (count-recursive-children)
    (extract)
    )
  :=
  ;; [#object[java.io.File 0x3fd85e42 "."] 1126] ; less children without .clj-kondo dir
  )

(comment
  "List all files in directory with their respective count of children, recursively, excluding .clj-kondo"
  (->> (clojure.java.io/file ".")
    (filesystem-tree)
    (prune-cofree-tree #(not= ".clj-kondo" (.getName %)))
    (count-recursive-children)
    (list-files)
    )
  :=
  ;; ([#object[java.io.File 0x6f141d4a "."] 1126]
  ;;  [#object[java.io.File 0x59c7ecf "./shadow-cljs.edn"] 1]
  ;;  [#object[java.io.File 0x3ef48750 "./src-build"] 3]
  ;;  [#object[java.io.File 0x3078efa0 "./src-build/README.md"] 1]
  ;;  [#object[java.io.File 0x4b043ce7 "./src-build/build.clj"] 1]
  ;;  [#object[java.io.File 0x25636abf "./.cljfmt.edn"] 1]
  ;;  [#object[java.io.File 0x482e5ab3 "./Dockerfile"] 1]
  ;;  [#object[java.io.File 0x2b2a2eed "./deps.edn"] 1]
  ;;  [#object[java.io.File 0x2542861b "./resources"] 617]
  ;;  [#object[java.io.File 0x4274a5af "./resources/public"] 616]
  ;;  [#object[java.io.File 0x7029b76a "./resources/public/hyperfiddle-starter-app"] 615]
  ;;  [#object[java.io.File 0x4beffb1c "./resources/public/hyperfiddle-starter-app/js"] 611]
  ;;  [#object[java.io.File 0x758f0e4 "./resources/public/hyperfiddle-starter-app/js/manifest.edn"] 1]
  ;;  [#object[java.io.File 0x1ae86784 "./resources/public/hyperfiddle-starter-app/js/main.js"] 1]
  ;;  [#object[java.io.File 0x735de67e "./resources/public/hyperfiddle-starter-app/js/cljs-runtime"] 608]
  ;;  [#object[java.io.File 0x3579010d "./resources/public/hyperfiddle-starter-app/js/cljs-runtime/hyperfiddle.router5.js.map"] 1]
  ;;  [#object[java.io.File 0x5016c7 "./resources/public/hyperfiddle-starter-app/js/cljs-runtime/goog.string.internal.js"] 1]
  ;;  [#object[java.io.File 0x5629dd06 "./resources/public/hyperfiddle-starter-app/js/cljs-runtime/goog.dom.tags.js"] 1]
  ;;  [#object[java.io.File 0x47b53027 "./resources/public/hyperfiddle-starter-app/js/cljs-runtime/clojure.datafy.js.map"] 1]
  ;;  [#object[java.io.File 0x5c7c9102 "./resources/public/hyperfiddle-starter-app/js/cljs-runtime/cloroutine.impl.js.map"] 1]
  ;;  ...)
  )
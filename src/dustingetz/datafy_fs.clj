(ns dustingetz.datafy-fs
  "nav implementation for java file system traversals"
  (:require [clojure.core.protocols :refer [nav Datafiable Navigable]]
            [clojure.spec.alpha :as s]
            [contrib.assert :refer [check]]
            [hyperfiddle.nav0 :refer [Identifiable NavContext nav-context]]
            [hyperfiddle.rcf :refer [tests]]
            [clojure.java.io :as io]
            [hyperfiddle.hfql0 :as hfql]
            [clojure.core.protocols :as ccp])
  (:import [java.nio.file Path Paths Files]
           java.io.File
           java.nio.file.LinkOption
           [java.nio.file.attribute BasicFileAttributes FileTime]
           [org.apache.tika Tika] ; mime type detection
           ))

; spec the data, not the object
(s/def ::name string?)
(s/def ::hidden boolean?)
(s/def ::absolute-path string?)
(s/def ::modified inst?)
(s/def ::created inst?)
(s/def ::accessed inst?)
(s/def ::size string?)
;; (s/def ::kind (s/nilable qualified-keyword?))
(s/def ::kind qualified-keyword?) ;; HACK FIXME implement nilable in hyperfiddle.spec
(s/def ::file (s/keys :opt [::name ::absolute-path ::modified ::created ::accessed ::size ::kind]))
(s/def ::children (s/coll-of ::file))

(defn get-extension [?path]
  (when ?path
    (when-not (= \. (first ?path)) ; hidden
      (some-> (last (re-find #"(\.[a-zA-Z0-9]+)$" ?path))
              (subs 1)))))

(tests
  "get-extension"
  (get-extension nil) := nil
  (get-extension "") := nil
  (get-extension ".") := nil
  (get-extension "..") := nil
  (get-extension "image") := nil
  (get-extension "image.") := nil
  (get-extension "image..") := nil
  (get-extension "image.png") := "png"
  (get-extension "image.blah.png") := "png"
  (get-extension "image.blah..png") := "png"
  (get-extension ".gitignore") := nil)

(comment
  "java.io.File interop"
  (def h (clojure.java.io/file "src"))

  (sort (.listFiles h))

  (.getName h) := "src"
  (.getPath h) := "src"
  (.isDirectory h) := true
  (.isFile h) := false
  ;(.getParent h) := nil -- ??
  ;(.getParentFile h) := nil -- ??
  (-> (datafy java.io.File) :members keys)
  (->> (seq (.listFiles h)) (take 1) first datafy)
  (for [x (take 5 (.listFiles h))] (.getName x)))

(defn file-path "get java.nio.file.Path of j.n.f.File"
  [^File f] (-> f .getAbsolutePath (java.nio.file.Paths/get (make-array String 0))))

(tests
  (def p (file-path (clojure.java.io/file "src")))
  (instance? Path p) := true
  (-> (datafy Path) :members keys)
  (-> p .getRoot str) := "/"
  (-> p .getFileName str) := "src"
  (-> p .getParent .getFileName str) := "electric-fiddle"
  (-> p .getParent .toFile .getName) := "electric-fiddle"
  #_(-> p .getParent .toFile datafy))

(defn path-attrs [^Path p]
  (Files/readAttributes p BasicFileAttributes (make-array java.nio.file.LinkOption 0)))

(tests
  (def attrs (path-attrs (file-path (clojure.java.io/file "src"))))
  (instance? BasicFileAttributes attrs) := true
  (.isDirectory attrs) := true
  (.isSymbolicLink attrs) := false
  (.isRegularFile attrs) := false
  (.isOther attrs) := false)

(defn file-attrs [^File f] (path-attrs (file-path f)))

(tests
  (file-attrs (clojure.java.io/file "src"))
  )

(def ... `...) ; define a value for easy test assertions

(extend-type java.nio.file.attribute.FileTime
  Identifiable (-identify [o] (-> o .toInstant java.util.Date/from)) ; value semantics ; why is this implemented at all? If so shouldn't it just call datafy on itself?
  Datafiable (datafy [o] (-> o .toInstant java.util.Date/from)))

(defonce TIKA (org.apache.tika.Tika.))

(defn detect-mime-type [^File file] (.detect TIKA file))
(defn detect-mime-type-no-access [^String file-name] (.detect TIKA file-name))

(defmulti datafy-file-content detect-mime-type)

(defmethod datafy-file-content "text/plain" [^File f] (line-seq (io/reader f)))
(defmethod datafy-file-content :default [^File f] (io/reader f))

(defn file-hidden? [^File x] (= "." (subs (.getName x) 0 1)))
(defn file-absolute-path [^File x] (-> x .toPath .normalize .toAbsolutePath str))
(defn file-order-compare [^File x] [(not (.isDirectory x)) (.getName x)])
(defn dir-list [^File x] (some->> x .listFiles (sort-by file-order-compare) vec))
(defn dir-parent [^File x] (some-> x file-path .getParent .toFile))
(defn file-name [^File x] (.getName x))
(defn file-kind [^File x]
  (let [attrs (file-attrs x)]
    (cond (.isDirectory attrs) ::dir
          (.isSymbolicLink attrs) ::symlink
          (.isOther attrs) ::other
          (.isRegularFile attrs) (if-let [s (get-extension (.getName x))]
                                   (keyword (namespace ::foo) s)
                                   ::unknown-kind)
          () ::unknown-kind)))
(defn file-created [^File x] (-> x file-attrs .creationTime .toInstant java.util.Date/from))
(defn file-accessed [^File x] (let [attrs (file-attrs x)] (-> attrs .lastAccessTime .toInstant java.util.Date/from)))
(defn file-modified [^File x] (let [attrs (file-attrs x)] (-> attrs .lastModifiedTime .toInstant java.util.Date/from)))
(defn file-size [^File x] (.size (file-attrs x)))
(defn file-mime-type [^File x] (detect-mime-type-no-access (.getName x)))

(extend-type java.io.File
  Identifiable (-identify [^File x] (.getName x)) ; locally unique in its folder ; if this is LOCALLY unique, is it a unique identity? In not, should nav only be defined in the local folder?
  NavContext
  (-nav-context [^File f] {`ccp/nav (fn [^File _ k v] (nav f k v))})
  Navigable
  (nav [^File f k v]
    (let [attrs (file-attrs f)]
      (case k
        ;; reverse data back to object, to be datafied again by caller
        ::modified (.lastModifiedTime attrs)
        ::created (.creationTime attrs)
        ::accessed (.lastAccessTime attrs)
        ::children (vec (dir-list f))
        ::content (datafy-file-content f)
        v)))
  Datafiable
  (datafy [^File f]
    ; represent object's top layer as EDN-ready value records, for display
    ; datafy is partial display view of an object as value records
    ; nav is ability to resolve back to the underlying object pointers
    ; they compose to navigate display views of objects like a link
    (when (.exists f)
      (let [attrs (file-attrs f)
            n (.getName f)
            mime-type (detect-mime-type-no-access n)]
        (as-> {::name n
               ::hidden (file-hidden? f)
               ::kind (file-kind f)
               ::absolute-path (file-absolute-path f)
               ::created (file-created f)
               ::accessed (-> attrs .lastAccessTime .toInstant java.util.Date/from)
               ::modified (-> attrs .lastModifiedTime .toInstant java.util.Date/from)
               ::size (.size attrs)
               ::mime-type mime-type} %
          (merge % (if (= ::dir (::kind %))
                     {::children #() ; fns are hyperlinks - experiment - use nav to call it - should probably be elided entirely
                      ::parent (dir-parent f)}))
          (with-meta % (nav-context f))))))
  hfql/Suggestable
  (-suggest [_]
    [{:label 'name, :entry '.getName}
     #_{:label 'hidden, :entry `file-hidden?}
     {:label 'kind, :entry {`file-kind `name}}
     #_{:label 'absolute-path, :entry `file-absolute-path}
     #_{:label 'created, :entry `file-created}
     #_{:label 'accessed, :entry `file-accessed}
     {:label 'modified, :entry `file-modified}
     {:label 'size, :entry `file-size}
     {:label 'mime-type, :entry `file-mime-type}
     {:label 'children, :entry `dir-list}
     {:label 'parent, :entry `dir-parent}])
)

(tests
  (require '[clojure.datafy :refer [datafy]])
  ; careful, calling seq loses metas on the underlying
  (def h (clojure.java.io/file "src"))
  (type h) := java.io.File
  "(datafy file) returns an EDN-ready data view that is one layer deep"
  (datafy h)
  := #:dustingetz.datafy-fs
        {:name "src",
         :absolute-path _,
         :mime-type _,
         :hidden _,
         :size _,
         :modified _,
         :created _,
         :accessed _,
         :kind ::dir,
         :children _
         :parent _})

(tests
  "datafy of a directory includes a Clojure coll of children, but child elements are native file
  objects"
  (as-> (datafy h) %
        (nav % ::children ((::children %)))
        (datafy %)
        (take 2 (map type %)))
  := [java.io.File java.io.File]

  "nav to a leaf returns the native object"
  (as-> (datafy h) %
        (nav % ::modified (::modified %)))
  (type *1) := java.nio.file.attribute.FileTime

  "datafy again to get the plain value"
  (type (datafy *2)) := java.util.Date)

(tests
  (as-> (datafy h) %
        (nav % ::children (::children %)) ; fn - weird
        (datafy %) ; can skip - simple data
        (map datafy %)
        (vec (filter #(= (::name %) "contrib") %)) ; stabilize test
        (nav % 0 (% 0))
        (datafy %)
        #_(s/conform ::file %)
        (select-keys % [::name ::kind ::children]))
  := {::name "contrib",
      ::kind ::dir,
      ::children _})

(tests
  "nav into children and back up via parent ref"
  (def m (datafy h))
  (::name m) := "src"
  (as-> m %
    (nav % ::children (::children %))
    (datafy %) ; dir
    (nav % 1 (get % 1)) ; first file in dir (skip .DS_Store it is not a dir!)
    (datafy %)
    (nav % ::parent (::parent %)) ; dir (skip level on way up)
    (datafy %)
    (::name %))
  := "src")

(defn absolute-path [^String path-str & more]
  (-> (java.nio.file.Path/of ^String path-str (into-array String more))
    .normalize .toAbsolutePath str))

(comment
  (absolute-path "./") := "/Users/dustin/src/hf/electric-fiddle"
  (absolute-path "node_modules") := "/Users/dustin/src/hf/electric-fiddle/node_modules"
  (clojure.java.io/file (absolute-path "./"))
  (clojure.java.io/file (absolute-path "node_modules")))

(defn relativize-path "Convert an absolute path to one relative to base-dir"
  [base-dir abs-path]
  (let [base (.toPath (clojure.java.io/file (check base-dir)))
        full (.toPath (clojure.java.io/file (check abs-path)))]
    (when (-> full .normalize (.startsWith (.normalize base)))
      (str (.relativize (.normalize base) (.normalize full))))))

(tests
  (relativize-path (absolute-path "./") (absolute-path "./vendor/electric/src")) := "vendor/electric/src"
  (relativize-path (absolute-path "./") (absolute-path "./vendor/electric/src/")) := "vendor/electric/src"
  (relativize-path (absolute-path "./") (absolute-path "vendor/electric/src")) := "vendor/electric/src"
  (relativize-path (absolute-path "./") (absolute-path "vendor/electric/src/")) := "vendor/electric/src"
  (relativize-path (absolute-path "../") (absolute-path "vendor/electric/src")) := "electric-fiddle/vendor/electric/src"
  (relativize-path (absolute-path "./") (absolute-path "./")) := ""
  (relativize-path (absolute-path "./") (absolute-path "")) := ""
  (relativize-path (absolute-path "./") (absolute-path "../")) := nil

  (relativize-path "/fake/" (absolute-path "./src")) := nil
  (relativize-path (absolute-path "./") "/fake/") := nil
  (relativize-path (absolute-path "./") "fake") := nil)

(ns dustingetz.datafy-git2
  (:require [clj-jgit.porcelain :as git]
            clj-jgit.util
            [clojure.core.protocols :refer [nav Datafiable Navigable]]
            [dustingetz.datafy-fs :as fs]
            [hyperfiddle.hfql0 :as hfql :refer [Identifiable pull-spec]])
  (:import (org.eclipse.jgit.api Git)
           (org.eclipse.jgit.internal.storage.file FileRepository)
           (org.eclipse.jgit.revwalk RevCommit)
           (org.eclipse.jgit.lib Constants ObjectId ObjectIdRef Ref Repository PersonIdent)))

; re-export wrappers for convenience - one API not two
(def load-repo (memoize git/load-repo))
#_(def log git/git-log)
#_(def branch-list git/git-branch-list)

(defn short-commit-id [id] (apply str (take 7 id)))

#_(defn remote-ref? [^Ref ref] (.startsWith (.getName ref) Constants/R_REMOTES))
#_(defn local-ref? [^Ref ref] (.startsWith (.getName ref) Constants/R_HEADS))
(defn ref-type [^Ref ref] (if (.startsWith (.getName ref) "refs/remotes/") :remote :local))
(defn repo-id [^Git x] (-> x .getRepository .getIdentifier))
(defn repo-path [^Git x] (-> x .getRepository .getDirectory fs/file-absolute-path))
(defn repo-repo [^Git x] (-> x .getRepository))
(defn status [^Git x] (git/git-status x))
(def branch-current (memoize (fn [^Git x] (git/git-branch-current x))))
(def branch-list (memoize (fn [^Git x] (vec (git/git-branch-list x :jgit? true :list-mode :all))))) ; arraylist
(defn log [^Git x] (vec (git/git-log x :until "HEAD" :jgit? true)))

(defn commit-name [^RevCommit o] (.getName o))
(defn commit-short-name [^RevCommit o] (-> o .getName short-commit-id))
(defn commit-message [^RevCommit o] (.getShortMessage o))
(defn commit-author-ident [^RevCommit o] (.getAuthorIdent o))
(defn commit-committer-ident [^RevCommit o] (.getCommitterIdent o))

(comment
  (->> (load-repo "./") repo-path (fs/relativize-path (fs/absolute-path "./"))) := ".git")

(extend-protocol Datafiable
  Git
  (datafy [^Git o]
    (->
      {:status (git/git-status o)
       :repo (.getRepository o)
       :branch-current (git/git-branch-current o)
       ; return jgit objects for user to datafy
       :branches (partial branch-list o)
       :log (partial log o)}
      (with-meta {`nav
                  (fn [xs k v]
                    (case k
                      (:branches :log) (if v (v))
                      v))})))

  FileRepository
  (datafy [^FileRepository o]
    {:dir (.getDirectory o)})

  Ref #_ObjectIdRef ; including ObjectIdRef$PeeledNonTag
  (datafy [^ObjectIdRef o]
    {#_#_:ref-name (.getName o) ; refs/remotes/origin/agent-network, refs/heads/wip/hot-md
     :ref-name-short (Repository/shortenRefName (.getName o))
     :commit (-> o .getObjectId .getName) ; "88a97ef20b1fe5392a6025eca4170c80ad3479ce"
     :commit-short (-> o .getObjectId .getName short-commit-id)
     #_#_:symbolic (-> o .isSymbolic)
     #_#_:peeled (-> o .isPeeled)
     :ref-type (-> o ref-type) ; #{:local :remote}
     :object-id (.getObjectId o)}) ; #object[org.eclipse.jgit.lib.ObjectId

  ObjectId
  (datafy [^ObjectId o]
    {:toString (.toString o)
     :name (.getName o)})

  RevCommit
  (datafy [^RevCommit o]
    {:name (commit-name o)
     :short-name (commit-short-name o)
     :msg (commit-message o)
     :author (commit-author-ident o)
     :committer (commit-committer-ident o)})

  PersonIdent
  (datafy [^PersonIdent x] (clj-jgit.util/person-ident x)))

(comment
  (require '[clojure.datafy :refer [datafy]])
  (as-> (load-repo "./") x
    (datafy x)
    (nav x :log (:log x)))

  (as-> (load-repo "./") x
    (datafy x)
    (nav x :branches (:branches x))
    (datafy x)
    (nav x 0 (nth x 0))
    (datafy x)))

(extend-protocol Identifiable
  Git (-identify [^Git x] (repo-id x))
  FileRepository (-identify [^FileRepository x] (.getIdentifier x))
  Ref (-identify [^Ref x] (Repository/shortenRefName (.getName x)))
  ObjectId (-identify [^ObjectId x] (.getName x))
  RevCommit (-identify [^RevCommit x] (commit-short-name x))
  PersonIdent (-identify [^PersonIdent x] (.getEmailAddress x)))

(comment
  (require '[hyperfiddle.hfql0 :refer [identify]])
  (def x (load-repo "./"))
  (identify x) := "./.git"

  (as-> (datafy x) x
    (datafy x) (nav x :repo (:repo x))
    (identify x)) := "./.git"

  (as-> (datafy x) x
    (datafy x) (nav x :repo (:repo x))
    (identify x)) := "./.git"

  (as-> (datafy x) x
    (datafy x) (nav x :log (:log x))
    (datafy x) (nav x 0 (nth x 0)) ; log record
    (identify x)) := "b10bf19"

  (as-> (datafy x) x
    (datafy x) (nav x :log (:log x))
    (datafy x) (nav x 0 (nth x 0)) ; log record
    (datafy x) (nav x :id (:id x))
    (identify x)) := "47f2ce3"

  (as-> (datafy x) x
    (datafy x) (nav x :branches (:branches x))
    (datafy x) (nav x 0 (nth x 0)) ; log record
    (identify x)) := "agent-network"

  (as-> (datafy x) x
    (datafy x) (nav x :branches (:branches x))
    (datafy x) (nav x 0 (nth x 0)) ; log record
    (datafy x) (nav x :object-id (:object-id x))
    (identify x)) := "12b7acf4d68519b8fa98a31828b5f725abaf80e0")

(defn ref-short-name [^Ref o] (Repository/shortenRefName (.getName o)))
(defn ref-commit [^Ref o] (-> o .getObjectId .getName))
(defn ref-commit-short [^Ref o] (-> o .getObjectId .getName short-commit-id))

(extend-protocol hfql/Suggestable
  Git
  (-suggest [_]
    (pull-spec [.getRepository
                git/git-status
                git/git-branch-current
                branch-list
                log]))
  RevCommit
  (-suggest [_]
    (pull-spec [#_.getName
                commit-short-name
                .getShortMessage
                {.getAuthorIdent .getName}
                #_{.getCommitterIdent .getName}]))
  PersonIdent
  (-suggest [_]
    (pull-spec [.getName
                .getEmailAddress
                .getWhen
                .getTimeZone]))
  Ref
  (-suggest [_]
    (pull-spec [ref-short-name
                #_ref-commit
                ref-commit-short
                ref-type
                #_.getObjectId]))
  ObjectId
  (-suggest [_]
    (pull-spec [str .getName]))
  )

(ns hyperfiddle.hfql-repl
  "REPL helpers for exploring HFQL sitemaps. CLJ-only, server-side tooling.
  Usage:
    (require '[hyperfiddle.hfql-repl :as r])
    (r/set-ctx! {:sitemap (dustingetz.nav-demo-agents/load-all-sitemaps)})
    (r/routes)
    (r/describe 'ns/listed-agents)
    (r/pull-route 'ns/listed-agents)
    (r/search-route 'ns/listed-agents \"dustin\")"
  (:require [hyperfiddle.hfql2 :as hfql]
            [hyperfiddle.hfql2.analyzer :as ana]
            [hyperfiddle.hfql2.utils :refer [original-object]]
            [hyperfiddle.ct.comonad :refer [extract]]
            [hyperfiddle.ct.cofree :refer [unwrap]]
            [hyperfiddle.ct.either :refer [either from-right]]
            [hyperfiddle.ct.experiments.thunk :refer [force-thunk]]
            [hyperfiddle.navigator6.selection2 :as selection2]
            [clojure.string :as str]))

(def ^:private COLLECTION-LIMIT 10000)
(def ^:private DEFAULT-LIMIT 50)

(defonce !ctx (atom {}))

(defn set-ctx!
  "Set the REPL context. Keys:
    :sitemap  — merged sitemap map (symbol -> hfqlT)
    :bindings — optional map of vars to values, e.g. {#'*db* (d/db conn)}"
  [ctx]
  (reset! !ctx ctx)
  (let [n (count (:sitemap ctx))]
    (str "HFQL REPL context set. " n " routes available.")))

(defn- current-sitemap [] (:sitemap @!ctx))
(defn- current-bindings [] (:bindings @!ctx {}))

;; Discovery

(defn routes
  "List all route symbols from the current sitemap."
  []
  (sort (keys (current-sitemap))))

(defn describe
  "Describe a route: its type, whether it's partial, and its columns."
  [route-sym]
  (when-let [stepT (get (current-sitemap) route-sym)]
    (let [m (hfql/metas stepT)
          type (::ana/type m)
          edges (::ana/symbolic-edges m)
          partial? (::ana/partial m)]
      {:name route-sym
       :type (case type
               ::ana/traversal "traversal"
               ::ana/pull "pull"
               ::ana/call "call"
               (str type))
       :partial? (boolean partial?)
       :columns (or edges
                    ;; For traversals, look at the edge's columns
                    (::ana/symbolic-edges (::ana/edge m)))
       :form (::ana/form m)})))

;; Data access

(defn- collection-point? [point]
  (hfql/multiplicity-many? point))

(defn- resolve-and-pull
  "Core pull logic: resolve a route, optionally seed an arg, force the query, paginate."
  [sitemap route-sym arg offset limit]
  (let [stepT (get sitemap route-sym)]
    (when stepT
      (let [stepT (if arg
                    (hfql/seed (assoc (hfql/scope stepT) '% (hfql/hfql-resolve* arg)) stepT)
                    stepT)]
        ;; Force query
        (hfql/nav stepT)
        (let [coll-step (selection2/find-collection-point stepT)]
          (if (collection-point? coll-step)
            ;; Collection: count, window, pull each row
            (let [rowsT (unwrap coll-step)
                  rowsT (vec (take COLLECTION-LIMIT rowsT))
                  total (count rowsT)
                  page  (->> rowsT
                             (drop offset)
                             (take limit)
                             (mapv #(force-thunk (extract (hfql/pull* %)))))]
              {:data page :count total :offset offset :limit limit})
            ;; Single entity
            {:data (force-thunk (extract (hfql/pull* coll-step))) :count 1}))))))

(defn pull-route
  "Pull data from a sitemap route.
  For collections, returns paginated results.
  For entities, returns the entity directly.

  Usage:
    (pull-route 'listed-agents)                      ;; first 50 rows
    (pull-route 'listed-agents :offset 50 :limit 25) ;; page 2
    (pull-route 'find-agent \"dustin\")                ;; entity by arg"
  [route-sym & {:keys [offset limit arg]
                :or {offset 0 limit DEFAULT-LIMIT}}]
  (let [sm (current-sitemap)
        bindings (current-bindings)]
    (when-not (get sm route-sym)
      (throw (ex-info (str "Route not found: " route-sym
                           ". Available routes: " (str/join ", " (routes)))
                      {:route route-sym})))
    (with-bindings bindings
      (resolve-and-pull sm route-sym arg offset limit))))

;; Search

(defn- row-matches? [rowT columnTs needle]
  (let [rowT+ (hfql/augment-with-suggestions! rowT)
        cellTs (unwrap rowT+)]
    (loop [cols columnTs]
      (when (seq cols)
        (if-let [cellT (hfql/find-cell-by-column* (first cols) cellTs)]
          (let [either-value (hfql/nav* cellT)]
            (if (either
                  (fn [err] (str/includes? (str err) needle))
                  (fn [val] (str/includes? (str (original-object val)) needle))
                  either-value)
              true
              (recur (rest cols))))
          (recur (rest cols)))))))

(defn search-route
  "Search a collection route for rows matching needle (case-insensitive substring match).
  Returns matching rows, paginated.

  Usage:
    (search-route 'listed-agents \"dustin\")
    (search-route 'listed-agents \"dustin\" :limit 10)"
  [route-sym needle & {:keys [offset limit]
                       :or {offset 0 limit DEFAULT-LIMIT}}]
  (let [sm (current-sitemap)
        bindings (current-bindings)]
    (when-not (get sm route-sym)
      (throw (ex-info (str "Route not found: " route-sym) {:route route-sym})))
    (with-bindings bindings
      (let [stepT (get sm route-sym)]
        ;; Force query
        (hfql/nav stepT)
        (let [coll-step (selection2/find-collection-point stepT)]
          (if (collection-point? coll-step)
            (let [rowsT (vec (take COLLECTION-LIMIT (unwrap coll-step)))
                  columnTs (unwrap (hfql/augment-with-suggestions! (first rowsT)))
                  needle-lc (str/lower-case (str needle))
                  matching (->> rowsT
                                (filter #(row-matches? % columnTs needle-lc))
                                vec)
                  total (count matching)
                  page (->> matching
                            (drop offset)
                            (take limit)
                            (mapv #(force-thunk (extract (hfql/pull* %)))))]
              {:data page :count total :offset offset :limit limit :needle needle})
            {:data (force-thunk (extract (hfql/pull* coll-step))) :count 1 :needle needle}))))))

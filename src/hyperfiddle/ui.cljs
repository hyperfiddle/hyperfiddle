(ns hyperfiddle.ui
  (:require
    [contrib.data :refer [unwrap kwargs]]
    [contrib.string :refer [memoized-safe-read-edn-string]]
    [contrib.ui.remark :as remark]
    [contrib.reactive :as r]
    [contrib.reagent :refer [from-react-context fragment]]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.core :as browser]
    [hypercrud.browser.link :as link]
    [hypercrud.ui.auto-control :refer [auto-control]]
    [hypercrud.ui.form :as form]
    [hypercrud.ui.table :as table]
    [hyperfiddle.data :as hf]
    [hyperfiddle.ui.markdown-extensions :refer [extensions]]
    [hyperfiddle.ui.hacks]                                  ; exports
    ))


(defn ^:export value [[i a] ctx ?f & args]
  (let [ctx (context/focus ctx true i a)]
    [(or ?f (auto-control ctx)) @(:value ctx) ctx (kwargs args)]))

(defn ^:export field [[i a] ctx ?f & args]
  (let [field (case (::layout ctx) :hyperfiddle.ui.layout/table table/Field form/Field)]
    ^{:key (str i a)}
    [(r/partial field ?f)                                   ; Intentional explicit nil
     (context/focus ctx true i a)
     (kwargs args)]))

(defn ^:export table "sort-fn :: (fn [col ctx v] v)" [form sort-fn ctx]
  (let [sort-col (r/atom nil)]
    (fn [form sort-fn ctx]
      (let [ctx (assoc ctx ::layout (::layout ctx :hyperfiddle.ui.layout/table)
                           ::table/sort-col sort-col
                           :hyperfiddle.ui.markdown-extensions/unp true)]
        [:table.ui-table.unp
         (->> (form ctx) (into [:thead]))                   ; strict
         (->> (:relations ctx)
              (r/fmap (r/partial sort-fn sort-col ctx))
              (r/unsequence hf/relation-keyfn)
              (map (fn [[relation k]]
                     (->> (form (context/relation ctx relation))
                          (into ^{:key k} [:tr]))))         ; strict
              (into [:tbody]))]))))

(defn ^:export result "Default result renderer. Invoked as fn, returns seq-hiccup, hiccup or
nil. call site must wrap with a Reagent component"
  [ctx & [?f]]
  (cond
    ?f [?f ctx]
    (:relations ctx) [table (r/partial hf/form field) hf/sort-fn ctx]
    (:relation ctx) (hf/form field ctx)))

(defn browse [rel path ctx ?f & args]
  (let [props (kwargs args)
        {:keys [:link/dependent? :link/path] :as link} @(r/track link/rel->link rel path ctx)
        ctx (-> (apply context/focus ctx dependent? (unwrap (memoized-safe-read-edn-string (str "[" path "]"))))
                (as-> ctx (if ?f (assoc ctx :user-renderer ?f #_(if ?f #(apply ?f %1 %2 %3 %4 args))) ctx)))]
    (into [browser/ui link ctx (:class props)] (apply concat (dissoc props :class :children nil)))))

(defn link [rel path ctx ?label & args]
  (let [{:keys [:link/dependent? :link/path] :as link} @(r/track link/rel->link rel path ctx)
        ctx (apply context/focus ctx dependent? (unwrap (memoized-safe-read-edn-string (str "[" path "]"))))
        props (kwargs args)]
    [(:navigate-cmp ctx) (merge props (link/build-link-props link ctx)) (or ?label (name rel)) (:class props)]))

(defn ui-bindings [ctx]                                     ; legacy
  (assoc ctx
    :anchor link                                            ; legacy
    :browse browse
    :field field
    :cell field                                             ; legacy
    ::value value
    :browse' hf/browse'))

(def -remark-instance (remark/remark
                        (reduce-kv (fn [acc k v]
                                     (assoc acc k (remark/extension k v)))
                                   (empty extensions)
                                   extensions)))

(defn ^:export markdown [& args]
  (into [remark/markdown -remark-instance] args))

(def ^:export img
  (from-react-context
    (fn [{:keys [ctx props]} value]
      [:img (merge props {:src value})])))

(ns dustingetz.entity-browser1
  (:require [clojure.core.protocols :refer [nav datafy]]
            [contrib.data :refer [unqualify]]
            [dustingetz.easy-table :refer [Load-css]]
            [dustingetz.treelister2 :refer [explorer-seq]]
            [electric-fiddle.fiddle-index :refer [pages NotFoundPage]]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric3-contrib :as ex]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.electric-forms3 :as forms :refer [TablePicker!]]
            [hyperfiddle.rcf :refer [tests]]
            [hyperfiddle.router4 :as r]))

(e/defn TreeRow [[path value branch?]]
  (e/client
    (let [k (peek path)]
      (dom/td (dom/props {:style {:padding-left (some-> path count dec (* 15) (str "px"))}})
        (dom/span (dom/props {:class "dustingetz-tooltip"}) ; hover anchor
          (dom/text (if (keyword? k) ; glitch
                      (unqualify k) (str k)))
          (dom/span (dom/text (pr-str k)))))) ; tooltip
    (dom/td
      (let [v-str (e/server (pr-str value))]
        (if (e/server (fn? value)) ; unused - datafy coupling
          (dom/text "...")
          (dom/text (#(when-not branch? %) v-str)))))))

(e/defn Intercept [Control ?v v->x x->v]
  (e/client
    (if-let [[t [k x]] (Control (e/server (v->x ?v)))]
      [t [k (e/server (x->v x))]]
      (e/amb))))

(e/defn TreePicker! [field ?sel !query & {:keys [cols]}]
  (dom/fieldset (dom/legend #_(dom/text `User))
    (dom/props {:class "entity"}))
  (let [xs! (e/server (ex/Offload-latch #(drop 1 (explorer-seq (!query)))))
        cols (e/server (e/diff-by identity cols))]
    (Intercept
      (e/fn Control [?i]
        (TablePicker! field (doto ?i prn) ; hack crash
          (e/server (count xs!))
          (e/fn [i] (e/server (when-some [x (ex/Offload-latch #(datafy (nav xs! i (nth xs! i nil))))]
                                (TreeRow x))))
          :edit-monoid vector))
      ?sel
      (e/server (fn v->x [?v] (when ?v (first (->> xs! (keep-indexed (fn [i {:keys [path name]}]
                                                                       (when (= ?v (conj path name)) i))))))))
      (e/server (fn x->v [?i] (if-some [{:keys [path name value]} (nth xs! ?i nil)]
                                (conj path name)))))))

(e/defn EntityPicker! [field ?sel !query & {:keys [cols]}]
  (dom/fieldset (dom/legend #_(dom/text `Waitlist))
    (dom/props {:class "entity-children"})
    (dom/props {:style {:--col-count (count cols)}})
    (let [xs! (e/server (ex/Offload-latch !query))
          cols (e/server (e/diff-by identity cols))]
      (Intercept
        (e/fn Control [?i]
          (TablePicker! field (doto ?i prn) ; the prn prevents a crash
            (e/server (count xs!))
            (e/fn Row [i]
              (e/server (when-some [x (ex/Offload-latch #(datafy (nav xs! i (nth xs! i nil))))]
                          (e/for [k cols]
                            (dom/td (some-> x k pr-str dom/text))))))
            :edit-monoid vector))
        ?sel
        (e/server (fn v->x [?v] (when ?v (first (keep-indexed (fn [i x] (when (= ?v x) i)) xs!)))))
        (e/server (fn x->v [?i] (get xs! ?i nil)))))))

(e/declare *hfql-spec)

(declare css)
(e/defn HfqlRoot
  [sitemap
   & {:keys [default]
      :or {default nil}}]
  (e/client
    #_(dom/pre (dom/text (pr-str r/route)))
    (dom/style (dom/text css)) (Load-css "dustingetz/easy_table.css")
    (dom/div (dom/props {:class (str "Browser dustingetz-EasyTable")})
      (e/for [route (e/diff-by identity (e/as-vec r/route))] ; reboot entire page
        (binding [r/route route]
          (let [[fiddle & _] r/route]
            (if-not fiddle
              (r/ReplaceState! ['. default])
              (let [Fiddle (get pages fiddle NotFoundPage)]
                (set! (.-title js/document) (str (some-> fiddle name (str " – ")) "Hyperfiddle"))
                (r/pop
                  (binding [*hfql-spec (e/server (get sitemap fiddle []))] ; cols don't serialize perfectly yet fixme
                    (r/Apply Fiddle r/route)))))))))))

(def css "
.Browser.dustingetz-EasyTable { position: relative; } /* re-hack easy-table.css hack */
.Browser fieldset { position: relative; height: 25em; }
:where(.Browser fieldset.entity)          table { grid-template-columns: 15em auto; }
.Browser fieldset.entity-children table { grid-template-columns: repeat(var(--col-count), 1fr); }

/* table cell tooltips */
.Browser td {position: relative;}
.Browser .dustingetz-tooltip >       span { visibility: hidden; }
.Browser .dustingetz-tooltip:hover > span { visibility: visible; pointer-events: none; }
.Browser .dustingetz-tooltip > span {
  position: absolute; top: 20px; left: 10px; z-index: 2; /* interaction with row selection z=1 */
  margin-top: 4px; padding: 4px; font-size: smaller;
  box-shadow: 0 0 .5rem gray; border: 1px whitesmoke solid; border-radius: 3px; background-color: white; }")
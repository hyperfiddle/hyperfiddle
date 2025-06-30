(ns electric-fiddle.fiddle-index ; merge into hyperfiddle.entrypoint
  (:require [contrib.data :refer [subgroup-by]]
            [dustingetz.str :refer [includes-str?]]
            [dustingetz.treelister1 :refer [treelister]] ; todo upgrade to treelister3
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.electric-forms5 :refer [Input TablePicker*]]
            [hyperfiddle.router4 :as r]
            [hyperfiddle.rcf :refer [tests]]))

(defn ns-segments [ns-name-str] (clojure.string/split ns-name-str #"\."))

(tests
  (ns-segments (namespace 'a.b.c/d)) := ["a" "b" "c"]
  (ns-segments "tutorial") := ["tutorial"])

(defn fiddles-by-ns-segments [fiddle-index] #_(def q fiddle-index)
  (subgroup-by (fn [[sym F]]
                 (if-some [s (namespace sym)]
                   (ns-segments s)
                   [(str sym)])) ; edge case - human routes
    fiddle-index))

(tests
  (fiddles-by-ns-segments
    {'electric-tutorial.inputs-local/DemoInputNaive nil,
     'dustingetz.million-checkboxes2/MillionCheckboxes2 nil,
     'dustingetz.london-talk-2024.webview-scroll-dynamic/WebviewScrollDynamic nil,
     'datomic-browser.datomic-browser/DatomicBrowser nil,
     'electric-tutorial.form-service/FormsService nil})
  := {"electric-tutorial" {"inputs-local" [['electric-tutorial.inputs-local/DemoInputNaive nil]],
                           "form-service" [['electric-tutorial.form-service/FormsService nil]]},
      "dustingetz" {"million-checkboxes2" [['dustingetz.million-checkboxes2/MillionCheckboxes2 nil]],
                    "london-talk-2024" {"webview-scroll-dynamic" [['dustingetz.london-talk-2024.webview-scroll-dynamic/WebviewScrollDynamic nil]]}},
      "datomic-browser" {"datomic-browser" [['datomic-browser.datomic-browser/DatomicBrowser nil]]}}
  (fiddles-by-ns-segments {'tutorial nil}) := {"tutorial" [['tutorial nil]]})

(e/declare pages) ; inject so FiddleIndex is routable as a fiddle, also used by tutorial

(e/defn NotFoundPage [& args]
  (e/client
    (dom/h1 (dom/text "Page not found"))
    (dom/p (dom/text "Probably we broke URLs, sorry! ")
      (r/link ['/ []] (dom/text "index")))))

(e/defn SearchGrid [title Query Row]
  #_(r/focus [0]) ; search
  (let [!search (atom "") search (e/watch !search)
        xs! (Query search)
        n (count xs!)]
    (dom/fieldset (dom/legend (dom/text title " ")
                    (reset! !search (Input search))
                    (dom/text " (" n " items)"))
      (TablePicker* nil n (e/fn [index] (Row index (nth xs! index nil)))))))

(declare css)
(e/defn FiddleIndex []
  (e/client (dom/style (dom/text css)) (dom/props {:class "Explorer FiddleIndex"})
    #_(dom/pre (dom/text (pr-str r/route)))
    (SearchGrid (str `FiddleIndex2)
      (e/fn [search] (vec (treelister
                            (fn children [[k v]] (if (map? v) (into (sorted-map) v) nil))
                            (fn keep? [[k v]] (or (includes-str? k search) (includes-str? v search)))
                            (fiddles-by-ns-segments pages))))
      (e/fn Row [i [?tab [k v :as ?x]]]
        (when ?x
          (dom/td (dom/props {:style {:--tab ?tab}})
                  (dom/text k))
          (dom/td
            (if-not (map? v)
              (e/for [[qs F] (e/diff-by {} v)] ; grouped fiddles in ns
                (r/link ['/ [qs]] #_[qs] ; glitch? `[qs]` form crashes on 'fiddles but not 'blog
                        (dom/text (name qs)))))))))))

(e/defn FiddleRoot
  [fiddles
   & {:keys [default]
      :or {default `(FiddleIndex)}}]
  #_(dom/pre (dom/text (pr-str r/route)))
  (let [[fiddle & _] r/route]
    (if-not fiddle (r/ReplaceState! ['. default])
      (let [Fiddle (get fiddles fiddle NotFoundPage)]
        (set! (.-title js/document) (str (some-> fiddle name (str " – ")) "Electric Fiddle"))
        (binding [pages fiddles] ; todo untangle - tutorial uses some fiddle infrastructure, perhaps should use more?
          (case fiddle
            `FiddleIndex #_(FiddleIndex) (Fiddle) ; lol why - workaround crash 20241205
            (r/pop (Fiddle))))))))

(e/defn FiddleMain [ring-req fiddles & {:as props}] ; dev, optionally in prod (e.g. tutorial)
  (binding [e/http-request (e/server ring-req)
            dom/node js/document.body]
    (dom/div ; mandatory wrapper div https://github.com/hyperfiddle/electric/issues/74
      (r/router (r/HTML5-History)
        (FiddleRoot (merge {`FiddleIndex FiddleIndex} fiddles) props)))))

(def css (str hyperfiddle.electric-forms5/css
           "
/* Cosmetic grid standard */
.FiddleIndex fieldset { padding: 0; padding-left: 0em; background-color: white; }

/* Userland layout */
.FiddleIndex table td a+a { margin-left: .5em; }
.FiddleIndex table td { white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.FiddleIndex table td { padding-left: calc(var(--tab, 0) * 15px + 8px); }
.FiddleIndex table { grid-template-columns: 20em auto; }

/* Full height tables */
body:has(.FiddleIndex) { height: 100dvh; box-sizing: border-box; }
.FiddleIndex, .FiddleIndex *:has(table) { height: 100%; }

"
           ))
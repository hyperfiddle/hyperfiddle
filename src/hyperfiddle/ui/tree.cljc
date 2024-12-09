(ns hyperfiddle.ui.tree
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]))

(e/defn* RenderKey [k] k)

(e/defn Tree
  ([m] (Tree. identity m))
  ([intercept m]
   (e/client
     (dom/ul
       (dom/props {:class "tree"})
       (e/for-by first [[k v] (map intercept m)]
         (let [leaf? (not (map? v))]
           (dom/li (dom/props {:class (if leaf? "leaf" "branch")})
             (dom/span (dom/props {:class "key"}) (dom/text (RenderKey. k)))
             (if leaf?
               (dom/span (dom/props {:class "value"}) (dom/text v))
               (Tree. intercept v)))))))))

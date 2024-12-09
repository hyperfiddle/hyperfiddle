(ns hyperfiddle.ui.tags
  (:require
   [contrib.color :as color]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric3 :as e])
  #?(:require-macros [hyperfiddle.ui.tags]))

(e/defn Button [Body]
  (dom/button (dom/props {:class "block h-full aspect-square w-5 hover:drop-shadow flex items-center justify-center"})
    (Body)))

(defmacro button [& body] `(Button (e/fn [] ~@body)))

(e/defn Content [Body]
  (dom/span (dom/props {:class "pl-2 text-sm flex-1 text-nowrap text-ellipsis cursor-pointer"})
    (Body)))

(defmacro content [& body] `(Content (e/fn [] ~@body)))

(e/defn Tag [Body]
  (dom/div (dom/props {:class "py-0 border border-gray-200 rounded flex gap-0.5 items-center overflow-hidden bg-slate-50"})
    (Body)))

(defmacro tag [& body] `(Tag (e/fn [] ~@body)))



;; Experiment
(defn ns-color [ident] (when (qualified-ident? ident) (contrib.color/color (namespace ident) color/SEED-ANGLE 60 70)))

;; Experiment
(e/defn IdentTag [ident Body & {::keys [colored? color-fn], :or {colored? true, color-fn ns-color}}]
  (Tag
    (e/fn []
      (when-let [ns (namespace ident)]
        (Content (e/fn []
                   (dom/props {:class "pl-1.5 pr-2.5 text-gray-50"
                               :style {:background-color (#(and colored? (color-fn %)) ident)
                                       :clip-path "polygon(0 0, 100% 0, 90% 100%, 0 100%)"}})
                   (dom/text ns))))
      (Content (e/fn [] (dom/props {:style {:margin-left "-0.125rem", :padding-left 0}}) (dom/text (name ident))))
      (Body))))

(defmacro ident-tag [ident {::keys [colored? color-fn] :as props} & body]
  `(IdentTag ~ident (e/fn [] ~@body) ~@(mapcat identity props)))

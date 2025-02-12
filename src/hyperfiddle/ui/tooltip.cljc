(ns hyperfiddle.ui.tooltip
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [contrib.css]
   #?(:cljs [goog.style :refer [getTransformedSize getPageOffset]])))

(e/declare element)

#?(:cljs
   (defn get-content [^js tooltip-node]
     ;; Alternative method: clone content of a hidden div to floating tooltip
     (some-> tooltip-node (.getAttribute "data-tooltip"))))

#?(:cljs
   (defn coordinates [^js tooltip-element ^js target-node]
     (if target-node
       (if-let [offset (getPageOffset target-node)]
         (if-let [target-size (getTransformedSize target-node)]
           ;; TODO implement anchoring and smart off-screen anchor switch.
           ;;     E.g. :top :bottom :left :right
           ;;     If tooltip get clipped off-window on the right clipped, swap anchor for :left
           [(+ (.-x offset) #_(.-width target-size))
            (+ (.-y offset) (.-height target-size))]
           [0 0])
         [0 0])
       [0 0])))

(e/defn TooltipArea [Body]
  (binding [element (e/client (->> (dom/On "mousemove" #(some-> % .-target (.closest "[data-tooltip]")) nil)
                                   (dom/On "mouseleave" (constantly nil))))]
    (Body)))

(e/defn AdvancedTooltip [Body]
  (dom/div
    (dom/props {:class #=(contrib.css/css-slugify `Tooltip)})
    (Body element)))

#?(:cljs
   (defn basic-set-content! [^js tooltip-element text-content]
     (when (not-empty text-content)
       (set! (.-textContent tooltip-element) text-content))))

(e/defn Tooltip []
  "Render `data-tooltip` as plain text. To be styled with CSS."
  (AdvancedTooltip (e/fn [target-node]
                     (let [[x y] (coordinates dom/node target-node)]
                       (dom/props {:class [#=(contrib.css/css-slugify `Tooltip-basic) (#(when (zero? %) #=(contrib.css/css-slugify `Tooltip-hidden)) x)]
                                   :style {:left (str x "px") :top (str y "px")}})
                       (basic-set-content! dom/node (get-content target-node)) ; only set text, don't unset, just hide tooltip - enabling disappear animation
                       ))))


(def css
"

.hyperfiddle-ui-tooltip-Tooltip{
  left: var(--hyperfiddle-ui-tooltip-offsetX);
  top: var(--hyperfiddle-ui-tooltip-offsetY);
  position: absolute;
  z-index: 2;
}

.hyperfiddle-ui-tooltip-Tooltip.hyperfiddle-ui-tooltip-Tooltip-basic {
  background-color: white;
  padding: 0.5rem 1rem;
  border-radius: 0.25rem;
  border: 1px lightgray solid;
  box-shadow: 0 10px 15px -3px rgb(0 0 0 / 0.1), 0 4px 6px -4px rgb(0 0 0 / 0.1); /* tw-shadow-lg */
  white-space: pre;
  pointer-events: none;
}

.hyperfiddle-ui-tooltip-Tooltip.hyperfiddle-ui-tooltip-Tooltip-hidden {
  pointer-events: none;
  opacity: 0;
  transition: opacity 0.1s ease, top 0.1s steps(1,end), left 0.1s steps(1, end);
}
"
)
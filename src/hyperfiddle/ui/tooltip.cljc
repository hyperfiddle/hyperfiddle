(ns hyperfiddle.ui.tooltip
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [dustingetz.css :refer [css-slugify]]
   [clojure.string :as str]
   #?(:cljs [goog.style :refer [getTransformedSize getPageOffset]])
   #?(:cljs [goog.dom :refer [isElement]])))

(e/declare element)

#?(:cljs
   (defn get-content [^js tooltip-node]
     {:pre [(or (nil? tooltip-node) (isElement tooltip-node))]
      :post [(string? %)]}
     ;; Alternative method: clone content of a hidden div to floating tooltip
     (str (some-> tooltip-node (.getAttribute "data-tooltip")))))

#?(:cljs
   (defn coordinates [^js tooltip-element ^js target-node]
     {:pre [(isElement tooltip-element) (or (nil? target-node) (isElement target-node))]}
     (if target-node
       (if-let [offset (getPageOffset target-node)]
         (if-let [target-size (getTransformedSize target-node)]
           [(+ (.-x offset) #_(.-width target-size))
            (+ (.-y offset) (.-height target-size))]
           [0 0])
         [0 0])
       [0 0])))

#?(:cljs (def ^:private !prev-anchor (atom nil)))
#?(:cljs (def ^:private !anchor-gen (atom 0)))

;; When true, CSS @supports block handles positioning — inline left/top would
;; fight position-try-fallbacks, so JS must not set them.
#?(:cljs
   (def ^:private anchor-positioning?
     (try (js/CSS.supports "anchor-name" "--a") (catch :default _ false))))

#?(:cljs
   (defn ^:no-doc set-anchor-name! [^js tooltip-node ^js target-node]
     ;; Rotates anchor names so the browser re-evaluates position-try-fallbacks
     ;; from scratch (spec says "keep last-used fallback if it still fits").
     ;; Only updates on new target — keeps anchor on last element for fade-out.
     (when (and target-node (isElement target-node))
       (let [n    (swap! !anchor-gen inc)
             name (if (odd? n) "--hf-tooltip-a" "--hf-tooltip-b")]
         (when-some [prev @!prev-anchor]
           (.. prev -style (removeProperty "anchor-name")))
         (set! (.. target-node -style -anchorName) name)
         (set! (.. tooltip-node -style -positionAnchor) name)
         (reset! !prev-anchor target-node)))))

(defn ^:no-doc reducer [rf init]
  {:pre [(ifn? rf)]}
  (let [!state (atom init)]
    (fn [v]
      (swap! !state rf v))))

(def TOOLTIP-CLASS (css-slugify `Tooltip))

#?(:cljs
   (defn ^:no-doc is-tooltip? [node]
     {:pre [(some? node) (isElement node)]}
     (-> node .-classList (.contains TOOLTIP-CLASS))))

#?(:cljs
   (defn ^:no-doc find-tooltip-element [js-event]
     {:pre [(instance? js/Event js-event)]}
     (some-> (.-target js-event) (.closest "[data-tooltip]"))))

#?(:cljs
   (defn ^:no-doc filter-tooltip [rf]
     {:pre [(ifn? rf)]}
     (reducer (fn [prev next] (if (is-tooltip? (.-target next)) prev (rf prev next))) nil)))

(e/defn TooltipArea [Body]
  (binding [element (e/client (->> (dom/On "mousemove" (filter-tooltip (comp find-tooltip-element {})) nil)
                                   (dom/On "mouseleave" (constantly nil))))]
    (Body)))

(e/defn AdvancedTooltip [Body]
  (dom/div
    (dom/props {:class TOOLTIP-CLASS})
    (Body element)))

#?(:cljs
   (defn basic-set-content! [^js tooltip-element text-content]
     {:pre [(isElement tooltip-element) (string? text-content)]}
     (when-not (str/blank? text-content)
       (set! (.-textContent tooltip-element) text-content))))

(e/defn Tooltip []
  "Render `data-tooltip` as plain text. To be styled with CSS."
  (AdvancedTooltip (e/fn [target-node]
                     (set-anchor-name! dom/node target-node)
                     (dom/props {:class [(css-slugify `Tooltip-basic)
                                         (when-not target-node (css-slugify `Tooltip-hidden))]
                                 :style (when-not anchor-positioning?
                                          (let [[x y] (coordinates dom/node target-node)]
                                            {:left (str x "px") :top (str y "px")}))})
                     (basic-set-content! dom/node (get-content target-node)))))  ; only set text, don't unset — enables disappear animation

(def css
"

.hyperfiddle-ui-tooltip-Tooltip{
  position: absolute;
  z-index: 2;

  transition-behavior: allow-discrete; /* baseline 2024 */
  transition-property:        pointer-events, border-color;
  transition-duration:        0.9s,           0.1s;
  transition-delay:           0s,             0.8s;
  transition-timing-function: step-end,       ease-in;

}

.hyperfiddle-ui-tooltip-Tooltip.hyperfiddle-ui-tooltip-Tooltip-basic {
  --background-color: white;
  background-color: var(--background-color);
  padding: 0.5rem 1rem;
  border-radius: 0.25rem;
  border: 1px lightgray solid;
  box-shadow: 0 10px 15px -3px rgb(0 0 0 / 0.1), 0 4px 6px -4px rgb(0 0 0 / 0.1); /* tw-shadow-lg */
  white-space: pre-wrap;
  transform: translateX(1.5em);
}

.hyperfiddle-ui-tooltip-Tooltip.hyperfiddle-ui-tooltip-Tooltip-hidden {
  pointer-events: none;
  opacity: 0;
  border-color: var(--background-color);
  transition: opacity 0.1s ease, top 0.1s steps(1,end), left 0.1s steps(1, end);
}

/* ── CSS Anchor Positioning (Chrome 125+, Edge 125+) ──
   Auto-flips tooltip to avoid viewport overflow.
   JS skips inline left/top when supported, so position-try-fallbacks can
   freely swap left↔right, top↔bottom without !important interference. */
@supports (anchor-name: --a) {

  /* Last resort: span viewport width when tooltip overflows all sides */
  @position-try --full-width-below { top: anchor(bottom); left: 1rem; right: 1rem; width: auto; margin: 0; }
  @position-try --full-width-above { bottom: anchor(top); left: 1rem; right: 1rem; width: auto; margin: 0; }

  .hyperfiddle-ui-tooltip-Tooltip.hyperfiddle-ui-tooltip-Tooltip-basic {
    position: fixed;
    transform: none;
    width: max-content; /* forces overflow detection instead of text wrapping */
    overflow-wrap: break-word; /* break long tokens (e.g. Java class names) when clamped */
    /* position-anchor set by JS (rotating names to bust stale fallback cache) */
    top: anchor(bottom);
    left: anchor(left);
    margin-inline-start: 1.5em;
    position-try-fallbacks: flip-inline, flip-block, flip-block flip-inline,
                            --full-width-below, --full-width-above;
  }
}

@supports not (anchor-name: --a) {
  .hyperfiddle-ui-tooltip-Tooltip.hyperfiddle-ui-tooltip-Tooltip-basic {
    max-width: min(40ch, calc(100vw - 2rem));
    overflow-wrap: break-word;
  }
}
"
)
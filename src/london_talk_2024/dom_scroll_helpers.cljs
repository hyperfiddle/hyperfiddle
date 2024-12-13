(ns london-talk-2024.dom-scroll-helpers
  (:require [clojure.math :as math]
            [contrib.data :refer [clamp]]
            [contrib.missionary-contrib :as mx]
            [missionary.core :as m]))

(defn scroll-state [scrollable]
  (->> (m/observe
         (fn [!]
           (! [0 0 0])
           (let [sample (fn [] (! [(.. scrollable -scrollTop) ; optimization - detect changes (pointless)
                                   (.. scrollable -scrollHeight) ; snapshot height to detect layout shifts in flipped mode
                                   (.. scrollable -clientHeight)]))] ; measured viewport height (scrollbar length)
             (.addEventListener scrollable "scroll" sample #js {"passive" true})
             #(.removeEventListener scrollable "scroll" sample))))
    (mx/throttle 16) ; RAF interval
    (m/relieve {})))

(defn resize-observer [node]
  (m/relieve {}
    (m/observe (fn [!] (! [(.-clientHeight node)
                           (.-clientWidth node)])
                 (let [obs (new js/ResizeObserver
                             (fn [entries]
                               (let [content-box-size (-> entries (aget 0) .-contentBoxSize (aget 0))]
                                 (! [(.-blockSize content-box-size)
                                     (.-inlineSize content-box-size)]))))]
                   (.observe obs node) #(.unobserve obs))))))

(defn compute-overquery [overquery-factor record-count offset limit]
  (let [q-limit (* limit overquery-factor)
        occluded (clamp (- q-limit limit) 0 record-count)
        q-offset (clamp (- offset (math/floor (/ occluded overquery-factor))) 0 record-count)]
    [q-offset q-limit]))

(defn compute-scroll-window [row-height record-count clientHeight scrollTop]
  (let [padding-top 0 ; e.g. sticky header row
        limit (math/ceil (/ (- clientHeight padding-top) row-height)) ; aka page-size
        offset (int (/ (clamp scrollTop 0 (* record-count row-height)) ; prevent overscroll past the end
                      row-height))]
    (compute-overquery 1 record-count offset limit)))

(defn scroll-window [row-height record-count node] ; returns [offset, limit]
  (m/cp
    (let [[clientHeight] (m/?< (resize-observer node))
          [scrollTop] (m/?< (scroll-state node))] ; smooth scroll has already happened, cannot quantize
      (compute-scroll-window row-height record-count clientHeight scrollTop))))

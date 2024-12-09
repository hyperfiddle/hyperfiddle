(ns hyperfiddle.ui.loader
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.ui.spinner :refer [Spinner]]
            [heroicons.electric.v24.outline :as i]))

(e/defn* LoadingMask [message]
  (e/client
    (dom/div (dom/props {:class "hyperfiddle skipp"
                         :style {:position         :fixed
                                 :top              0, :bottom 0, :left 0, :right 0
                                 :margin           :auto
                                 :z-index          20
                                 :background-color "rgba(255,255,255,0.8)"
                                 :backdrop-filter  "blur(5px)"
                                 :display          :flex
                                 :flex-direction   :column
                                 :place-content    :center
                                 :place-items      :center}})
      (Spinner. {:style {:width "2rem"}})
      (dom/p (dom/text message)))))

(e/defn* LoadingFailedMask [Body]
  (e/client
    (dom/div (dom/props {:style {:position         :fixed
                                 :top              0, :bottom 0, :left 0, :right 0
                                 :margin           :auto
                                 :z-index          40
                                 :background-color "rgba(255,255,255,0.8)"
                                 :backdrop-filter  "blur(5px)"
                                 :display          :flex
                                 :flex-direction   :column
                                 :place-content    :center
                                 :place-items      :center}})
             (i/x-circle (dom/props {:style {:width "2rem"}}))
             (Body.))))

(e/defn* Loader
  "Runs an effect until an expected value is available.
  A common pattern usually expressed with (try … (catch Pending _ …)).
  Useful to display a loading mask over a loading page.

  Runs `Query`, expecting it to return `[::loading value]` or `[::loaded value]`.
  When Query returns `[::loading value]`, call `Loading` with `value`.
  When Query returns `[::loaded value]`, call `Body` with `value`.
  If `preload?` is true, call `Body` while ::loading, passing it the return value of `Loading`.

  Example:
  (Loader. {::preload? true
            ::Query (e/fn [] (try [::loaded (e/offload (constantly \"result\"))]
                               (catch Pending _
                                 [::loading \"Content is loading…\"])))
            ::Loading (e/fn [value]
                        (LoadingMask. value) ; `value` is \"Content is loading…\"
                        nil ; Body will be called with `nil` because ::preload? is true
                        )}
    (e/fn [value]
      (prn value) ; nil, then \"result\"
      ))
  "
  [{::keys [preload? Query Loading] :or {preload? false}} Body]
  (let [[status value] (Query.)
        loading?       (= ::loading status)
        body-value     (if loading? (Loading. value) value)]
    (when (or preload? (not loading?))
      (Body. body-value))))






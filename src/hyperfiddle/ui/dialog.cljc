(ns hyperfiddle.ui.dialog
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [heroicons.electric.v24.outline :as i]
            [hyperfiddle.ui.stage :as stage])
  #?(:cljs (:require-macros [hyperfiddle.ui.dialog])))

(e/def open? false)
(e/def close! (constantly nil))
(e/def on-close (constantly nil))
(e/def !features (atom #{}))

(e/def label-id)
(e/def description-id)

(e/defn* RegisterFeature [feat]
  (swap! !features conj feat)
  (e/on-unmount #(swap! !features disj feat)))

(defmacro title [{::keys [as] :or {as 'hyperfiddle.electric-dom2/h3}} & body]
  `(~as (dom/props {:id    label-id
                    :style {:grid-area "title"}})
    (new RegisterFeature ::labelled)
    ~@body))

(defmacro button-close [{::keys [as] :or {as 'hyperfiddle.electric-dom2/button}} & body]
  `(~as
    (dom/props {:type "reset" :style {:grid-area "close", :justify-self :end}})
    (dom/on! "click" stage/discard!)
    (i/x-mark (dom/props {:class "w-4 h-4 cursor-pointer"}))
    ~@body))

(defmacro button-discard [{::keys [as] :or {as 'hyperfiddle.electric-dom2/button}} & body]
  `(~as
    (dom/props {:type "reset"})
    (dom/on! "click" stage/discard!)
    ~@body))

(defmacro button-commit [{::keys [as] :or {as 'hyperfiddle.electric-dom2/button}} & body]
  `(~as
    (dom/props {:type "submit"})
    (dom/on "click" (e/fn* [_] (stage/Commit.)))
    ~@body))

(defmacro description [{::keys [as] :or {as 'hyperfiddle.electric-dom2/p}} & body]
  `(~as
    (dom/props {:id description-id, :style {:grid-area "description"}})
    (new RegisterFeature ::described)
    ~@body))

(defmacro content [{::keys [as] :or {as 'hyperfiddle.electric-dom2/div}} & body]
  `(~as
    (dom/props {:style {:grid-area "content"}})
    (stage/stage! (do ~@body))))

(defmacro actions [{::keys [as] :or {as 'hyperfiddle.electric-dom2/div}} & body]
  `(~as (dom/props {:class "flex justify-end gap-2", :style {:grid-area "actions"}})
    ~@body))

(e/defn* Panel [Body]
  (e/client
    (dom/form
      (dom/props {:method :dialog
                  :class  "grid gap-2"
                  :style  {:grid-template-columns "1fr auto"
                           :grid-template-areas   " \"title close\" \"description description\" \"content content\" \"actions actions\" "}})
      (let [Commit stage/Commit]
        (binding [stage/Commit (e/fn* []
                                 (case (Commit.) ; sequence effects
                                   (stage/discard!)))]
          (Body.))))))

(defmacro panel [& body]
  `(new Panel (e/fn* [] ~@body)))

#?(:cljs
   (defn reset-form [^js node]
     (if (exists? (.-reset node))
       (.reset node)
       (when-let [^js form (first (filter #(= "FORM" (.-tagName %)) (.-children node)))]
         (.reset form)))))

(defn set-open-state [node discard! modal? open?]
  (if open?
    (if modal?
      (.showModal node)
      (.show node))
    (do (discard!)
        (.close node))))

(e/defn* Dialog [{::keys [modal? OnSubmit]
                  :or {OnSubmit `(e/fn* [x#] x#)}}
                 Body]
  (stage/staged OnSubmit
    (binding [label-id       (str (gensym "label_"))
              description-id (str (gensym "description_"))
              !features      (atom #{})]
      (e/client
        (dom/dialog
          (dom/props {:aria-role :dialog
                      :style     {:cursor :auto, :position :absolute}})
          (when modal?
            (dom/props {:aria-modal true}))
          (let [features (e/watch !features)]
            (when (features ::labelled)
              (dom/props {:aria-labelledby label-id,}))
            (when (features ::described)
              (dom/props {:aria-describedby description-id})))
          (binding [close! #(on-close)]
            (binding [stage/discard! (fn [] (reset-form dom/node) (stage/discard!) (close!))]
              (set-open-state dom/node stage/discard! modal? open?)
              (dom/on! "close" (fn [e] (.preventDefault e) ; important ! fire before form submit
                                 (close!)))
              (dom/on! "click" (fn [^js e]
                                 (.stopPropagation e)
                                 (when (= (.-target e) dom/node)
                                   (stage/discard!)))) ;; blur modal (click on ::backdrop)
              (dom/on! "keyup" (fn [e] (when (= "Escape" (.-key e)) (stage/discard!))))
              (Body.))))))))

(defmacro dialog [{::keys [modal?] :as props} & body]
  `(new Dialog ~props (e/fn* [] ~@body)))

(defn event-happened-in-child-dialog? [current-node event]
  (.contains current-node (.. event -target (closest "dialog"))))

(defmacro button [{::keys [as] :or {as 'hyperfiddle.electric-dom2/button}} & body]
  `(let [!open# (atom false)]
     (binding [open?    (e/watch !open#)
               on-close #(reset! !open# false)]
       (dom/button
         (dom/props {:type :button, :style {:position :relative}})
         (dom/on! "click" (fn [^js e#]
                            (.stopPropagation e#)
                            (swap! !open# not)))
         (dom/on! "keyup" (fn [^js e#]
                            (when (event-happened-in-child-dialog? dom/node e#)
                              (.preventDefault e#))))
         ~@body))))

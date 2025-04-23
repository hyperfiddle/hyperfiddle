(ns dustingetz.offload-ui
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.electric-forms5 :as forms]))

(defn now-ms []
  #?(:clj (System/currentTimeMillis)
     :cljs (.getTime (new js/Date))))

(defn always [f] (fn [& _] (f)))

(e/defn Stable-fn [f] ((e/capture-fn) f))
(e/defn Always [f & args] ((always (Stable-fn f)) args))


(e/defn Tap-diffs [label x] (e/Tap-diffs (partial prn label) x))

(defn retain [pred]
  (partial swap! (atom nil) (fn [old new] (if (pred new) new old))))

(e/defn Latch [x]
  (let [!prev (atom [])]
    (reset! !prev [x])
    (e/diff-by {} (e/watch !prev))))

#?(:cljs
   (defn format-duration [ms]
     (cond
       (< ms 1000) (str ms "ms")
       (< ms 10000) (-> (/ ms 100) (Math/floor) (/ 10) (.toFixed 1) (str "s"))
       () (str (Math/round (/ ms 1000)) "s"))))

(e/defn Interruptible [F]
  (e/fn [x]
    (let [[t status] (e/Token x)
          ack!       (fn [status] (when t (t status)))
          v          (Latch (e/When t (F x)))]
      (Always (partial ack! ::done) v)
      [(e/pure v) (e/Reconcile (or status ::running)) (Stable-fn (partial ack! ::interrupted))])))

(e/defn OffloadUI* [nm f status interrupt!]
  (e/client
    ;; Do we want to display server start/end times or client? does it matter if there's a difference?
    ;; do clock drifts matter? are drifts always constant?
    (let [start (e/server (Always now-ms f)) ; wart, we want to time on the client, but f is not serializable
          end (e/server (case status
                          ::running (e/System-time-ms)
                          (::done ::interrupted) (now-ms)))]
      (when-let [e (forms/Button :label "×" :class "cancel" :disabled (not= status ::running))]
        (let [h (hash e)]
          (e/server
            (Always interrupt! h))))
      (dom/props {:data-timing-label nm
                  :data-timing-duration (format-duration (- end start))
                  :data-timing-status (name status)}))))

(e/defn OffloadUI [nm f]
  (e/server
    (let [[v< status interrupt!] (e/call (Interruptible e/Offload-reset) f)]
      (OffloadUI* nm f status interrupt!)
      (e/join v<))))

(e/defn Initialized [x init-v]
  (let [!v (atom init-v)]
    (reset! !v x)
    (e/watch !v)))

(def css
"


[data-timing-label]{--color: orange;}
[data-timing-label][data-timing-status=interrupted]{--color: crimson;}
[data-timing-label][data-timing-status=done]{--color: green;}

[data-timing-label]{ position: relative; border: 1px var(--color) solid;}
[data-timing-label]::before{
  display: block;
  background-color: var(--color);
  color: white;
  content: attr(data-timing-label) \" \" attr(data-timing-duration);
  padding: 0.2em 0.5em 0.1em 0.5em;
  font-size: 0.75em;
  position: absolute;
  z-index: 1;
  bottom: 100%;
  left: 1em;
  margin: auto;
  border-top-right-radius: 3px;
}

[data-timing-label] button.cancel{
  position: absolute;
  z-index: 2;
  bottom: 100%;
  left: -1px;
  width: 1.1em;
  height: 1.1em;
  padding: 0;
  line-height: 0;
  border-radius: 0;
  border: none;
  border-top-left-radius: 3px;
  border-bottom: 1px var(--color) solid;
  background-color: var(--color);
  color: white;

}


[data-timing-label] button.cancel:hover{
  background-color: #ffbf00;
  cursor: pointer;
}

[data-timing-label] button.cancel:disabled{
  color: gray;
}
")

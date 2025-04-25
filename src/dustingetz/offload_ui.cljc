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

(defn to-fixed [num decimal-places]
  #?(:cljs (.toFixed num decimal-places)
     :clj (format (str "%." decimal-places "f") (double num))))

(defn format-duration [ms]
  (cond
    (< ms 1000) (str ms "ms")
    (< ms 10000) (-> (/ ms 100) (Math/floor) (/ 10) (to-fixed 1) (str "s"))
    () (str (Math/round (/ ms 1000)) "s")))

(e/defn Interruptible [F]
  (e/fn [x]
    (let [[t status] (e/Token x)
          ack!       (fn [status] (when t (t status)))
          v          (Latch (e/When t (F x)))]
      (Always (partial ack! ::done) v)
      [(e/pure v)
       (e/Reconcile (or status ::running))
       (Stable-fn (partial ack! ::interrupted))])))

(defn timed [f]
  (fn [& args]
    (let [start (now-ms)
          result (apply f args)
          end (now-ms)]
      [result start end])))

(e/defn Initialized [x init-v]
  (let [!v (atom init-v)]
    (reset! !v x)
    (e/watch !v)))

(defn time-delta [start end]
  (cond (< end start) 0
        (zero? start) 0
        () (- end start)))

(e/defn OffloadUI* [node nm f query-start query-end status interrupt!]
  (e/client
    ;; Do we want to display server start/end times or client? does it matter if there's a difference?
    ;; do clock drifts matter? are drifts always constant?
    (let [electric-start (e/server (Always now-ms f))
          electric-end (e/Reconcile
                         (case status
                           ::running (e/server #_(now-ms) (e/System-time-ms))
                           (::done ::interrupted) (e/server (now-ms))))]
      (binding [dom/node node]
        (dom/div (dom/props {:class "timing"})
          (dom/span (dom/text (pr-str nm)
                              (str " Total: " (format-duration (time-delta electric-start electric-end))
                                " | Query: " (case status ::running "…", (::done ::interrupted) (format-duration (time-delta query-start query-end))))))
          (e/Reconcile (when-let [e (forms/Button :label "×" :class "cancel" :disabled (not= status ::running))]
                         (let [h (hash e)]
                           (e/server
                             (Always interrupt! h)))))
          (dom/props {#_#_:data-timing-label (pr-str nm)
                      #_#_:data-timing-duration (str "Total: " (format-duration (time-delta electric-start electric-end))
                                              " | Query: " (case status ::running "…", (::done ::interrupted) (format-duration (time-delta query-start query-end))))
                      :data-timing-status (name status)
                      :data-timing-start electric-start
                      :data-timing-end electric-end
                      }))))))

(e/defn OffloadUI
  ([nm f] (OffloadUI (e/client dom/node) nm f))
  ([node nm f]
   (e/server
     (let [[v< status interrupt!] (e/call (Interruptible e/Offload-reset) (timed f))
           [v start end] (e/join v<)]
       (OffloadUI* node nm f start end status interrupt!)
       v))))

(def css
"


:has(>.timing){--color: orange; will-change: outline-color;}
:has(>.timing[data-timing-status=interrupted]){--color: crimson;}
:has(>.timing[data-timing-status=done]){--color: green; /*animation: timing-fade-out 1s ease-out forwards;*/}
/*.timing[data-timing-status]:not([data-timing-status=done])::before{display: block;}*/

@keyframes timing-fade-out{
  from { outline-color: var(--color); }
  to   { outline-color: transparent; }
}

:has(>.timing) > .timing + *:not(.timing){ position: relative; outline: 1px var(--color) solid;}
/*.timing:has(input:hover):not(:has(label:hover))::before { opacity: 0.2; }*/
.timing{
  /*pointer-events: none;*/
  box-sizing: border-box;
  background-color: var(--color);
  color: white;
  padding-left: 0.5rem;
  font-size: 0.75rem;
  font-weight: normal;
  height: 1.1rem;
  width: fit-content;
  z-index: 1;
  border-top-left-radius: 3px;
  border-top-right-radius: 3px;
  overflow: clip;
}


/*:has(>.timing):hover:not(:has(.timing:hover)) { outline: 1px var(--color) solid; animation: none; }*/
:has(>.timing):hover > .timing { display: block; }

/* .timing:hover:not(:has(.timing:hover)) > button.cancel{display: block;} */

.timing > button{
  width: 1.1rem;
  height: 1.1rem;
  padding: 0;
  line-height: 0;
  border-radius: 0;
  border: none;
  border-top-left-radius: 3px;
  border-bottom: 1px var(--color) solid;
  background-color: var(--color);
  color: white;
  border: none;
}


.timing button.cancel:hover{
  background-color: #ffbf00;
  cursor: pointer;
}

.timing button.cancel:disabled{
  color: gray;
}


/* Tweaks */

legend:has(.timing){ border: 2px #e9e9e9 groove; }

.timing[data-timing-status] ~ *:not(.timing) td[data-empty]::before { content: \"⏳\"; }

")

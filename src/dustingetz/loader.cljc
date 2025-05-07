(ns dustingetz.loader
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.electric-forms5 :as forms]))

(defn now-ms []
  #?(:clj (System/currentTimeMillis)
     :cljs (.getTime (new js/Date))))

(defn always [f] (fn [& _] (f)))

(e/defn Stable-fn [f] ((e/capture-fn) f))
(e/defn Always [f & args] ((always (Stable-fn f)) args))

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

(defn timed [f & args]
  (let [start (now-ms)
        result (apply f args)
        end (now-ms)]
    [result start end]))

(e/defn Initialized [x init-v]
  (let [!v (atom init-v)]
    (reset! !v x)
    (e/watch !v)))

(defn time-delta [start end]
  (cond (< end start) 0
        (zero? start) 0
        () (- end start)))

(e/defn OffloadUI [node label f query-start query-end status interrupt!]
  (e/client
    ;; Do we want to display server start/end times or client? does it matter if there's a difference?
    ;; do clock drifts matter? are drifts always constant?
    (let [electric-start (e/server (Always now-ms f))
          electric-end (e/Reconcile
                         (case status
                           ::running #_(e/server (now-ms)) (e/System-time-ms)
                           (::done ::interrupted) (e/server (now-ms))))]
      (binding [dom/node (e/Reconcile (or node dom/node))]
        (dom/div (dom/props {:class "data-loader"})
                 (dom/span (dom/text (#(when (and % (not= "" %)) (str (pr-str %) " ")) label)
                              (str "Total: " (format-duration (time-delta electric-start electric-end))
                                " | Query: " (case status ::running "…", (::done ::interrupted) (format-duration (time-delta query-start query-end))))))
          (e/Reconcile (when-let [e (forms/Button :label "×" :class "cancel" :disabled (not= status ::running))]
                         (let [h (hash e)]
                           (e/server
                             (Always interrupt! h)))))
          (dom/props {:data-loader-status (name status)
                      :data-loader-start electric-start
                      :data-loader-end electric-end
                      }))))))

(defn delayed [delay-ms f & args]
  #?(:clj (Thread/sleep delay-ms))
  (apply f args))

(e/defn Offload
  [f & {:keys [label node delay-ms] :or {delay-ms 0}}]
  (e/server
    (let [[v< status interrupt!] (e/call (Interruptible e/Offload-reset) (partial timed (partial delayed delay-ms f)))
          [v start end] (e/join v<)]
      (OffloadUI node label f start end status interrupt!)
      v)))

;; (def ground (constantly nil))

#_(e/defn OffloadUI [_nm x] ; FIXME this impl triggers a conditional glitch at a distance in entity-browser3
  ;; (ground x) ; works
  ;; (e/server (ground x)) ; works
  ;; (e/client (e/server (ground x))) ; works
  ;; (e/client (identity (e/server (ground x)))) ; works
  ;; (e/client (ground (e/server (ground x)))) ; works
  ;; (e/client (ground (e/pure (e/server (ground x))))) ; works
  ;; (e/client (e/call (e/fn [] (e/server (ground x))))) ; FAIL
  ;; (e/client (e/join (e/pure (e/server (ground x))))) ; FAIL
  (e/server x))


(def css
"

:has(>.data-loader){--color: orange; will-change: outline-color;}
:has(>.data-loader[data-loader-status=interrupted]){--color: crimson;}
:has(>.data-loader[data-loader-status=done]){--color: green; /*animation: data-loader-fade-out 1s ease-out forwards;*/}

@keyframes data-loader-fade-out{
  from { outline-color: var(--color); }
  to   { outline-color: transparent; }
}

.data-loader + :is(li,td,tr,option,[role=cell],[role=gridcell],[role=row],[role=listitem],[role=menuitem],[role=option],[role=section],[role=tab],[role=treeitem]):first-of-type
 { border-top: 1px var(--color) solid; }
.data-loader + :is(li,td,tr,option,[role=cell],[role=gridcell],[role=row],[role=listitem],[role=menuitem],[role=option],[role=section],[role=tab],[role=treeitem]):last-of-type
 { border-bottom: 1px var(--color) solid; }

.data-loader + :not(:is(li,td,tr,option,[role=cell],[role=gridcell],[role=row],[role=listitem],[role=menuitem],[role=option],[role=section],[role=tab],[role=treeitem]))
 { border: 1px var(--color) solid; }

.data-loader{
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


:has(>.data-loader):hover > .data-loader { display: block; }

.data-loader > button{
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


.data-loader button.cancel:hover{
  background-color: #ffbf00;
  cursor: pointer;
}

.data-loader button.cancel:disabled{
  color: gray;
}


/* Tweaks */

legend:has(.data-loader){ border: 2px #e9e9e9 groove; }

.data-loader[data-loader-status] ~ *:not(.data-loader) td[data-empty]::before { content: \"⏳\"; }

")

(e/defn Constantly [x] (e/fn [& args] x))

(defn either [f & args]
  (try [(apply f args) ::nil]
       (catch #?(:clj Throwable, :cljs :default) t
         [::nil t])))

(e/defn Loader ; TODO provide interrupt affordance and timing stats
  ([f {:keys [Busy Failed] :as props}] (Loader (e/amb) f props))
  ([init f {:keys [Busy Failed] :or {Busy (Constantly (e/amb)), Failed (Constantly (e/amb))}}]
   (e/server
     (binding [OffloadUI (e/fn [node label f start-time end-time status interrupt!]
                           (e/Reconcile
                             (case status
                               ::running (Busy)
                               nil))
                           (e/amb))]
       (let [[v error] (Offload (partial either f))]
         (e/Reconcile
           (if (e/Some? v)
             (e/Reconcile (if (not= ::nil error) (do (Failed error) (e/amb)) v))
             init)))))))
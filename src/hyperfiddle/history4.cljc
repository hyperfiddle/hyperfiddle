;; Evolution of history3. HTML5History is synchronous and navigation prevention is token-based.
(ns hyperfiddle.history4
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [contrib.cljs-target :refer [do-browser]]
   [hyperfiddle.rcf :as rcf :refer [tests % tap with]]
   [hyperfiddle.electric3 :as e]
   )
  #?(:clj (:import [clojure.lang IRef IAtom]))
  #?(:cljs (:require-macros hyperfiddle.history4))
  )

(comment
  (rcf/enable! true))

;;; History

(defprotocol IHistory
  (navigate! [this route])
  (back! [this])
  (forward! [this])
  (replace-state! [this new-state]))

(declare notify-watches)

(defn updatef [[history idx] f] [(update history idx f) idx])

#?(:clj
   (defrecord AtomHistory [^IAtom state watches max-size]
     IAtom
     (swap [this f]           (notify-watches this (swap-vals! state updatef f)))
     (swap [this f arg]       (.swap this #(f % arg)))
     (swap [this f arg1 arg2] (.swap this #(f % arg1 arg2)))
     (swap [this f x y args]  (.swap this #(apply f % x y args)))
     (reset [this newval]     (.swap this (constantly newval)))
     (compareAndSet [this oldv newv]
       (loop []
         (let [refv (deref this)]
           (if (not= oldv refv)
             false
             (or (compare-and-set! state refv (updatef refv (constantly newv)))
               (recur))))))

     IRef
     (setValidator [_ _] (throw (UnsupportedOperationException. "History does not support validators")))
     (getValidator [_] (throw (UnsupportedOperationException. "History does not support validators")))
     (getWatches [_] (deref watches))
     (addWatch [this key callback]
       (swap! watches assoc key callback)
       this)
     (removeWatch [_ key] (swap! watches dissoc key))
     (deref [_] (let [[history idx] @state]
                  (get history idx)))
     ))

(defn notify-watches [this [oldstate newstate]]
  (let [oldval (apply get oldstate)
        newval (apply get newstate)]
    (doseq [[key callback] @(:watches this)]
      (callback key this oldval newval))))

;; (add-watch (atom 0) ::key (constantly true))

#?(:clj (defmethod print-method AtomHistory [x w] (print-dup x w)))

#?(:cljs
   (defrecord AtomHistory [state watches max-size]
     IAtom
     ISwap
     (-swap! [this f]
       (notify-watches this (swap-vals! state updatef f))
       (deref this))
     (-swap! [this f arg]       (-swap! this #(f % arg)))
     (-swap! [this f arg1 arg2] (-swap! this #(f % arg1 arg2)))
     (-swap! [this f x y args]  (-swap! this #(apply f % x y args)))

     IReset
     (-reset! [this newval]     (-swap! this (constantly newval)))

     IWatchable
     (-add-watch [this key callback]
       (swap! watches assoc key callback)
       this)
     (-remove-watch [_ key] (swap! watches dissoc key))

     IDeref
     (-deref [_] (let [[history idx] @state]
                  (get history idx)))
     ))

(extend-type AtomHistory
  IHistory
  (navigate! [this route]
    (notify-watches this
      (swap-vals! (.-state this)
        (fn [[history idx]]
          (if (= (count history) (.-max-size this)) ; TODO use a ring data structure for O(1) ops
            [(conj (subvec history 1)           route)      idx]
            [(conj (subvec history 0 (inc idx)) route) (inc idx)])))))
  (back! [this]
    (notify-watches this
      (swap-vals! (.-state this) (fn [[history idx]] [history (max (dec idx) 0)]))))
  (forward! [this] (notify-watches this
                     (swap-vals! (.-state this) (fn [[history idx]] [history (min (inc idx) (dec (count history)))]))))
  (replace-state! [this new-state] (reset! this new-state)))


(defn atom-history
  "Return a new IHistory instance backed by an atom.
  Initial history state can be provided with `initial-state`.
  Default history size is unbounded and can be constrained to `max-size` elements in a FIFO way.
  A negative value or 0 has no effect."
  ([] (atom-history nil 0))
  ([initial-state] (atom-history initial-state 0))
  ([initial-state max-size] (->AtomHistory (atom [[initial-state] 0]) (atom {}) max-size)))

#?(:clj
   (defrecord ProxyHistory [parent ^IAtom state]
     IAtom
     (swap [this f]           (swap! state f))
     (swap [this f arg]       (swap! state f arg))
     (swap [this f arg1 arg2] (swap! state f arg1 arg2))
     (swap [this f x y args]  (apply swap! state f x y args))
     (reset [this newval]     (reset! state newval))
     (compareAndSet [this oldv newv] (compare-and-set! state oldv newv))

     IRef
     (setValidator [_ _] (throw (UnsupportedOperationException. "History does not support validators")))
     (getValidator [_] (throw (UnsupportedOperationException. "History does not support validators")))
     (getWatches [_] (.getWatches state))
     (addWatch [this key callback] (add-watch state key callback) this)
     (removeWatch [_ key] (remove-watch state key))
     (deref [_] (deref state))
     ))


#?(:clj (defmethod print-method ProxyHistory [x w] (print-dup x w)))

#?(:cljs
   (defrecord ProxyHistory [^IHistory parent ^IAtom state]
     IAtom
     ISwap
     (-swap! [this f]           (swap! state f))
     (-swap! [this f arg]       (swap! state f arg))
     (-swap! [this f arg1 arg2] (swap! state f arg1 arg2))
     (-swap! [this f x y args]  (apply swap! state f x y args))

     IReset
     (-reset! [this newval]     (reset! state newval))

     IWatchable
     (-add-watch [this key callback] (add-watch state key callback)
       this)
     (-remove-watch [_ key] (remove-watch state key))

     IDeref
     (-deref [_] (deref state))
     ))

(extend-type ProxyHistory
  IHistory
  (navigate! [this route] (navigate! (.-parent this) route))
  (back! [this] (back! (.-parent this)))
  (forward! [this] (forward! (.-parent this)))
  (replace-state! [this new-state] (reset! this new-state)))


(defn proxy-history
  "Return a new IHistory instance backed by an atom.
  History state is stored in an atom.
  Navigation is forwarded to the `parent` history.
  Initial state is provided with `initial-state`. "
  ([parent] (proxy-history parent nil))
  ([parent initial-state] (->ProxyHistory parent (atom initial-state)))) ; keep state local, not in url


(tests
  "navigate"
  (let [h (atom-history)]
    @h := nil
    (navigate! h :a)
    @h := :a))

(tests
  "back and forth"
  (let [h (atom-history)]
    @h := nil
    (navigate! h :a)
    (navigate! h :b)
    @h := :b
    (back! h)
    @h := :a
    (back! h)
    @h := nil
    (forward! h)
    @h := :a
    (navigate! h :c)
    @h := :c
    (forward! h)
    @h := :c))

(tests
  "replace-state"
  (let [h (atom-history)]
    @h := nil
    (replace-state! h :a)
    @h := :a
    (navigate! h :b)
    @h := :b
    (replace-state! h :a)))

(tests
  "max-size and initial value"
  (let [h (atom-history :init 1)]
    @h := :init
    (navigate! h :a)
    @h := :a
    (navigate! h :b)
    @h := :b
    (back! h)
    @h := :b
    ))

(defn history? [h]
  (and (satisfies? IHistory h)
    (#?(:clj instance?, :cljs satisfies?) IAtom h)))

(tests
  (history? (atom-history)) := true)

;;; 1. and 2.

(e/declare history)                    ; History instance mutable ref

;; HTML5 integration

(defn absolute [path]
  (assert (string? path) (str "Expected path to be a string, got " (pr-str path)))
  (str "/" (str/replace-first path #"^/+" "")))

(tests
  (absolute "foo")        := "/foo"
  (absolute "/foo")       := "/foo"
  (absolute "//foo")      := "/foo"
  (absolute "//foo//bar") := "/foo//bar")

#?(:cljs
   (do-browser

     (defn throttler [rate-ms]
       (let [!nextf (atom nil)
             !running (atom false)]
         (fn rec [f]
           (if @!running
             (reset! !nextf f)
             (do (reset! !running true)
                 (f)
                 (.setTimeout js/window (fn [] (reset! !running false)
                                          (when-let [nextf @!nextf]
                                            (reset! !nextf nil)
                                            (rec nextf)))
                   rate-ms))))))

     ;; User agent limits HistoryAPI to 100 changes / 30s timeframe (https://bugs.webkit.org/show_bug.cgi?id=156115)
     ;; Firefox and Safari log an error and ignore further HistoryAPI calls for security reasons.
     ;; Chrome does the same but can also hang the tab: https://bugs.chromium.org/p/chromium/issues/detail?id=1038223
     (let [throttle (throttler 300)]    ; max 3changes/s, 90/30s
       (defn replaceState! [path] (throttle #(.replaceState js/window.history (.. js/window -history -state) "" (absolute path)))))

     (defn html5-pushState! [path] (.pushState js/window.history nil "" (absolute path)))
     (defn html5-back! [] (.back js/window.history))
     (defn html5-forward! [] (.forward js/window.history))

     (defn html5-path []
       (let [loc (.-location js/window)]
         (str (.-pathname loc) (.-search loc) (.-hash loc))))

     (defrecord HTML5History [!history !latest-navigate]
       IAtom
       ISwap
       (-swap! [_ f]
         ;; (prn "swap! " @!history " -> " (f @!history))
         (let [newval (swap! !history f)]
           ;; (prn "replaceState! for " newval)
           (replaceState! newval)
           newval))
       (-swap! [this f arg] (swap! this #(f % arg)))
       (-swap! [this f arg1 arg2] (swap! this #(f % arg1 arg2)))
       (-swap! [this f x y args] (swap! this #(apply f % x y args)))
       IReset
       (-reset! [this newval] #_(prn "reset! html5-history with " newval) (-swap! this (constantly newval)))
       IWatchable
       (-add-watch [_ key callback] (add-watch !history key callback))
       (-remove-watch [_ key] (remove-watch !history key))
       IDeref
       (-deref [_] (deref !history))
       IHistory
       (navigate! [_ route]
         (reset! !latest-navigate (.getTime (new js/Date)))
         (html5-pushState! route)
         (navigate! !history route)
         nil)
       (back! [_]
         (back! !history)
         (html5-back!)
         nil)
       (forward! [_]
         (html5-forward!)
         (forward! !history)
         nil)
       (replace-state! [this new-state]
         #_(prn "called replace-state! with " new-state)
         (reset! this new-state)))

     (defn set-history! [^HTML5History !history path timestamp]
       ;; Discard if time of event is older than latest history navigation (prevent races)
       (let [latest-user-navigate-time (deref (.-!latest-navigate !history))
             current-event-time (+ (.-timeOrigin js/performance) timestamp)]
         #_(prn "history Δ" (- latest-user-navigate-time current-event-time))
         (when (< latest-user-navigate-time current-event-time)
           (reset! !history path))))

     (defn html5-history "Will write to browser history, but must be wired for reads from electric." []
       (->HTML5History (atom-history (html5-path)) (atom 0)))))








(ns hyperfiddle.navigator6.utils
  (:refer-clojure :exclude [Delay])
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-forms5 :as forms]
            [missionary.core :as m])
  #?(:clj (:import [java.util.concurrent.locks LockSupport]))
  #?(:cljs (:require-macros [hyperfiddle.navigator6.utils :refer [in rebooting freezing]])))

(defmacro freezing [lexical-syms & body]
  `(let [~@(interleave lexical-syms (map #(list `e/Snapshot %) lexical-syms))]
     ~@body))

(defmacro rebooting [sym & body] `(e/for [~sym (e/diff-by identity (e/as-vec ~sym))]
                                    #_(println "Rebooting on " ~sym)
                                    (let [~sym (e/Snapshot ~sym)]
                                      ~@body)))

(defmacro in [dom-node & body] `(binding [dom/node ~dom-node] ~@body))

(defn glitch-guard
  ([f] (glitch-guard some? f))
  ([pred f]
   (fn [a & as]
     (when (pred a)
       (apply f a as)))))

(defn caching-nth
  "Returns f where (f i) ≈ (nth coll i) but caches prefix in a vector.
   After calling (f i):
   - any call to (f j) where j ≤ i will be 𝓞(1).
   - any call to (f j) where j > i will be O(∆(i, j)), which is still 𝓞(n),
     but on streamy collections the base case is j = i + 1, which will be 𝓞(1).
   Keeps only realized prefix + current tail. Same memory behavior as nth (i.e. will OOM accessing large N)."
  ([coll] (caching-nth coll ::not-found))
  ([coll not-found]
   (let [!v (volatile! [])            ; cached prefix as a vector
         !s (volatile! (seq coll))]   ; current tail
     (fn f
       ([i] (let [x (f i not-found)]
              (if (identical? x ::not-found)
                (throw #?(:clj (IndexOutOfBoundsException. (str i)), :cljs (js/Error. "Index out of bounds")))
                x)))
       ([i nf]
        (when (neg? i) (throw #?(:clj (IndexOutOfBoundsException. (str i)), :cljs (js/Error. "Index out of bounds"))))
        ;; ensure cache has (inc i) items
        (let [need (- (inc i) (count @!v))]
          (when (pos? need)
            (let [s0 @!s]
              (loop [n need, s s0, tv (transient @!v)]
                (if (zero? n)
                  (do (vreset! !v (persistent! tv))
                      (vreset! !s s))
                  (if (nil? s)
                    (do (vreset! !v (persistent! tv))
                        (vreset! !s nil))
                    (recur (dec n) (next s) (conj! tv (first s))))))))
        (let [v @!v]
          (if (< i (count v)) (nth v i) nf))))))))


(defn run-with-bindings [bindings f & args]
  #?(:clj (with-bindings bindings (apply f args))))

(e/defn Delay [v] (e/fn [] (e/Snapshot v))) ; used to return a server object in a form commands. Snapshot ensures no spooky effects at a distance happens on accidental V change. E.g. glitches.

(e/defn InterceptCommand [command-name F commands]
  (forms/Parse (e/fn [[op & args :as command]]
                 (e/Reconcile (if (= op command-name)
                                (F command)
                                command)))
    commands))

#?(:clj
   (defn ^:no-doc sleep-hard
     "Debugging utility. Pause for `ms` milliseconds. Ignores interrupts during the pause. Restores interrupt status afterwards."
     [^long ms]
     (let [deadline (+ (System/nanoTime) (* 1000000 (long ms)))]
       (loop [interrupted? false]
         (let [remaining (- deadline (System/nanoTime))]
           (if (pos? remaining)
             (do
               (LockSupport/parkNanos remaining)
               (recur (or interrupted? (Thread/interrupted))))
             (when interrupted?
               (.interrupt (Thread/currentThread)))))))))


(defn -noempty [<x] (m/eduction (remove hyperfiddle.incseq/empty-diff?) <x))
(e/defn  ^:no-doc Swallow-empty-diffs "Don't touch this" [x] (e/join (-noempty (e/pure x))))

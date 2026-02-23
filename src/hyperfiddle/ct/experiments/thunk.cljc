(ns hyperfiddle.ct.experiments.thunk
  "Model a delayed value with customizable caching levels.
  `(thunk (prn \"hello\") 1)` produces a Thunk object printing \"hello\" and returning 1 on deref.

  The only difference with clojure's delay is the ability to customize caching strategy.

  Caching levels are as follow:
  - ::none : no caching – default – equivalent to (fn [] (prn \"hello\") 1).
  - ::hard : full caching – equivalent to (delay (prn \"hello\") 1).
  - ::weak : weak reference caching – on deref, the thunk body is run, its
    result is cached and made available to GC collection. Subsequent derefs will
    return the cached value immediately, without running the thunk body. After the
    GC reclaims the cached result, next deref will rerun body and cache the result
    again.
  - ::soft : soft reference caching – same behavior as ::weak after a GC clear. Akin to a LRU cache. JVM only.

  Two-layer caching design:

  Construction time — explicit parameter on thunk macro:
  (thunk body)            ; defaults to ::none
  (thunk ::weak body)     ; explicit strategy

  Call-time override — ambient, affects all derefs in scope:
  (binding [*cache-override* ::none]
    (deref t1)
    (deref t2))           ; both bypass cache

  Construction is always local and known — an explicit parameter eliminates
  lazy-seq hazards and test friction. Call-time override IS inherently ambient —
  a dynamic var is the right tool because the override applies to whatever thunks
  are forced in scope.

  A thunk t can be forced in three equivalent ways:
  - (deref t)
  - (force-thunk t) - like clojure.core/force, will force thunks and non-thunk values will pass through.
  - (extract t) - alias for deref, Thunk is a comonad. Note comonad extract is
  supposed to be pure. It is up to the user to write pure code in thunks. When
  working with thunks as comonads, one should always assume the caching strategy
  is set to ::none.

  Thunk implements IFn (0-arity calls extract), so comp and fmap both
  compute (f (extract t)) lazily — they differ only in return type:
  - (comp inc (thunk 1))  ; returns a plain fn, thunk's own caching preserved
  - (fmap inc (thunk 1))  ; returns a new Thunk, caching the whole pipeline
  "
  (:require [hyperfiddle.ct.functor :refer [Functor fmap ->Identity]]
            [hyperfiddle.ct.semigroup :refer [Semigroup -<>]]
            [hyperfiddle.ct.monoid :refer [Monoid mempty]]
            [hyperfiddle.ct.applicative :refer [Applicative]]
            [hyperfiddle.ct.monad :refer [Monad]]
            [hyperfiddle.ct.comonad :refer [Comonad extract duplicate]]
            [hyperfiddle.rcf :refer [tests]]
            )
  #?(:clj (:import [java.lang.ref SoftReference WeakReference]))
  #?(:cljs (:require-macros [hyperfiddle.ct.experiments.thunk :refer [thunk]])))

(def ^:dynamic *cache-override* nil)

(def CACHE-STRATEGIES
  #?(:clj
     {::hard [identity identity]
      ::soft [#(SoftReference. %) #(.get %)]
      ::weak [#(WeakReference. %) #(.get %)]
      ::none [(constantly nil) identity]}
     :cljs
     {::hard [identity identity]
      ::weak [#(js/WeakRef. %) #(.deref %)]
      ::none [(constantly nil) identity]}
     ))

(defn cache-reference [make-ref getter]
  (^:once fn* [object]
    (let [ref (make-ref object)]
      #?(:clj (reify clojure.lang.IDeref
                (deref [_] (getter ref)))
         :cljs (reify IDeref
                 (-deref [_] (getter ref)))))))

(defn get-cache-strategy [strategy]
  (apply cache-reference (get CACHE-STRATEGIES strategy)))

(declare thunk*)

(deftype Thunk [f cache-strategy ^objects !cache] ; same as clojure.lang.Delay + customizable cache.
  Functor
  (-fmap [_ g] (thunk* cache-strategy (fmap g f)))

  Semigroup
  (-<> [a b] (thunk* cache-strategy #(-<> (deref a) (deref b))))
  Monoid
  (mempty [a] (fmap mempty a))

  Applicative
  (-pure [_ x] (thunk* cache-strategy (fn [] x)))
  (-fapply [Af Aa] (thunk* cache-strategy #((deref Af) (deref Aa))))

  Monad
  (-bind [ma a->mb] (thunk* cache-strategy #(deref (a->mb (deref ma)))))

  Comonad
  (-extract [_]
    (if (some? *cache-override*)
      (f) ; call-time override: bypass shared cache, always recompute
      (let [cache-fn (get-cache-strategy cache-strategy)
            compute! (fn []
                       (let [computed (->Identity (f))]
                         (aset !cache 0 (cache-fn computed))
                         computed))
            locking-compute! (fn []
                               (if (= ::none cache-strategy)
                                 (compute!)
                                 (locking !cache (compute!))))]
        (extract
          (if-let [ref (aget !cache 0)]
            (if-some [object (deref ref)]
              object
              (locking-compute!))
            (locking-compute!))))))
  (-duplicate [_] (Thunk. (fn [] f) cache-strategy (object-array 1)))
  (-extend [this k] (fmap k (duplicate this)))

  #?(:clj clojure.lang.IDeref)
  #?(:clj (deref [this] (extract this)))
  #?(:cljs IDeref)
  #?(:cljs (-deref [this] (extract this)))

  #?(:clj clojure.lang.IPending)
  #?(:clj (isRealized [_] (some? (some-> (aget !cache 0) deref))))
  #?(:cljs IPending)
  #?(:cljs (-realized? [_] (some? (some-> (aget !cache 0) deref))))

  #?(:clj clojure.lang.IFn :cljs IFn)
  (#?(:clj invoke :cljs -invoke) [this] (extract this))
  )

(defn thunk* [cache-strategy f]
  (assert (contains? CACHE-STRATEGIES cache-strategy))
  (->Thunk f cache-strategy (object-array 1)))

(defn thunk? [object] (instance? Thunk object))
(defmacro thunk [& body]
  (let [[strategy & body] (if (keyword? (first body))
                            body
                            (cons ::none body))]
    `(thunk* ~strategy (fn [] ~@body))))

(defn force-thunk [object] (if (thunk? object) (extract object) object))

(tests
  "Thunk as IFn - 0-arity invocation extracts value"
  (let [t (thunk 42)]
    (t) := 42
    @t := 42
    (extract t) := 42)

  "comp vs fmap - same value, different return type (fn vs Thunk)"
  (let [t (thunk 1)
        composed (comp inc t)
        fmapped (fmap inc t)]
    (fn? composed) := true
    (composed) := 2
    (thunk? fmapped) := true
    @fmapped := 2)

  "both comp and fmap are lazy — neither forces until demanded"
  (let [counter (atom 0)
        t (thunk (swap! counter inc) 1)
        _composed (comp inc t)
        _fmapped (fmap inc t)]
    @counter := 0)

  "explicit strategy keyword"
  (let [t (thunk ::weak 42)]
    @t := 42)

  "*cache-override* bypasses ::hard cache — forces recomputation"
  (let [counter (atom 0)
        t (thunk ::hard (do (swap! counter inc) 42))]
    @t := 42
    @counter := 1
    @t := 42
    @counter := 1  ; ::hard caches, body runs once
    (binding [*cache-override* ::none]
      @t := 42
      @counter := 2  ; override bypasses cache, body reruns
      @t := 42
      @counter := 3)) ; still bypassing, reruns again

  "*cache-override* is scoped — after binding ends, construction-time cache resumes"
  (let [counter (atom 0)
        t (thunk ::hard (do (swap! counter inc) 42))]
    @t := 42
    @counter := 1  ; first deref, cached
    (binding [*cache-override* ::none]
      @t := 42
      @counter := 2) ; override, recomputed
    ;; outside binding, back to ::hard cache — original cached value still there
    @t := 42
    @counter := 2) ; cache hit from construction-time ::hard

  "*cache-override* affects all thunks in scope regardless of their strategy"
  (let [c1 (atom 0)
        c2 (atom 0)
        t1 (thunk ::hard (do (swap! c1 inc) 1))
        t2 (thunk ::weak (do (swap! c2 inc) 2))]
    (binding [*cache-override* ::none]
      @t1 := 1
      @t1 := 1
      @c1 := 2    ; both derefs recomputed
      @t2 := 2
      @t2 := 2
      @c2 := 2))  ; both derefs recomputed
  )

(comment
  "No caching, equivalent to plain 0-arg fn"
  (def _t (thunk (System/currentTimeMillis)))
  _t
  @_t
  @_t ; not=
  )

(comment
  "Weak caching, clears on GC"
  (def _t (thunk ::weak (System/currentTimeMillis)))
  _t
  @_t
  @_t ; =

  nil ; eval three times to clear REPL *1 *2 *3

  (System/gc)
  @_t ; not=
  )

(comment
  (thunk "hello")
  (thunk? *1)
  (extract *2)
  (fmap #(str % " world") *3)
  (extract *1)
  (*2)
  )

(ns hyperfiddle.navigator6.sorting)

(defmulti comparable type)
(defmethod comparable :default [x] x)
#?(:clj (defmethod comparable java.lang.Class [^Class x] (.getName x)))
(defmethod comparable #?(:clj clojure.lang.Symbol :cljs cljs.core.Symbol) [sym] (str sym))
(defmethod comparable #?(:clj clojure.lang.Keyword :cljs cljs.core.Keyword) [kw] (str (namespace kw) "/" (name kw))) ; drop ":"

#?(:clj
   (defn- ->sort-comparator [sort-spec]
     (debug-exceptions `->sort-comparator
       (let [[[k asc?]] sort-spec
             order (if asc? neg? pos?)   ; TODO support multi-sort
             compare-fn (fn [a b]
                          (cond
                            (nil? a) false
                            (nil? b) true
                            :else (order (compare a b))))]
         (when k
           (fn [a b]
             (debug-exceptions `->sort-comparator-inner
               (compare-fn (comparable (get a k)) (comparable (get b k))))))))))

(comment
  (sort (->sort-comparator [[:k true]]) [{:k 1} {:k 0} {:k 2}])
  (sort (->sort-comparator [[:k false]]) [{:k 1} {:k 0} {:k 2}])
  )

(defn -fsym [sexpr]
  (when (seq? sexpr)
    (if (= 'quote (first sexpr))
      (-fsym (second sexpr))
      (first sexpr))))

(defn sexpr-comparator [a b]
  (debug-exceptions `sexpr-comparator
    (cond
      (nil? a) -1
      (nil? b) 1
      (and (keyword? a) (symbol? b)) (compare a (keyword b))
      (and (symbol? a) (keyword? b)) (compare (keyword a) b)
      (seq? a) (sexpr-comparator (-fsym a) b)
      (seq? b) (sexpr-comparator a (-fsym b))
      :else (compare a b))))

(defn ?sort-by [keyfn v*]
  (try (debug-exceptions `?sort-by
         (sort-by keyfn sexpr-comparator v*))
       (catch #?(:clj Throwable :cljs :default) e
         (let [log-message (str "Skipped sort: some elements are not comparable." (type e) " " (ex-message e))]
           #?(:clj (log/info log-message)
              :cljs (.info js/console log-message)))
         v*)))


#?(:clj
   (defn- sort-children-by-ordered-keyset
     "Sort a map structure by keys. Static selected keys (from stepT) have
  priority and are laid out in order, as in an ordered map. Extra keys comes later and are sorted by
  `comparator`. Return a seq of map entries."
     [stepT childrenT]
     {:pre [(= ::ana/pull (::ana/type (hfql/metas stepT))) (every? hfql/symbolic-edge childrenT)]
      :post [(every? hfql/symbolic-edge childrenT) #_(= (count %) (count childrenT))]}
     (let [static-keys (distinct (map hfql/symbolic-edge childrenT))
           static-keyset (set static-keys)
           [static-children dynamic-children] (contrib.data/group-by-pred (comp boolean static-keyset hfql/symbolic-edge) childrenT)
           static-map (into {} (map (juxt hfql/symbolic-edge identity)) static-children)]
       #_(prn {'static-keys static-keys
             'static-kvs static-children
             'dynamic-kvs dynamic-children})
       (mappend (keep (fn [pullspec-key] (get static-map pullspec-key)) static-keys)
         (?sort-by hfql/symbolic-edge (remove (comp some? static-keyset hfql/symbolic-edge) dynamic-children))))))

(defn toggle-column-sort [sort-spec for-col]
  (when-some [[col asc?] (first sort-spec)]
    [[for-col (if (= col for-col) (not asc?) true)]]))


;; For reference
;; #?(:clj
;;    (defn- eager-pull-search-sort-BIG-O-N-SQUARE []
;;      ;; (prn `eager-pull-search-sort-BIG-O-N-SQUARE edge hfql-bindings search sort-spec)
;;      (let [do-filter (prefix-filterer (fn [needle pair] (search-matches (snd pair) needle)))]
;;        (fn [columnsT rowsT hfql-bindings search sort-spec]
;;          [columnsT
;;           ;; (def _args [rowsT columnsT hfql-bindings search sort-spec])
;;           (try
;;             (debug-exceptions `eager-pull-search-sort-BIG-O-N-SQUARE
;;               ;; (Thread/sleep 3000)
;;               (if (not-empty search)
;;                 (dedupe
;;                   (for [rowT rowsT
;;                         columnT columnsT
;;                         :let [cell-value (hfql/nav (hfql/seed (hfql/scope rowT) columnT))]
;;                         :when (search-matches cell-value search)]
;;                     rowT))
;;                 rowsT)
;;               #_(let [pulled (sequence
;;                                (eduction
;;                                  (map (join pair)) ; duplicate arg and apply both to ->Pair
;;                                  (map (fmap pull)) ; pull right side of pair
;;                                  (map (fmap extract)) ; extract right side of pair
;;                                  rowsT))
;;                       filtered (if (not-empty search)
;;                                  (do-filter search pulled)
;;                                  pulled)
;;                       sorted (if-some [sorter (and (not-empty sort-spec) (->sort-comparator sort-spec))]
;;                                (try (sort-by snd sorter filtered) (catch Throwable _ filtered))
;;                                filtered)]
;;                   (with-meta (map fst sorted) (meta (extract rowsT)))))
;;             (catch Throwable t (prn 'eager-pull-search-sort (ex-message t) #_t) []))]))))
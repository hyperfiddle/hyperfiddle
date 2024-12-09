(ns hyperfiddle.ui.accordion
  (:require
   [clojure.set :as set]
   [heroicons.electric3.v24.outline :as i]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.ui.disclosure :as dis]))

(defmacro accordion [& body]
  `(dom/div (dom/props {:class ["accordion divide-y divide-gray-100"
                                "w-full bg-white border rounded-md px-2"]})
     ~@body))

(e/declare disabled?) ; default false

(defmacro entry [props & body]
  `(let [props# ~props]
     (dis/disclosure (set/rename-keys props# {::open? ::dis/open?, ::preload? ::dis/preload?, ::keep-loaded? ::dis/keep-loaded?
                                              ::disabled? ::dis/disabled?}) ; forward props
       (dom/props {:class "py-2 break-inside-avoid"})
       (binding [disabled? (::disabled? props# false)]
         ~@body))))

(defmacro header [& body]
  `(dis/button (dom/props {:class "flex gap-2 w-full items-center"})
     (when-not disabled?
       (if dis/open?
         (i/chevron-down (dom/props {:class "w-4 print:hidden"}))
         (i/chevron-right (dom/props {:class "w-4 print:hidden"}))))
     ~@body))

(defmacro body [& body]
  `(dis/panel {} (dom/props {:class "p-2"})
     ~@body))


(comment
  (accordion
    (entry {::open? true, ::keep-loaded? true}
      (header (dom/span (dom/text "Section 1")))
      (body (dom/p (dom/text "Content"))))
    (entry {::open? false, ::keep-loaded? true}
      (header (dom/span (dom/text "Section 2")))
      (body (dom/p (dom/text "Content")))))
  )

(ns hypercrud.ui.select
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.tx :as tx-util]
            [hypercrud.form.option :as option]
            [hypercrud.ui.form :as form]))


(defn select-boolean [entity {:keys [stage-tx!] {:keys [ident]} :field}]
  (let [value (get entity ident)
        props {;; normalize value for the dom - value is either nil, an :ident (keyword), or eid
               :value (if (nil? value) "" (str value))
               ;; reconstruct the typed value
               :on-change #(let [v (condp = (.-target.value %)
                                     "" nil
                                     "true" true
                                     "false" false)]
                            (stage-tx! (tx-util/update-entity-attr entity ident v)))}]
    [:div.value.editable-select {:key ident}
     [:span.select
      [:select props
       [:option {:key true :value "true"} "True"]
       [:option {:key false :value "false"} "False"]
       [:option {:key :nil :value ""} "--"]]]]))


(defn select* [entity {:keys [expanded-cur forms graph navigate-cmp stage-tx!]
                       {:keys [ident options]} :field} edit-element]
  (let [value (get entity ident)
        temp-id! hc/*temp-id!*
        props {;; normalize value for the dom - value is either nil, an :ident (keyword), or eid
               :value (cond
                        (nil? value) ""
                        (tx-util/tempid? value) "create-new"
                        :else (str value))

               ;; reconstruct the typed value
               :on-change #(do
                            (let [select-value (.-target.value %)
                                  eid (cond
                                        (= "" select-value) nil
                                        (= "create-new" select-value) (temp-id!)
                                        :else-hc-select-option-node (option/parse-string options select-value))]
                              ;reset the cursor before change! otherwise npe when trying to render
                              ;todo these both set the same cursor, and should be atomic
                              (reset! expanded-cur (if (= "create-new" select-value) {} nil))
                              (stage-tx! (tx-util/update-entity-attr entity ident eid))
                              ;; and also add our new guy to the option list (for all combos)
                              ))}
        create-new? (some-> value tx-util/tempid?)
        show-form? (or (not= nil @expanded-cur) create-new?)]

    [:div.value.editable-select {:key (option/get-key options)}
     (if (and (option/editable? options entity) (not show-form?))
       edit-element)
     [:span.select
      [:select props (-> (->> (option/get-option-records options graph entity)
                              (sort-by #(get % (option/label-prop options)))
                              (mapv (fn [entity]
                                      (let [eid (:db/id entity)]
                                        ^{:key eid}
                                        [:option {:value (option/to-string options entity)} (str (get entity (option/label-prop options)))]))))
                         (concat
                           (if (option/create-new? options entity)
                             [[:option {:key :create-new :value "create-new"} "Create New"]]
                             [])
                           [[:option {:key :blank :value ""} "--"]]))]]
     (if show-form?
       ;; TODO branch the client in create-new case
       [form/form graph value forms (option/get-form-id options entity) expanded-cur stage-tx! navigate-cmp])]))

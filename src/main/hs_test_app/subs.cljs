(ns hs-test-app.subs
  (:require [re-frame.core :refer [reg-sub subscribe]]
            [clojure.string :as str]))

(reg-sub
 :current-route
 :-> :route)

(rf/reg-sub
 ::patients
(reg-sub
(reg-sub
(reg-sub
(reg-sub
 :patients
 :-> :patients)

(reg-sub
 :patients-formatted
 (fn [_ _]
   (subscribe [:patients]))
 (fn [patients _]
   (map (fn [{:keys [first_name last_name] :as patient}]
          (-> patient
              (dissoc :first_name :last_name)
              (assoc :name (str first_name " " last_name))
              (update :gender #(if (true? %) "Male" "Female"))
              (update :birth #(-> (js/Intl.DateTimeFormat.
                                   "en-US"
                                   #js {:year "numeric"
                                        :month "short"
                                        :day "2-digit"})
                                  (.format (js/Date. %))))))
        patients)))

(reg-sub
 :patient-in-edit
 :-> :patient-in-edit)

(reg-sub
 :form
 (fn [db [_ form-id]]
   (get-in db [:form form-id] [])))

(reg-sub
 :form-submitting?
 (fn [db [_ form-id]]
   (get-in db [:form-submitting? form-id] false)))

(reg-sub
 :form-errors
 (fn [db [_ form-id]]
   (get-in db [:form-errors form-id] [])))

(reg-sub
 :has-form-errors?
 (fn [[_ form-id] _]
   (subscribe [:form-errors form-id]))
 (fn [errors _]
   (boolean (seq errors))))

;(reg-sub
; :form-field-value
; (fn [db [_ form-id field-id]]
;   (get-in db [:form form-id field-id] "")))
;; TODO
(reg-sub
 :form-field-value
 (fn [[_ form-id] _]
   (subscribe [:form form-id]))
 (fn [form [_ _ field-id]]
   (get form field-id "")))

(reg-sub
 :form-date-value
 (fn [[_ form-id field-id] _]
   (subscribe [:form-field-value form-id field-id]))
 (fn [v [_ _ field-id]]
   (let [[year month day] (if (empty? v)
                            ["" "" ""]
                            (str/split v #"-"))]
     {:year year
      :month month
      :day day
      :year-id (-> field-id name (str "-y") keyword)
      :month-id (-> field-id name (str "-m") keyword)
      :day-id (-> field-id name (str "-m") keyword)})))

(reg-sub
 :form-field-error?
 (fn [[_ form-id] _]
   (subscribe [:form-errors form-id]))
 (fn [errors [_ _ field-id]]
   (let [field-errors (filterv #(= (:field %) field-id) errors)]
     (some? (seq field-errors)))))

(reg-sub
 :form-field-error-message
 (fn [[_ form-id] _]
   (subscribe [:form-errors form-id]))
 (fn [errors [_ _ field-id]]
   (let [field-errors (filterv #(= (:field %) field-id) errors)]
     (-> field-errors
         first
         :message))))

(reg-sub
 :array-form-field-value
 (fn [db [_ form-id index field-id]]
   (get-in db [:form form-id index field-id] "")))

(reg-sub
 :array-form-indexed
 (fn [[_ form-id] _]
   (subscribe [:form form-id]))
 (fn [form _]
   (map-indexed
    (fn [index m]
      (let [id-map (reduce (fn [acc k]
                             (assoc acc
                                    (-> k name (str "-id") keyword)
                                    (-> k name (str "-" index) keyword)))
                           {}
                           (keys m))]
        (merge m {:index index
                  :uuid (random-uuid)} id-map)))
    form)))

(reg-sub
 :array-form-date-value
 (fn [[_ form-id index field-id] _]
   (subscribe [:array-form-field-value form-id index field-id]))
 (fn [v [_ _ _ field-id]]
   (let [[year month day] (if (empty? v)
                            ["" "" ""]
                            (str/split v #"-"))]
     {:year year
      :month month
      :day day
      :year-field-name (-> field-id name (str "-y") keyword)
      :month-field-name (-> field-id name (str "-m") keyword)
      :day-field-name (-> field-id name (str "-m") keyword)})))

(reg-sub
 :array-form-field-error?
 (fn [[_ form-id] _]
   (subscribe [:form-errors form-id]))
 (fn [array-form-errors [_ _ index field-id]]
   (let [errors (->> array-form-errors
                     (filterv #(= (:index %) index))
                     (filterv #(= (:field %) field-id)))]
     (some? (seq errors)))))

(reg-sub
 :array-form-field-error-message
 (fn [[_ form-id] _]
   (subscribe [:form-errors form-id]))
 (fn [array-form-errors [_ _ index field-id]]
   (let [errors (->> array-form-errors
                     (filterv #(= (:index %) index))
                     (filterv #(= (:field %) field-id)))]
     (-> errors
         first
         :message))))

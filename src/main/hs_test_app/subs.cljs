(ns hs-test-app.subs
  (:require [re-frame.core :as rf]
            [clojure.string :as str]))

(rf/reg-sub
 ::current-route
 :-> :route)

(rf/reg-sub
 ::patients
 :-> :patients)

(rf/reg-sub
 ::patients-formatted
 ;:<- ::patients
 (fn [_ _]
   (rf/subscribe [::patients]))
 (fn [patients _]
   (map (fn [{:keys [first_name last_name] :as patient}]
          (-> patient
              (dissoc :first_name :last_name)
              (assoc :name (str first_name " " last_name))))
        patients)))

(rf/reg-sub
 ::patient-in-edit
 :-> :patient-in-edit)

(rf/reg-sub
 ::form
 (fn [db [_ form-id]]
   (get-in db [:form form-id] [])))

(rf/reg-sub
 ::form-submitting?
 (fn [db [_ form-id]]
   (get-in db [:form-submitting? form-id] false)))

(rf/reg-sub
 ::form-errors
 (fn [db [_ form-id]]
   (get-in db [:form-errors form-id] [])))

(rf/reg-sub
 ::has-form-errors?
 (fn [[_ form-id] _]
   (rf/subscribe [::form-errors form-id]))
 (fn [errors _]
   (boolean (seq errors))))

(rf/reg-sub
 ::form-field-value
 (fn [db [_ form-id field-id]]
   (get-in db [:form form-id field-id] "")))

(rf/reg-sub
 ::form-date-value
 (fn [[_ form-id field-id] _]
   (rf/subscribe [::form-field-value form-id field-id]))
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

(rf/reg-sub
 ::form-field-error?
 (fn [[_ form-id] _]
   (rf/subscribe [::form-errors form-id]))
 (fn [errors [_ _ field-id]]
   (let [field-errors (filterv #(= (:field %) field-id) errors)]
     (boolean (seq field-errors)))))
     (some? (seq field-errors)))))

(rf/reg-sub
 ::form-field-error-message
 (fn [[_ form-id] _]
   (rf/subscribe [::form-errors form-id]))
 (fn [errors [_ _ field-id]]
   (let [field-errors (filterv #(= (:field %) field-id) errors)]
     (-> field-errors
         first
         :message))))

(rf/reg-sub
 ::array-form-field-value
 (fn [db [_ form-id index field-id]]
   (get-in db [:form form-id index field-id] "")))

(rf/reg-sub
 ::array-form-indexed
 (fn [[_ form-id] _]
   (rf/subscribe [::form form-id]))
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

(rf/reg-sub
 ::array-form-date-value
 (fn [[_ form-id index field-id] _]
   (rf/subscribe [::array-form-field-value form-id index field-id]))
 (fn [v [_ _ _ field-id]]
   (let [[year month day] (if (empty? v)
                            ["" "" ""]
                            (str/split v #"-"))]
     {:year year
      :month month
      :day day
      :year-id (-> field-id name (str "-y") keyword)
      :month-id (-> field-id name (str "-m") keyword)
      :day-id (-> field-id name (str "-m") keyword)})))

(rf/reg-sub
 ::array-form-field-error?
 (fn [[_ form-id] _]
   (rf/subscribe [::form-errors form-id]))
 (fn [array-form-errors [_ _ index field-id]]
   (let [errors (get array-form-errors index)
         field-errors (filterv #(= (:field %) field-id) errors)]
     (some? (seq field-errors)))))

(rf/reg-sub
 ::array-form-field-error-message
 (fn [[_ form-id] _]
   (rf/subscribe [::form-errors form-id]))
 (fn [array-form-errors [_ _ index field-id]]
   (let [errors (get array-form-errors index)
         field-errors (filterv #(= (:field %) field-id) errors)]
     (-> field-errors
         first
         :message))))

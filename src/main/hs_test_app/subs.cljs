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
 ::form-control-value
 (fn [db [_ form-id control-id]]
   (get-in db [:form form-id control-id] "")))
 ::patient-in-edit
 :-> :patient-in-edit)

(rf/reg-sub
 ::form
 (fn [db [_ form-id]]
   (get-in db [:form form-id] [])))

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

(ns hs-test-app.views
  (:require [re-frame.core :as rf]
            ;[fork.re-frame :as fork]
            [hs-test-app.events :as events]
            [hs-test-app.subs :as subs]))

;(defn patient-reg-form [props]
;  [fork/form (merge {:prevent-default? true
;                     :clean-on-unmount? true}
;                    props)
;   (fn [{:keys [values
;                form-id
;                handle-change
;                handle-blur
;                submitting?
;                handle-submit]}]
;     [:form {:id form-id
;             :on-submit handle-submit}
;      [:label "health insurance number"
;       [:input {:name "health_insurance_number"
;                :type "text"
;                :value (values "health_insurance_number")
;                :on-change handle-change
;                :on-blur handle-blur}]]
;      [:label "first name"
;       [:input {:name "first_name"
;                :type "text"
;                :value (values "first_name")
;                :on-change handle-change
;                :on-blur handle-blur}]]
;      [:label "last name"
;       [:input {:name "last_name"
;                :type "text"
;                :value (values "last_name")
;                :on-change handle-change
;                :on-blur handle-blur}]]
;      [:label "gender"
;       [:select {:name "gender"
;                 :value (values "gender")
;                 :on-change handle-change
;                 :on-blur handle-blur}
;        [:option {:value true} "male"]
;        [:option {:value false} "female"]]]
;      [:label "birth"
;       [:input {:name "birth"
;                :type "date"
;                :value (values "birth")
;                :on-change handle-change
;                :on-blur handle-blur}]]
;      [:label "address"
;       [:input {:name "address"
;                :type "text"
;                :value (values "address")
;                :on-change handle-change
;                :on-blur handle-blur}]]
;      [:button {:type "submit"
;                :disabled submitting?}
;       "Save"]])])
(defn text-input [form-id control-id label]
  (let [value (rf/subscribe [::subs/form-control-value form-id control-id])]
    [:label label
     [:input {:name control-id
              :type "text"
              :value @value
              :on-change #(rf/dispatch
                           [::events/update-form {:form-id form-id
                                                  :control-id control-id
                                                  :value (-> %
                                                             .-target
                                                             .-value)}])
                ;@TODO :on-blur handle-blur
              }]]))
(def text-input-patient-reg
  (partial text-input :patient-reg-form))

(defn select-picker [form-id control-id label option-label-to-value]
  (let [value (rf/subscribe [::subs/form-control-value form-id control-id])]
    [:label label
     [:select {:name control-id
               :value @value
               :on-change #(rf/dispatch
                            [::events/update-form {:form-id form-id
                                                   :control-id control-id
                                                   :value (-> %
                                                              .-target
                                                              .-value)}])
               ;@TODO :on-blur
               }
      [:option {:value ""} "Please select"]
      (map (fn [o]
             [:option {:key (:label o) :value (:value o)} (:label o)])
           option-label-to-value)]]))
(def select-picker-patient-reg
  (partial select-picker :patient-reg-form))

(defn patient-reg-form [handle-submit]
  [:form {:id :patient-reg-form
          :on-submit handle-submit}
   (text-input-patient-reg :first_name "first name")
   (text-input-patient-reg :last_name "last name")
   (select-picker-patient-reg :gender "gender" [{:label "Male" :value true}
                                                {:label "Female" :value false}])
   (text-input-patient-reg :birth "birth")
   (text-input-patient-reg :address "address")
   (text-input-patient-reg :health_insurance_number "health insurance number")
   [:button {:type "submit"
             ;@TODO :disabled submitting?
             }
    "Save"]])

(defmulti view :handler)

(defmethod view ::list [_]
  [:div "Patients"
   [:input]
   [:ul
    [:li
     [:span "name"]
     [:span "gender"]
     [:span "birth"]
     [:span "address"]
     [:span "health insurance number"]]
    (map (fn [{:keys
               [id
                first_name
                last_name
                gender
                birth
                address
                health_insurance_number]}]
           [:li {:key health_insurance_number
                 :on-click #(rf/dispatch [::events/navigate
                                          ::edit
                                          {:id id}])}
            [:span (str first_name " " last_name)]
            [:span gender]
            [:span birth]
            [:span address]
            [:span health_insurance_number]])

         @(rf/subscribe [::subs/patients]))]])

;(defmethod view ::edit []
;  [:div "Edit Patient Information"
;   (when-let [{:keys [id
;                      first_name
;                      last_name
;                      gender
;                      birth
;                      address
;                      health_insurance_number]}
;              @(rf/subscribe [::subs/patient-in-edit])]
;     [patient-reg-form {:values {"first_name" first_name
;                                 "last_name" last_name
;                                 "gender" gender
;                                 "birth" birth
;                                 "address" address
;                                 "health_insurance_number" health_insurance_number}
;                        :form-id "edit-patient"
;                        :on-submit #(rf/dispatch [::events/update-patient id %])}])])
(defmethod view ::edit []
  [:div "Edit Patient Information"
   (let [{:keys [id]} @(rf/subscribe [::subs/patient-in-edit])
         data @(rf/subscribe [::subs/form :patient-reg-form])]
     [patient-reg-form  (fn [event]
                          (.preventDefault event)
                          (rf/dispatch [::events/update-patient id {:values data}]))])])

(defmethod view ::create []
  [:div "Create New Patient"
   [patient-reg-form {:on-submit #(rf/dispatch [::events/create-patient %])}]])

(defn main-panel []
  [:div "Health Samurai Test App"
   [view @(rf/subscribe [::subs/current-route])]])

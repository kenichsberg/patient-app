(ns hs-test-app.views
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [cljs-time.format :as fmt]
            [hs-test-app.events :as events]
            [hs-test-app.subs :as subs]))

(def formatter (fmt/formatter "yyyy-MM-dd"))
(def get-year (fmt/formatter "yyyy"))
(def get-month (fmt/formatter "MM"))
(def get-day (fmt/formatter "dd"))

(defn concat-keyword [k string]
  (-> k
      (name)
      (str string)
      (keyword)))

(defn text-input [on-change control-id label default-value placeholder]
  [:label label
   [:input {:name control-id
            :type "text"
            :default-value default-value
            :placeholder placeholder
            :on-change on-change
                ;@TODO :on-blur handle-blur
            }]])

(defn select-picker [on-change control-id label default-value options]
  [:label label
   [:select {:name control-id
             :default-value default-value
             :on-change on-change
               ;@TODO :on-blur
             }
    [:option {:value ""} "Please select"]
    (map (fn [o]
           [:option {:key (:label o) :value (:value o)} (:label o)])
         options)]])

(def month-options
  [{:label "Jan" :value "01"}
   {:label "Feb" :value "02"}
   {:label "Mar" :value "03"}
   {:label "Apr" :value "04"}
   {:label "May" :value "05"}
   {:label "Jun" :value "06"}
   {:label "Jul" :value "07"}
   {:label "Aug" :value "08"}
   {:label "Sep" :value "09"}
   {:label "Oct" :value "10"}
   {:label "Nov" :value "11"}
   {:label "Dec" :value "12"}])

(comment
  (def d (fmt/parse formatter "1940-11-10"))
  (def f (fmt/formatter "yyyy-MM-dd"))
  (def get-day (fmt/formatter "dd"))
  (fmt/unparse get-day
               (fmt/parse f "1940-11-10"))
  (fmt/unparse get-day
               (fmt/parse formatter "1999-01-01")))

(defn date-input [on-change control-id label default-value]
  ;(when-let [date (fmt/parse formatter default-value)]
  ;(when-let [date (js/Date. default-value)]
  (let [[year month day] (str/split default-value #"-")]
    [:<>
     [:label label]
     [text-input
      (partial on-change control-id :day)
      (concat-keyword control-id "-day")
      "Day"
      (if (nil? day) "" day)
      "DD"]
     [select-picker
      (partial on-change control-id :month)
      (concat-keyword control-id "-month")
      "Month"
      (if (nil? month) "" month)
      month-options]
     [text-input
      (partial on-change control-id :year)
      (concat-keyword control-id "-year")
      "Year"
      (if (nil? year) "" year)
      "YYYY"]]))

(defn on-change-for-reg-form [control-id event]
  (rf/dispatch
   [::events/update-form {:form-id :patient-reg-form
                          :control-id control-id
                          :value (-> event
                                     .-target
                                     .-value)}]))

(defn on-change-for-filter-form [index control-id event]
  (rf/dispatch
   [::events/update-filter-form {:form-id :patient-filter-form
                                 :index index
                                 :control-id control-id
                                 :value (-> event
                                            .-target
                                            .-value)}]))

(defn on-change-date-for-reg-form [control-id target event]
  (rf/dispatch
   [::events/update-form-date {:form-id :patient-reg-form
                               :control-id control-id
                               :target target
                               :value (-> event
                                          .-target
                                          .-value)}]))

(def options-for-gender
  [{:label "Male" :value true}
   {:label "Female" :value false}])

(def options-for-patient-filter
  [{:label "first name" :value "first_name"}
   {:label "last name" :value "last_name"}
   {:label "gender" :value "gender"}
   {:label "birth" :value "birth"}
   {:label "address" :value "address"}
   {:label "health_insurance_number" :value "health_insurance_number"}])

(defn patient-reg-form [handle-submit]
  [:form {:id :patient-reg-form
          :on-submit handle-submit}
   [text-input
    (partial on-change-for-reg-form :first_name)
    :first_name
    "first name"
    @(rf/subscribe [::subs/form-control-value :patient-reg-form :first_name])]
   [text-input
    (partial on-change-for-reg-form :last_name)
    :last_name
    "last name"
    @(rf/subscribe [::subs/form-control-value :patient-reg-form :last_name])]
   [select-picker
    (partial on-change-for-reg-form :gender)
    :gender
    "gender"
    @(rf/subscribe [::subs/form-control-value :patient-reg-form :gender])
    options-for-gender]
   [date-input
    on-change-date-for-reg-form
    :birth
    "birth"
    @(rf/subscribe [::subs/form-control-value :patient-reg-form :birth])]
   [text-input
    (partial on-change-for-reg-form :address)
    :address
    "address"
    @(rf/subscribe [::subs/form-control-value :patient-reg-form :address])]
   [text-input
    (partial on-change-for-reg-form :health_insurance_number)
    :health_insurance_number
    "health insurance number"
    @(rf/subscribe [::subs/form-control-value :patient-reg-form :health_insurance_number])]
   [:button {:type "submit"
             ;@TODO :disabled submitting?
             }
    "Save"]])

(defn date-filter-input [index operator value]
  [:<>
   [select-picker
    ;(@TODO)
    (partial on-change-for-filter-form index :operator)
    (keyword (str "operator-" index))
    "operator"
    operator
    [{:label "after" :value "gt"}
     {:label "before" :value "lt"}
     {:label "equal" :value "eq"}]]
   [date-input
    ;(@TODO)
    (partial on-change-for-filter-form index :value)
    (keyword (str "value-" index))
    "value"
    value]])

(defn conditional-input [index field operator value]
  (cond
    (nil? field) nil
    (= field "gender") [select-picker
                        (partial on-change-for-filter-form index :value)
                        (keyword (str "value-" index))
                        "value"
                        value
                        options-for-gender]
    (= field "birth") [date-filter-input index operator value]
    :else [text-input
           (partial on-change-for-filter-form index :value)
           (keyword (str "value-" index))
           "value"
           value]))

(defn patient-filter-form []
  (let [values (:vals @(rf/subscribe [::subs/form :patient-filter-form]))]
    [:form {:id :patient-filter-form}
     ;@TODO inside map, we can't know what index is there.
     (map
      (fn [{:keys [index field operator value]}]
        [:fieldset {:key index}
         [select-picker
          (partial on-change-for-filter-form index :field)
          (keyword (str "field-" index))
          "field"
          value
          options-for-patient-filter]
         [conditional-input index field operator value]
         [:button {:type "button"
                   :on-click #(rf/dispatch [::events/delete-index-from-vec-form
                                            {:form-id :patient-filter-form
                                             :index index}])}
          "x"]])
      values)
     [:button {:type "button"
               :on-click #(rf/dispatch [::events/push-vec-form
                                        {:form-id :patient-filter-form}])}
      "Add Filter"]
     (when (not-empty values)
       [:button {:type "button"
                 :on-click #(rf/dispatch [::events/push-vec-form
                                          {:form-id :patient-filter-form}])}
        "Search"])]))

(defmulti view :handler)
(defmethod view ::list [_]
  [:div "Patients"
   [:input {:type "text"
            :name "keywords"
            :on-change #(rf/dispatch [::events/search (-> %
                                                          .-target
                                                          .-value)])}]
   [patient-filter-form]
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

(defmethod view ::edit []
  [:div "Edit Patient Information"
   (let [{:keys [id]} @(rf/subscribe [::subs/patient-in-edit])
         data @(rf/subscribe [::subs/form :patient-reg-form])]
     [patient-reg-form  (fn [event]
                          (.preventDefault event)
                          (rf/dispatch [::events/update-patient
                                        id
                                        {:values data}]))])])

(defmethod view ::create []
  [:div "Create New Patient"
   [patient-reg-form {:on-submit #(rf/dispatch [::events/create-patient %])}]])

(defn main-panel []
  [:div "Health Samurai Test App"
   [view @(rf/subscribe [::subs/current-route])]])

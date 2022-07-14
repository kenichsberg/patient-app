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

(def gender-options
  [{:label "Male" :value true}
   {:label "Female" :value false}])

(def filter-options
  [{:label "first name" :value "first_name"}
   {:label "last name" :value "last_name"}
   {:label "gender" :value "gender"}
   {:label "birth" :value "birth"}
   {:label "address" :value "address"}
   {:label "health_insurance_number" :value "health_insurance_number"}])

(defn modify-keyword [k string]
  (-> k
      (name)
      (str string)
      (keyword)))

;;@TODO arguments are to be hash-map
(defn text-input [on-change control-id label default-value placeholder]
  [:label label
   [:input {:name control-id
            :type "text"
            :default-value default-value
            :placeholder placeholder
            :on-change on-change
                ;@TODO :on-blur handle-blur
            }]])

;;@TODO arguments are to be hash-map
(defn select-picker [on-change
                     control-id
                     label
                     default-value
                     options
                     with-empty-option]
  [:label label
   [:select {:name control-id
             :default-value default-value
             :on-change on-change
               ;@TODO :on-blur
             }
    (when (not with-empty-option)
      [:option {:value ""} "Please select"])
    (map (fn [o]
           [:option {:key (:label o) :value (:value o)} (:label o)])
         options)]])

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
      (partial on-change :day)
      (modify-keyword control-id "-day")
      "Day"
      (if (nil? day) "" day)
      "DD"]
     [select-picker
      (partial on-change :month)
      (modify-keyword control-id "-month")
      "Month"
      (if (nil? month) "" month)
      month-options]
     [text-input
      (partial on-change :year)
      (modify-keyword control-id "-year")
      "Year"
      (if (nil? year) "" year)
      "YYYY"]]))

(defn reg-control [control-id event]
  (rf/dispatch
   [::events/update-control {:form-id :patient-reg-form
                             :control-id control-id
                             :value (-> event
                                        .-target
                                        .-value)}]))

(defn reg-dynamic-control [index control-id event]
  (rf/dispatch
   [::events/upsert-dynamic-control {:form-id :patient-filter-form
                                     :index index
                                     :control-id control-id
                                     :value (-> event
                                                .-target
                                                .-value)}]))

(defn reg-date-control [control-id target event]
  (rf/dispatch
   [::events/update-date-control {:form-id :patient-reg-form
                                  :control-id control-id
                                  :target target
                                  :value (-> event
                                             .-target
                                             .-value)}]))

(defn reg-dynamic-date-control [index control-id target event]
  (rf/dispatch
   [::events/upsert-dynamic-date-control {:form-id :patient-filter-form
                                          :index index
                                          :control-id control-id
                                          :target target
                                          :value (-> event
                                                     .-target
                                                     .-value)}]))

(defn patient-reg-form [handle-submit]
  [:form {:id :patient-reg-form
          :on-submit handle-submit}
   [text-input
    (partial reg-control :first_name)
    :first_name
    "first name"
    @(rf/subscribe [::subs/form-control-value :patient-reg-form :first_name])]
   [text-input
    (partial reg-control :last_name)
    :last_name
    "last name"
    @(rf/subscribe [::subs/form-control-value :patient-reg-form :last_name])]
   [select-picker
    (partial reg-control :gender)
    :gender
    "gender"
    @(rf/subscribe [::subs/form-control-value :patient-reg-form :gender])
    gender-options]
   [date-input
    (partial reg-date-control :birth)
    :birth
    "birth"
    @(rf/subscribe [::subs/form-control-value :patient-reg-form :birth])]
   [text-input
    (partial reg-control :address)
    :address
    "address"
    @(rf/subscribe [::subs/form-control-value :patient-reg-form :address])]
   [text-input
    (partial reg-control :health_insurance_number)
    :health_insurance_number
    "health insurance number"
    @(rf/subscribe [::subs/form-control-value :patient-reg-form :health_insurance_number])]
   [:button {:type "submit"
             ;@TODO :disabled submitting?
             }
    "Save"]])

(defn gender-filter-input [index operator value]
  [:<>
   [select-picker
    (partial reg-dynamic-control index :operator)
    (keyword (str "operator-" index))
    "operator"
    operator
    [{:label "equals" :value "eq"}]
    true]
   [select-picker
    (partial reg-dynamic-control index :value)
    (keyword (str "value-" index))
    "value"
    value
    gender-options]])

(defn date-filter-input [index operator value]
  [:<>
   [select-picker
    (partial reg-dynamic-control index :operator)
    (keyword (str "operator-" index))
    "operator"
    operator
    [{:label "after" :value "gt"}
     {:label "before" :value "lt"}
     {:label "equals" :value "eq"}]]
   [date-input
    (partial reg-dynamic-date-control index :value)
    (keyword (str "value-" index))
    "value"
    value]])

(defn text-filter-input [index operator value]
  [:<>
   [select-picker
    (partial reg-dynamic-control index :operator)
    (keyword (str "operator-" index))
    "operator"
    operator
    [{:label "equals" :value "eq"}
     {:label "contains" :value "cont"}]]
   [text-input
    (partial reg-dynamic-control index :value)
    (keyword (str "value-" index))
    "value"
    value]])

;(defn conditional-input [index field operator value]
;  (cond
;    (nil? field) nil
;    (= field "gender") [select-picker
;                        (partial reg-dynamic-control index :value)
;                        (keyword (str "value-" index))
;                        "value"
;                        value
;                        gender-options]
;    (= field "birth") [date-filter-input index operator value]
;    :else [text-input
;           (partial reg-dynamic-control index :value)
;           (keyword (str "value-" index))
;           "value"
;           value]))

(defn conditional-input [index field operator value]
  (cond
    (nil? field) nil
    (= field "gender") [gender-filter-input index operator value]
    (= field "birth") [date-filter-input index operator value]
    :else [text-filter-input index operator value]))

(defn patient-filter-form []
  (let [fieldsets @(rf/subscribe [::subs/form :patient-filter-form])]
    [:form {:id :patient-filter-form}
     (map-indexed
      (fn [index {:keys [field operator value]}]
        [:fieldset {:key index}
         [select-picker
          (partial reg-dynamic-control index :field)
          (keyword (str "field-" index))
          "field"
          value
          filter-options]
         [conditional-input index field operator value]
         [:button {:type "button"
                   :on-click #(rf/dispatch [::events/delete-dynamic-fieldset
                                            {:form-id :patient-filter-form
                                             :index index}])}
          "x"]])
      fieldsets)
     [:button {:type "button"
               :on-click #(rf/dispatch [::events/add-dynamic-fieldset
                                        {:form-id :patient-filter-form}])}
      "Add Filter"]
     (when (not-empty fieldsets)
       [:button {:type "button"
                 :on-click #(rf/dispatch [::events/search-patients
                                          {:filters fieldsets}])}
        "Search"])]))

;(defmulti view :handler)
;(defmulti view identity)
(defmulti view (fn [arg]
                 (println (str "view: " arg))
                 arg))

;(defmethod view ::list [_]
(defmethod view :default [_]
  [:div "Patients"
   [:input {:type "text"
            :name "keywords"
            :on-change #(rf/dispatch [::events/search-patients
                                      {:keywords (-> %
                                                     .-target
                                                     .-value)}])}]
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
                 :on-click #(rf/dispatch [::events/push-state
                                          :patients-edit
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

(ns hs-test-app.views
  (:require [re-frame.core :as rf]
            [hs-test-app.events :as events]
            [hs-test-app.subs :as subs]
            [hs-test-app.validation :as v]))

;;
;;
;; -------------------------------------------------
;; Maps for select-picker
;;
;; -------------------------------------------------
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
   {:label "health insurance number" :value "health_insurance_number"}])

;;
;;
;; -------------------------------------------------
;; A map for validation
;;
;; -------------------------------------------------
(defn valid-filter [m]
  (let [get-error (fn [{:keys [field operator value]}]
                    (cond
                      (empty? field) {:field :field
                                      :error-type :required}
                      (empty? operator) {:field :operator
                                         :error-type :required}
                      (empty? value) {:field :value
                                      :error-type :required}
                      (and (= field "birth")
                           (false? (:valid?
                                    (v/valid-date-string nil value)))) {:field :value
                                                                        :error-type :invalid-date}
                      :else nil))
        error (get-error m)
        message-map {:required (str (-> error :field name) " is required.")
                     :invalid-date "Invalid date."}]
    {:valid? (nil? error)
     :field (:field error)
     :message (get message-map (:error-type error))}))

(def validator-map
  {:patient-reg-form {:first_name [[v/required "first name"]]
                      :last_name [[v/required "last name"]]
                      :gender [[v/required "gender"]]
                      :birth [[v/required "birth"] [v/valid-date-string]]
                      :address [[v/required "address"]]
                      :health_insurance_number [[v/required "health insurance number"]
                                                [v/health-insurance-number]]}
   :patient-filter-form valid-filter})
;;
;;
;; -------------------------------------------------
;; Form input components
;;
;; -------------------------------------------------
(defn text-input [{:keys [field-id
                          label
                          default-value
                          placeholder
                          on-change
                          on-blur
                          error?
                          error-message]}]
  [:label label
   [:input {:name field-id
            :type "text"
            :default-value default-value
            :placeholder placeholder
            :on-change on-change
            :on-blur on-blur}]
   [:p {:style (if error? {} {:visibility "hidden"})}
    (or error-message "There are no errors.")]])

(defn select-picker [{:keys [field-id
                             label
                             default-value
                             options
                             without-empty-option
                             on-change
                             on-blur
                             error?
                             error-message]}]
  [:label label
   [:select {:name field-id
             :default-value default-value
             :on-change on-change
             :on-blur on-blur}
    (when (not without-empty-option)
      [:option {:value ""} "Please select"])
    (map (fn [o]
           [:option {:key (:label o) :value (:value o)} (:label o)])
         options)]
   [:p {:style (if error? {} {:visibility "hidden"})}
    (or error-message "There are no errors.")]])

(defn date-input [{:keys [label
                          year-id
                          month-id
                          day-id
                          year
                          month
                          day
                          on-change
                          error?
                          error-message]}]
  [:<>
   [:label label]
   [text-input
    {:name day-id
     :label "Day"
     :default-value day
     :placeholder "DD"
     :on-change (partial on-change :day)
     :error? error?}]
   [select-picker
    {:name month-id
     :label "Month"
     :default-value month
     :options month-options
     :on-change (partial on-change :month)
     :error? error?}]
   [text-input
    {:name year-id
     :label "Year"
     :default-value year
     :placeholder "YYYY"
     :on-change (partial on-change :year)
     :error? error?}]
   [:p {:style (if error? {} {:visibility "hidden"})}
    (or error-message "There are no errors.")]])

;;
;;
;; -------------------------------------------------
;; Functions for form input components
;;
;; -------------------------------------------------
(defn change-field
  ;; multi-ality function
  ([form-id
    validation-rules
    {:keys [field-type field-id]}
    event]                        ;; without date-unit
   (rf/dispatch
    [::events/on-change-field {:form-id form-id
                               :field-type field-type
                               :field-id field-id
                               :value (-> event
                                          .-target
                                          .-value)
                               :validation-rules validation-rules}]))

  ([form-id
    validation-rules
    {:keys [field-type field-id]}
    date-unit                     ;; with date-unit
    event]
   (rf/dispatch
    [::events/on-change-field {:form-id form-id
                               :field-type field-type
                               :field-id field-id
                               :date-unit date-unit
                               :value (-> event
                                          .-target
                                          .-value)
                               :validation-rules validation-rules}])))

(defn change-dynamic-field
  ;; multi-ality function
  ([form-id
    first-field
    validation-rules
    {:keys [field-type index field-id]}
    event]                        ;; without date-unit
   (rf/dispatch
    [::events/on-change-dynamic-field {:form-id form-id
                                       :first-field first-field
                                       :field-type field-type
                                       :index index
                                       :field-id field-id
                                       :value (-> event
                                                  .-target
                                                  .-value)
                                       :validation-rules validation-rules}]))
  ([form-id
    first-field
    validation-rules
    {:keys [field-type index field-id]}
    date-unit                     ;; with date-unit
    event]
   (rf/dispatch
    [::events/on-change-dynamic-field {:form-id form-id
                                       :first-field first-field
                                       :field-type field-type
                                       :index index
                                       :field-id field-id
                                       :date-unit date-unit
                                       :value (-> event
                                                  .-target
                                                  .-value)
                                       :validation-rules validation-rules}])))

(defn blur-field [form-id validation-rules]
  (rf/dispatch
   [::events/update-errors {:form-id form-id
                            :validation-rules validation-rules}]))

;;
;;
;; -------------------------------------------------
;; Form components
;;
;; -------------------------------------------------
(defn patient-reg-form [handle-submit]
  (let [validation-rules (:patient-reg-form validator-map)
        handle-change (partial change-field :patient-reg-form validation-rules)
        handle-blur (partial blur-field :patient-reg-form validation-rules)
        has-errors? @(rf/subscribe [::subs/has-form-errors? :patient-reg-form])
        submitting? @(rf/subscribe [::subs/form-submitting? :patient-reg-form])]
    [:form {:id :patient-reg-form
            :on-submit handle-submit}
     [text-input
      {:name :first_name
       :label "first name"
       :default-value @(rf/subscribe
                        [::subs/form-field-value :patient-reg-form :first_name])
       :on-change (partial handle-change {:field-type "text"
                                          :field-id :first_name})
       :on-blur handle-blur
       :error? @(rf/subscribe
                 [::subs/form-field-error? :patient-reg-form :first_name])
       :error-message @(rf/subscribe
                        [::subs/form-field-error-message :patient-reg-form :first_name])}]

     [text-input
      {:name :last_name
       :label "last name"
       :default-value @(rf/subscribe
                        [::subs/form-field-value :patient-reg-form :last_name])

       :on-change (partial handle-change {:form-type "text"
                                          :field-type :last_name})
       :on-blur handle-blur
       :error? @(rf/subscribe
                 [::subs/form-field-error? :patient-reg-form :last_name])
       :error-message @(rf/subscribe
                        [::subs/form-field-error-message :patient-reg-form :last_name])}]

     [select-picker
      {:name :gender
       :label "gender"
       :default-value @(rf/subscribe
                        [::subs/form-field-value :patient-reg-form :gender])
       :options gender-options
       :on-change (partial handle-change {:field-type "text"
                                          :field-id :gender})
       :on-blur handle-blur
       :error? @(rf/subscribe
                 [::subs/form-field-error? :patient-reg-form :gender])
       :error-message @(rf/subscribe
                        [::subs/form-field-error-message :patient-reg-form :gender])}]

     (let [{:keys [year
                   month
                   day
                   year-id
                   month-id
                   day-id]} @(rf/subscribe
                              [::subs/form-date-value :patient-reg-form :birth])]
       [date-input
        {:label "birth"
         :year-id year-id
         :month-id month-id
         :day-id day-id
         :year year
         :month month
         :day day
         :on-change (partial handle-change {:field-type "date"
                                            :field-id :birth})
         :on-blur handle-blur
         :error? @(rf/subscribe
                   [::subs/form-field-error? :patient-reg-form :birth])
         :error-message @(rf/subscribe
                          [::subs/form-field-error-message :patient-reg-form :birth])}])
     [text-input
      {:name :address
       :label "address"
       :default-value @(rf/subscribe
                        [::subs/form-field-value :patient-reg-form :address])
       :on-change (partial handle-change {:field-type "text"
                                          :field-id :address})
       :on-blur handle-blur
       :error? @(rf/subscribe
                 [::subs/form-field-error? :patient-reg-form :address])
       :error-message @(rf/subscribe
                        [::subs/form-field-error-message :patient-reg-form :address])}]

     [text-input
      {:name :health_insurance_number
       :label "health insurance number"
       :default-value @(rf/subscribe
                        [::subs/form-field-value :patient-reg-form :health_insurance_number])
       :on-change (partial handle-change {:field-type "text"
                                          :field-id :health_insurance_number})
       :on-blur handle-blur
       :error? @(rf/subscribe
                 [::subs/form-field-error? :patient-reg-form :health_insurance_number])
       :error-message @(rf/subscribe
                        [::subs/form-field-error-message :patient-reg-form :health_insurance_number])}]

     [:button {:type "submit"
               :disabled (or has-errors? submitting?)
               :on-click #(rf/dispatch [::events/submitting-form {:form-id :patient-reg-form}])}
      "Save"]]))

(defn operator-input [{:keys [field
                              index
                              operator
                              operator-id
                              on-change
                              on-blur]}]
  (when (some? field)
    (let [field-type (if (= field "birth") "date" "text")
          operator-options (cond
                             (= field "gender") [{:label "equals" :value "eq"}]
                             (= field "birth") [{:label "after" :value "gt"}
                                                {:label "before" :value "lt"}
                                                {:label "equals" :value "eq"}]
                             :else [{:label "equals" :value "eq"}
                                    {:label "contains" :value "gt"}])]
      [select-picker
       {:name operator-id
        :label "operator"
        :default-value operator
        :options operator-options
        :on-change (partial on-change {:field-type field-type
                                       :index index
                                       :field-id :operator})
        :on-blur on-blur
        :error? @(rf/subscribe
                  [::subs/array-form-field-error? :patient-filter-form index :operator])
        :error-message @(rf/subscribe
                         [::subs/array-form-field-error-message :patient-filter-form index :operator])}])))

(defn value-input [{:keys [field
                           index
                           value
                           value-id
                           on-change
                           on-blur]}]
  (when (some? field)
    (cond
      (= field "gender") [select-picker
                          {:name value-id
                           :label "value"
                           :default-value value
                           :options gender-options
                           :on-change (partial on-change {:field-type "text"
                                                          :index index
                                                          :field-id :value})
                           :on-blur on-blur
                           :error? @(rf/subscribe
                                     [::subs/array-form-field-error? :patient-filter-form index :value])
                           :error-message @(rf/subscribe
                                            [::subs/array-form-field-error-message
                                             :patient-filter-form
                                             index
                                             :value])}]
      (= field "birth") (let [{:keys [year
                                      month
                                      day
                                      year-id
                                      month-id
                                      day-id]} @(rf/subscribe
                                                 [::subs/array-form-date-value
                                                  :patient-filter-form
                                                  index
                                                  :value])]
                          [date-input
                           {:label "value"
                            :year year
                            :month month
                            :day day
                            :year-id year-id
                            :month-id month-id
                            :day-id day-id
                            :on-change (partial on-change {:field-type "date"
                                                           :index index
                                                           :field-id :value})
                            :on-blur on-blur
                            :error? @(rf/subscribe
                                      [::subs/array-form-field-error? :patient-filter-form index :value])
                            :error-message @(rf/subscribe
                                             [::subs/array-form-field-error-message
                                              :patient-filter-form
                                              index
                                              :value])}])
      :else [text-input
             {:name value-id
              :label "value"
              :default-value value
              :on-change (partial on-change {:field-type "text"
                                             :index index
                                             :field-id :value})
              :on-blur on-blur
              :error? @(rf/subscribe
                        [::subs/array-form-field-error? :patient-filter-form index :value])
              :error-message @(rf/subscribe
                               [::subs/array-form-field-error-message :patient-filter-form index :value])}])))

(defn patient-filter-form []
  (let [fieldsets @(rf/subscribe [::subs/array-form-indexed :patient-filter-form])
        validation-rules (:patient-filter-form validator-map)
        handle-change (partial change-dynamic-field :patient-filter-form :field validation-rules)
        handle-blur (partial blur-field :patient-filter-form validation-rules)
        has-errors? @(rf/subscribe [::subs/has-form-errors? :patient-filter-form])
        submitting? @(rf/subscribe [::subs/form-submitting? :patient-filter-form])]
    [:form {:id :patient-filter-form}
     (doall
      (map (fn [{:keys [index
                        uuid
                        field
                        operator
                        value
                        field-id
                        operator-id
                        value-id]}]
             [:fieldset {:key uuid}
              [select-picker
               {:field-id field-id
                :label "field"
                :default-value field
                :options filter-options
                :on-change (partial handle-change {:field-type "text"
                                                   :index index
                                                   :field-id :field})
                :on-blur handle-blur
                :error? @(rf/subscribe
                          [::subs/array-form-field-error? :patient-filter-form index :field])
                :error-message @(rf/subscribe
                                 [::subs/array-form-field-error-message :patient-filter-form index :field])}]
              [operator-input {:index index
                               :field field
                               :operator operator
                               :operator-id operator-id
                               :on-change handle-change
                               :on-blur handle-blur}]
              [value-input {:index index
                            :field field
                            :value value
                            :value-id value-id
                            :on-change handle-change
                            :on-blur handle-blur}]
              [:button {:type "button"
                        :on-click #(rf/dispatch [::events/delete-dynamic-fieldset
                                                 {:form-id :patient-filter-form
                                                  :index index}])}
               "x"]])
           fieldsets))
     [:button {:type "button"
               :on-click #(rf/dispatch [::events/add-dynamic-fieldset
                                        {:form-id :patient-filter-form}])}
      "Add Filter"]
     (when (seq fieldsets)
       [:button {:type "button"
                 :disabled (or has-errors? submitting?)
                 :on-click #((rf/dispatch [::events/submitting-form
                                           {:form-id :patient-filter-form}])
                             (rf/dispatch [::events/search-patients
                                           {:filters fieldsets}]))}
        "Search"])]))

;;
;;
;; -------------------------------------------------
;;
;; View rendering functions for the entire pages
;;
;; -------------------------------------------------
(defmulti view identity)

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
    (map (fn [{:keys [id
                      name
                      gender
                      birth
                      address
                      health_insurance_number]}]
           [:li {:key id
                 :on-click #(rf/dispatch [::events/push-state
                                          :patients-edit
                                          {:id id}])}
            [:span name]
            [:span gender]
            [:span birth]
            [:span address]
            [:span health_insurance_number]])
         @(rf/subscribe [::subs/patients-formatted]))]])

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

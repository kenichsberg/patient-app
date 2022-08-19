(ns hs-test-app.core-test
  (:require [cljs.test :refer-macros [deftest is]]
            [day8.re-frame.test :as rf-test]
            [re-frame.core :refer [reg-cofx reg-fx dispatch subscribe]]
            [hs-test-app.db]
            [hs-test-app.events]
            [hs-test-app.subs]
            [hs-test-app.routes]
            [hs-test-app.views :refer [validator-map]]
            [clojure.string :as str]))

;; NOTE
;; There is difference in implementation of constructing Date object
;; between Node.js and browzer's js.
;; Node.js returns false below, even though the browzer's js returns true. 
;; (prn "2000-02-31 is invalid?: " (-> "2000-02-31" js/Date. js/isNaN))
;; (prn "a-02-31 is invalid?: " (-> "a-02-31" js/Date. js/isNaN))
;;
;;
;;---- Mock remote(api-server's) db ----------------------------------
;;
(def mocked-db
  (atom [{:id 1
          :first_name "John"
          :last_name "Smith"
          :gender true
          :birth "2000-01-01"
          :address "N.Y."
          :health_insurance_number "123456789012"}
         {:id 2
          :first_name "Bob"
          :last_name "Smith"
          :gender true
          :birth "2020-01-01"
          :address "N.Y."
          :health_insurance_number "223456789012"}]))

(defn insert-into-mocked-db
  [record]
  (let [next-id (->> @mocked-db
                     (map :id)
                     (apply max)
                     inc)]
    (swap! mocked-db
           #(conj % (merge record {:id next-id}))
           record)))

(defn update-mocked-db
  [record id]
  (swap! mocked-db
         (fn [db record id]
           (-> (remove #(= id (:id %)) db)
               (conj (merge record {:id id}))))
         record
         id))

(defn reset-mocked-db
  []
  (reset! mocked-db
          [{:id 1
            :first_name "John"
            :last_name "Smith"
            :gender true
            :birth "2000-01-01"
            :address "N.Y."
            :health_insurance_number "123456789012"}
           {:id 2
            :first_name "Bob"
            :last_name "Smith"
            :gender true
            :birth "2020-01-01"
            :address "N.Y."
            :health_insurance_number "223456789012"}]))

;;
;;
;;---- test-fixtures ----------------------------------
;;
(defn test-fixtures []
  (reset-mocked-db)

  ;;Mock cofx
  (reg-cofx
   :raw-url
   (fn [cofx _]
     (assoc cofx :raw-url "http://localhost:8080/patients")))

  ;;Mock fx
  (reg-fx
   :http-xhrio
   (fn [{:keys [method on-success params uri]}]
     (condp = method
       :get (condp = (first on-success)
              :set-patients (dispatch (conj on-success @mocked-db))
              :set-patient-reg-form (dispatch (conj on-success
                                                    (->> @mocked-db
                                                         (filter #(= 1 (:id %)))
                                                         first)))
              (throw (js/Error. (str "on-success event: " on-success " does not exist."))))
       :post (do
               (insert-into-mocked-db params)
               (dispatch on-success))
       :put (let [id (-> (str/split uri #"/") last (js/parseInt))]
              (update-mocked-db params id)
              (dispatch on-success))
       (throw (js/Error. (str "Method: " method " does not exist.")))))))

;;
;;
;;---- Util functions for testing ----------------------------------
;;
(defn subed-form-data-map
  ([form-id fieldid->info]
   (reduce (fn [acc [field-id field-info]]
             (assoc acc field-id {:value (condp :type field-info
                                           "text" (subscribe
                                                   [:form-field-value
                                                    form-id
                                                    field-id])
                                           "date" (subscribe
                                                   [:form-date-value
                                                    form-id
                                                    field-id]))
                                  :error? (subscribe
                                           [:form-field-error?
                                            form-id
                                            field-id])
                                  :error-message (subscribe
                                                  [:form-field-error-message
                                                   form-id
                                                   field-id])}))
           {}
           fieldid->info))
  ([form-id index fieldid->info]
   (reduce (fn [acc [field-id field-info]]
             (assoc acc field-id {:value (condp :type field-info
                                           "text" (subscribe
                                                   [:array-form-field-value
                                                    form-id
                                                    index
                                                    field-id])
                                           "date" (subscribe
                                                   [:array-form-date-value
                                                    form-id
                                                    index
                                                    field-id]))
                                  :error? (subscribe
                                           [:form-field-error?
                                            form-id
                                            index
                                            field-id])
                                  :error-message (subscribe
                                                  [:form-field-error-message
                                                   form-id
                                                   index
                                                   field-id])}))
           {}
           fieldid->info)))

(comment
  (subed-form-data-map
   :patient-reg-form
   {:first_name {:type "text"}
    :last_name {:type "text"}
    :gender {:type "text"}
    :birth {:type "date"}
    :address {:type "text"}
    :health_insurance_number {:type "text"}}))

(defn deref-form-map [form-map]
  (->> form-map
       (mapcat (fn [[k {:keys [value error? error-message]}]]
                 [k {:value (deref value)
                     :error? (deref error?)
                     :error-message (deref error-message)}]))
       (apply hash-map)))

;;
;;
;;---- Test Cases ----------------------------------
;;
(let [current-route                (subscribe [:current-route])
      path-params                  (subscribe [:path-params])
      query-params                 (subscribe [:query-params])
      url                          (subscribe [:url])
      ;patients                     (subscribe [:patients])
      patients-formatted           (subscribe [:patients-formatted])
      patients-in-edit             (subscribe [:patient-in-edit])
      ;reg-form                     (subscribe [:form :patient-reg-form])
      reg-form-submitting?         (subscribe [:form-submitting? :patient-reg-form])
      reg-form-errors              (subscribe [:form-errors :patient-reg-form])
      reg-form-has-errors?         (subscribe [:has-form-errors? :patient-reg-form])
      reg-form-data                (subed-form-data-map
                                    :patient-reg-form
                                    {:first_name {:type "text"}
                                     :last_name {:type "text"}
                                     :gender {:type "text"}
                                     :birth {:type "date"}
                                     :address {:type "text"}
                                     :health_insurance_number {:type "text"}})
      filter-form-indexed          (subscribe [:array-form-indexed :patient-filter-form])
      filter-form-errors           (subscribe [:form-errors :patient-filter-form])
      filter-form-has-errors?         (subscribe [:has-form-errors? :patient-filter-form])
      ;filter-form-data                (subed-form-data-map
      ;                                 :patient-filter-form
      ;                                 0
      ;                                 {:field {:type "text"}
      ;                                  :operator {:type "text"}
      ;                                  :value {:type "text"}})]
      filter-form-data                (subscribe [:form :patient-filter-form])]

  ;(deftest init-test
  ;  (rf-test/run-test-sync
  ;   (test-fixtures)
  ;   (dispatch [:initialize-db])

  ;   ;; init
  ;   (is (nil? @current-route))
  ;   (is (nil? @path-params))
  ;   (is (nil? @query-params))
  ;   (is (nil? @url))
  ;   (is (= [] @patients-formatted))
  ;   (is (nil? @patients-in-edit))
  ;   (is (false? @reg-form-submitting?))
  ;   (is (= [] @reg-form-errors))
  ;   (is (false? @reg-form-has-errors?))
  ;   (is (= (deref-form-map reg-form-data)
  ;          {:first_name {:value "" :error? false :error-message nil}
  ;           :last_name {:value "" :error? false :error-message nil}
  ;           :gender {:value "" :error? false :error-message nil}
  ;           ;:birth {:value {:year ""
  ;           ;                :month ""
  ;           ;                :day ""
  ;           ;                :year-id "birth-y"
  ;           ;                :month-id "birth-m"
  ;           ;                :day-id "birth-d"}
  ;           ;        :error? false
  ;           ;        :error-message nil}
  ;           :birth {:value "" :error? false :error-message nil}
  ;           :address {:value "" :error? false :error-message nil}
  ;           :health_insurance_number {:value "" :error? false :error-message nil}}))
  ;   (is (= '() @filter-form-indexed))
  ;   (is (= [] @filter-form-errors))
  ;   (is (false? @filter-form-has-errors?))
  ;   (is (= (deref-form-map filter-form-data)
  ;          {:field {:value "" :error? false :error-message nil}
  ;           :operator {:value "" :error? false :error-message nil}
  ;           :value {:value "" :error? false :error-message nil}}))))

  ;;
  ;;
  ;; Init app-db and navigation flow.
  ;;
  (deftest init-test
    (rf-test/run-test-sync
     (test-fixtures)
     (dispatch [:initialize-db])
     ;; Initial Navigation to "/patients"
     (dispatch [:init-routes])

     (is (= :hs-test-app.views/list @current-route))
     (is (= {} @path-params))
     (is (nil? @query-params))
     (is (= "/patients" @url))
     ;(is (= [] @patients-formatted))
     (is (= [{:id 1,
              :gender "Male",
              :birth "Jan 01, 2000",
              :address "N.Y.",
              :health_insurance_number "123456789012",
              :name "John Smith"}
             {:id 2,
              :gender "Male",
              :birth "Jan 01, 2020",
              :address "N.Y.",
              :health_insurance_number "223456789012",
              :name "Bob Smith"}]
            @patients-formatted))
     (is (nil? @patients-in-edit))
     (is (false? @reg-form-submitting?))
     (is (= [] @reg-form-errors))
     (is (false? @reg-form-has-errors?))
     (is (= {:first_name {:value "" :error? false :error-message nil}
             :last_name {:value "" :error? false :error-message nil}
             :gender {:value "" :error? false :error-message nil}
             :birth {:value "" :error? false :error-message nil}
             :address {:value "" :error? false :error-message nil}
             :health_insurance_number {:value "" :error? false :error-message nil}}
            (deref-form-map reg-form-data)))
     (is (= '() @filter-form-indexed))
     (is (= [] @filter-form-errors))
     (is (false? @filter-form-has-errors?))
     (is (= []
            @filter-form-data))))

  ;;
  ;;
  ;; Navigate to Create Page and submit form 
  ;;
  (deftest create-page-test
    (rf-test/run-test-sync
     (test-fixtures)
     (dispatch [:initialize-db])
     (dispatch [:init-routes])

     ;; Navigate to Create Page
     (dispatch [:trigger-navigation "/patients/create" nil])
     (is (= :hs-test-app.views/create @current-route))
     (is (= {} @path-params))
     (is (nil? @query-params))
     (is (= "/patients/create" @url))
     (is (false? @reg-form-submitting?))
     (is (= []
            @reg-form-errors))
     (is ;(true? @reg-form-has-errors?))
      (false? @reg-form-has-errors?))
     (is (= {:first_name {:value "" :error? false :error-message nil}
             :last_name {:value "" :error? false :error-message nil}
             :gender {:value "" :error? false :error-message nil}
             :birth {:value "" :error? false
                     :error-message nil}
             :address {:value "" :error? false :error-message nil}
             :health_insurance_number {:value "" :error? false :error-message nil}}
            (deref-form-map reg-form-data)))

     ;; Invalid Input to First Name
     (dispatch [:update-field {:form-id :patient-reg-form
                               :field-type "text"
                               :field-id :first_name
                               :value "1"
                               :validation-rules (:patient-reg-form validator-map)}])
     (is (false? @reg-form-submitting?))
     (is (= [{:field :first_name :message "Please input alphabets."}
             {:field :last_name :message "Last Name is required."}
             {:field :last_name :message "Please input alphabets."}
             {:field :gender :message "Gender is required."}
             {:field :birth :message "Birth is required."}
             {:field :address :message "Address is required."}
             {:field :health_insurance_number
              :message "Health Insurance Number is required."}
             {:field :health_insurance_number :message "Please input numbers."}
             {:field :health_insurance_number
              :message "Incorrect number of digits."}]
            @reg-form-errors))
     (is (true? @reg-form-has-errors?))
     (is (= {:first_name {:value "1" :error? true :error-message "Please input alphabets."}
             :last_name {:value "" :error? true :error-message "Last Name is required."}
             :gender {:value "" :error? true :error-message "Gender is required."}
             :birth {:value ""
                     :error? true
                     :error-message "Birth is required."}
             :address {:value "" :error? true :error-message "Address is required."}
             :health_insurance_number {:value "" :error? true :error-message "Health Insurance Number is required."}}
            (deref-form-map reg-form-data)))

     ;; Valid Input to First Name
     (dispatch [:update-field {:form-id :patient-reg-form
                               :field-type "text"
                               :field-id :first_name
                               :value "Will"
                               :validation-rules (:patient-reg-form validator-map)}])
     (is (false? @reg-form-submitting?))
     (is (= [{:field :last_name :message "Last Name is required."}
             {:field :last_name :message "Please input alphabets."}
             {:field :gender :message "Gender is required."}
             {:field :birth :message "Birth is required."}
             {:field :address :message "Address is required."}
             {:field :health_insurance_number
              :message "Health Insurance Number is required."}
             {:field :health_insurance_number :message "Please input numbers."}
             {:field :health_insurance_number
              :message "Incorrect number of digits."}]
            @reg-form-errors))
     (is (true? @reg-form-has-errors?))
     (is (= {:first_name {:value "Will" :error? false :error-message nil}
             :last_name {:value "" :error? true :error-message "Last Name is required."}
             :gender {:value "" :error? true :error-message "Gender is required."}
             :birth {:value ""
                     :error? true
                     :error-message "Birth is required."}
             :address {:value "" :error? true :error-message "Address is required."}
             :health_insurance_number {:value "" :error? true :error-message "Health Insurance Number is required."}}
            (deref-form-map reg-form-data)))

     ;; Valid Inputs
     (dispatch [:update-field {:form-id :patient-reg-form
                               :field-type "text"
                               :field-id :last_name
                               :value "Smith"
                               :validation-rules (:patient-reg-form validator-map)}])
     (dispatch [:update-field {:form-id :patient-reg-form
                               :field-type "text"
                               :field-id :gender
                               :value true
                               :validation-rules (:patient-reg-form validator-map)}])
     (dispatch [:update-field {:form-id :patient-reg-form
                               :field-type "date"
                               :field-id :birth
                               :date-unit :day
                               :value "25"
                               :validation-rules (:patient-reg-form validator-map)}])
     (dispatch [:update-field {:form-id :patient-reg-form
                               :field-type "date"
                               :field-id :birth
                               :date-unit :month
                               :value "09"
                               :validation-rules (:patient-reg-form validator-map)}])
     (dispatch [:update-field {:form-id :patient-reg-form
                               :field-type "date"
                               :field-id :birth
                               :date-unit :year
                               :value "1968"
                               :validation-rules (:patient-reg-form validator-map)}])
     (dispatch [:update-field {:form-id :patient-reg-form
                               :field-type "text"
                               :field-id :address
                               :value "San Francisco"
                               :validation-rules (:patient-reg-form validator-map)}])
     (dispatch [:update-field {:form-id :patient-reg-form
                               :field-type "text"
                               :field-id :health_insurance_number
                               :value "111111111111"
                               :validation-rules (:patient-reg-form validator-map)}])
     (is (false? @reg-form-submitting?))
     (is (nil? @reg-form-errors))
     (is (false? @reg-form-has-errors?))
     (is (= {:first_name {:value "Will" :error? false :error-message nil}
             :last_name {:value "Smith" :error? false :error-message nil}
             :gender {:value true :error? false :error-message nil}
             :birth {:value "1968-09-25"
                     :error? false
                     :error-message nil}
             :address {:value "San Francisco" :error? false :error-message nil}
             :health_insurance_number {:value "111111111111" :error? false :error-message nil}}
            (deref-form-map reg-form-data)))

     ;; Press Save button 
     (dispatch [:toggle-form-submitting :patient-reg-form true])
     (is (true? @reg-form-submitting?))
     (dispatch [:create-patient {:values
                                 {:first_name "Will"
                                  :last_name "Smith"
                                  :gender true
                                  :birth "1968-09-25"
                                  :address "San Francisco"
                                  :health_insurance_number "111111111111"}}])
     (is (= :hs-test-app.views/list @current-route))
     (is (= {} @path-params))
     (is (nil? @query-params))
     (is (= "/patients" @url))
     (is (= [{:id 1
              :gender "Male"
              :birth "Jan 01, 2000"
              :address "N.Y."
              :health_insurance_number "123456789012"
              :name "John Smith"}
             {:id 2
              :gender "Male"
              :birth "Jan 01, 2020"
              :address "N.Y."
              :health_insurance_number "223456789012"
              :name "Bob Smith"}
             {:id 3
              :gender "Male"
              :birth "Sep 25, 1968"
              :address "San Francisco"
              :health_insurance_number "111111111111"
              :name "Will Smith"}]
            @patients-formatted))
     (is (nil? @patients-in-edit))
     (is (false? @reg-form-submitting?))
     (is (= [] @reg-form-errors))
     (is (false? @reg-form-has-errors?))
     (is (= {:first_name {:value "" :error? false :error-message nil}
             :last_name {:value "" :error? false :error-message nil}
             :gender {:value "" :error? false :error-message nil}
             :birth {:value "" :error? false :error-message nil}
             :address {:value "" :error? false :error-message nil}
             :health_insurance_number {:value "" :error? false :error-message nil}}
            (deref-form-map reg-form-data)))))

  ;;
  ;;
  ;; Navigate to Edit Page and press Save button 
  ;;
  (deftest edit-page-test
    (rf-test/run-test-sync
     (test-fixtures)
     (dispatch [:initialize-db])
     (dispatch [:init-routes])

     ;; Navigate to Edit page of id 1
     (dispatch [:trigger-navigation "/patients/1/edit" nil])
     (is (= :hs-test-app.views/edit @current-route))
     (is (= {:id "1"} @path-params))
     (is (nil? @query-params))
     (is (= "/patients/1/edit" @url))
     (is (= [{:id 1,
              :gender "Male",
              :birth "Jan 01, 2000",
              :address "N.Y.",
              :health_insurance_number "123456789012",
              :name "John Smith"}
             {:id 2,
              :gender "Male",
              :birth "Jan 01, 2020",
              :address "N.Y.",
              :health_insurance_number "223456789012",
              :name "Bob Smith"}]
            @patients-formatted))
     (is (= @patients-in-edit
            {:id 1,
             :first_name "John"
             :last_name "Smith"
             :gender true,
             :birth "2000-01-01",
             :address "N.Y.",
             :health_insurance_number "123456789012"}))
     (is (false? @reg-form-submitting?))
     (is (= [] @reg-form-errors))
     (is (false? @reg-form-has-errors?))
     (is (= {:first_name {:value "John" :error? false :error-message nil}
             :last_name {:value "Smith" :error? false :error-message nil}
             :gender {:value true :error? false :error-message nil}
             :birth {:value "2000-01-01"
                     :error? false
                     :error-message nil}
             :address {:value "N.Y." :error? false :error-message nil}
             :health_insurance_number {:value "123456789012" :error? false :error-message nil}}
            (deref-form-map reg-form-data)))

     ;; Invalid Input to Birth
     (dispatch [:update-field {:form-id :patient-reg-form
                               :field-type "date"
                               :field-id :birth
                               :date-unit :day
                               :value "0"
                               :validation-rules (:patient-reg-form validator-map)}])
     (is (false? @reg-form-submitting?))
     (is (= [{:field :birth :message "Invalid date."}]
            @reg-form-errors))
     (is (true? @reg-form-has-errors?))
     (is (= {:first_name {:value "John" :error? false :error-message nil}
             :last_name {:value "Smith" :error? false :error-message nil}
             :gender {:value true :error? false :error-message nil}
             :birth {:value "2000-01-0"
                     :error? true
                     :error-message "Invalid date."}
             :address {:value "N.Y." :error? false :error-message nil}
             :health_insurance_number {:value "123456789012" :error? false :error-message nil}}
            (deref-form-map reg-form-data)))

     ;; Valid Input to Birth - day
     (dispatch [:update-field {:form-id :patient-reg-form
                               :field-type "date"
                               :field-id :birth
                               :date-unit :day
                               :value "31"
                               :validation-rules (:patient-reg-form validator-map)}])
     (is (false? @reg-form-submitting?))
     (is (nil? @reg-form-errors))
     (is (false? @reg-form-has-errors?))
     (is (= {:first_name {:value "John" :error? false :error-message nil}
             :last_name {:value "Smith" :error? false :error-message nil}
             :gender {:value true :error? false :error-message nil}
             :birth {:value "2000-01-31"
                     :error? false
                     :error-message nil}
             :address {:value "N.Y." :error? false :error-message nil}
             :health_insurance_number {:value "123456789012" :error? false :error-message nil}}
            (deref-form-map reg-form-data)))

     ;;; Invalid Input to Birth - month
     ;(dispatch [:update-field {:form-id :patient-reg-form
     ;                          :field-type "date"
     ;                          :field-id :birth
     ;                          :date-unit :month
     ;                          :value "02"
     ;                          :validation-rules (:patient-reg-form validator-map)}])
     ;(is (false? @reg-form-submitting?))
     ;(is (= [{:field :birth :message "Invalid date."}]
     ;       @reg-form-errors))
     ;(is (true? @reg-form-has-errors?))
     ;(is (= {:first_name {:value "John" :error? false :error-message nil}
     ;        :last_name {:value "Smith" :error? false :error-message nil}
     ;        :gender {:value true :error? false :error-message nil}
     ;        :birth {:value "2000-02-31"
     ;                :error? true
     ;                :error-message "Invalid date."}
     ;        :address {:value "N.Y." :error? false :error-message nil}
     ;        :health_insurance_number {:value "123456789012" :error? false :error-message nil}}
     ;       (deref-form-map reg-form-data)))

     ;; Valid Input to Birth - month
     (dispatch [:update-field {:form-id :patient-reg-form
                               :field-type "date"
                               :field-id :birth
                               :date-unit :month
                               :value "12"
                               :validation-rules (:patient-reg-form validator-map)}])
     (is (false? @reg-form-submitting?))
     (is (nil? @reg-form-errors))
     (is (false? @reg-form-has-errors?))
     (is (= {:first_name {:value "John" :error? false :error-message nil}
             :last_name {:value "Smith" :error? false :error-message nil}
             :gender {:value true :error? false :error-message nil}
             :birth {:value "2000-12-31"
                     :error? false
                     :error-message nil}
             :address {:value "N.Y." :error? false :error-message nil}
             :health_insurance_number {:value "123456789012" :error? false :error-message nil}}
            (deref-form-map reg-form-data)))

     ;; Valid Input to Birth - year
     (dispatch [:update-field {:form-id :patient-reg-form
                               :field-type "date"
                               :field-id :birth
                               :date-unit :year
                               :value "1999"
                               :validation-rules (:patient-reg-form validator-map)}])
     (is (false? @reg-form-submitting?))
     (is (nil? @reg-form-errors))
     (is (false? @reg-form-has-errors?))
     (is (= {:first_name {:value "John" :error? false :error-message nil}
             :last_name {:value "Smith" :error? false :error-message nil}
             :gender {:value true :error? false :error-message nil}
             :birth {:value "1999-12-31"
                     :error? false
                     :error-message nil}
             :address {:value "N.Y." :error? false :error-message nil}
             :health_insurance_number {:value "123456789012" :error? false :error-message nil}}
            (deref-form-map reg-form-data)))

     ;; Press Save button 
     (dispatch [:toggle-form-submitting :patient-reg-form true])
     (is (true? @reg-form-submitting?))
     (dispatch [:update-patient
                1
                {:values
                 {:first_name "John"
                  :last_name "Smith"
                  :gender true
                  :birth "1999-12-31"
                  :address "N.Y."
                  :health_insurance_number "123456789012"}}])
     (is (= :hs-test-app.views/list @current-route))
     (is (= {} @path-params))
     (is (nil? @query-params))
     (is (= "/patients" @url))
     (is (= [{:id 1,
              :gender "Male",
              :birth "Dec 31, 1999",
              :address "N.Y.",
              :health_insurance_number "123456789012",
              :name "John Smith"}
             {:id 2,
              :gender "Male",
              :birth "Jan 01, 2020",
              :address "N.Y.",
              :health_insurance_number "223456789012",
              :name "Bob Smith"}]
            @patients-formatted))
     (is (nil? @patients-in-edit))
     (is (false? @reg-form-submitting?))
     (is (= [] @reg-form-errors))
     (is (false? @reg-form-has-errors?))
     (is (= {:first_name {:value "" :error? false :error-message nil}
             :last_name {:value "" :error? false :error-message nil}
             :gender {:value "" :error? false :error-message nil}
             :birth {:value "" :error? false :error-message nil}
             :address {:value "" :error? false :error-message nil}
             :health_insurance_number {:value "" :error? false :error-message nil}}
            (deref-form-map reg-form-data)))))

  ;;
  ;;
  ;; Change filter values 
  ;;
  (deftest filter-form-test
    (rf-test/run-test-sync
     (test-fixtures)
     (dispatch [:initialize-db])
     (dispatch [:init-routes])

     ;;
     ;;
     ;; 1. Press Add Filter button
     (dispatch [:add-filter {:form-id :patient-filter-form
                             :validation-rules (:patient-filter-form validator-map)}])
     (is (= [{:field :field :message "Required." :index 0}]
            @filter-form-errors))
     (is (true? @filter-form-has-errors?))
     (is (= [{:field nil}]
            @filter-form-data))

     ;; 1. Valid Input but not sufficient
     (dispatch [:update-dynamic-field {:form-id :patient-filter-form
                                       :field-type "text"
                                       :index 0
                                       :field-id :field
                                       :value "first_name"
                                       :validation-rules (:patient-filter-form validator-map)}])
     (is (= [{:field :value :message "Required." :index 0}]
            @filter-form-errors))
     (is (true? @filter-form-has-errors?))
     (is (= [{:field "first_name" :operator "eq"}]
            @filter-form-data))

     ;; 1. Valid and sufficient Input
     (dispatch [:update-dynamic-field {:form-id :patient-filter-form
                                       :field-type "text"
                                       :index 0
                                       :field-id :operator
                                       :value "gt"
                                       :validation-rules (:patient-filter-form validator-map)}])
     (dispatch [:update-dynamic-field {:form-id :patient-filter-form
                                       :field-type "text"
                                       :index 0
                                       :field-id :value
                                       :value "o"
                                       :validation-rules (:patient-filter-form validator-map)}])
     (is (nil?  @filter-form-errors))
     (is (false? @filter-form-has-errors?))
     (is (= [{:field "first_name" :operator "gt" :value "o"}]
            @filter-form-data))

     ;;
     ;;
     ;; 2. Press Add Filter button
     (dispatch [:add-filter {:form-id :patient-filter-form
                             :validation-rules (:patient-filter-form validator-map)}])
     (is (= [{:field :field :message "Required." :index 1}]
            @filter-form-errors))
     (is (true? @filter-form-has-errors?))
     (is (= [{:field "first_name" :operator "gt" :value "o"}
             {:field nil}]
            @filter-form-data))

     ;; 2. Valid Input but not sufficient
     (dispatch [:update-dynamic-field {:form-id :patient-filter-form
                                       :field-type "text"
                                       :index 1
                                       :field-id :field
                                       :value "birth"
                                       :validation-rules (:patient-filter-form validator-map)}])
     (is (= [{:field :value :message "Required." :index 1}]
            @filter-form-errors))
     (is (true? @filter-form-has-errors?))
     (is (= [{:field "first_name" :operator "gt" :value "o"}
             {:field "birth" :operator "eq"}]
            @filter-form-data))

     ;; 2. Invalid Input
     (dispatch [:update-dynamic-field {:form-id :patient-filter-form
                                       :field-type "text"
                                       :index 1
                                       :field-id :operator
                                       :value "lt"
                                       :validation-rules (:patient-filter-form validator-map)}])
     (dispatch [:update-dynamic-field {:form-id :patient-filter-form
                                       :field-type "date"
                                       :index 1
                                       :field-id :value
                                       :date-unit :day
                                       :value "00"
                                       :validation-rules (:patient-filter-form validator-map)}])
     (dispatch [:update-dynamic-field {:form-id :patient-filter-form
                                       :field-type "date"
                                       :index 1
                                       :field-id :value
                                       :date-unit :month
                                       :value "01"
                                       :validation-rules (:patient-filter-form validator-map)}])
     (dispatch [:update-dynamic-field {:form-id :patient-filter-form
                                       :field-type "date"
                                       :index 1
                                       :field-id :value
                                       :date-unit :year
                                       :value "2010"
                                       :validation-rules (:patient-filter-form validator-map)}])
     (is (= [{:field :value :message "Invalid date." :index 1}]
            @filter-form-errors))
     (is (true? @filter-form-has-errors?))
     (is (= [{:field "first_name" :operator "gt" :value "o"}
             {:field "birth" :operator "lt" :value "2010-01-00"}]
            @filter-form-data))

     ;; 2. Valid Input
     (dispatch [:update-dynamic-field {:form-id :patient-filter-form
                                       :field-type "date"
                                       :index 1
                                       :field-id :value
                                       :date-unit :day
                                       :value "15"
                                       :validation-rules (:patient-filter-form validator-map)}])
     (is (nil?  @filter-form-errors))
     (is (false? @filter-form-has-errors?))
     (is (= [{:field "first_name" :operator "gt" :value "o"}
             {:field "birth" :operator "lt" :value "2010-01-15"}]
            @filter-form-data))

     ;;
     ;;
     ;; 3. Change value of :field when values of :operator and :value
     ;;   are already filled in. 
     (dispatch [:update-dynamic-field {:form-id :patient-filter-form
                                       :field-type "text"
                                       :index 0
                                       :field-id :field
                                       :value "health_insurance_number"
                                       :validation-rules (:patient-filter-form validator-map)}])
     (is (= [{:field :value :message "Required." :index 0}]
            @filter-form-errors))
     (is (true? @filter-form-has-errors?))
     (is (= [{:field "health_insurance_number" :operator "eq"}
             {:field "birth" :operator "lt" :value "2010-01-15"}]
            @filter-form-data))

     ;;
     ;;
     ;; 4. Delete
     (dispatch [:delete-dynamic-fieldset {:form-id :patient-filter-form
                                          :index 0}])
     (is (= '()  @filter-form-errors))
     (is (false? @filter-form-has-errors?))
     (is (= [{:field "birth" :operator "lt" :value "2010-01-15"}]
            @filter-form-data)))))

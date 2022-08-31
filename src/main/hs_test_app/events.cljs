(ns hs-test-app.events
  (:require [goog.url :as gurl]
            [re-frame.core :refer [inject-cofx reg-event-db reg-event-fx reg-fx dispatch]]
            [clojure.string :as str]
            [day8.re-frame.http-fx]
            [ajax.core :refer [json-request-format json-response-format]]
            [hs-test-app.config :as config]
            [hs-test-app.db :as db]
            [hs-test-app.utils :as utils]
            [hs-test-app.validation :as v]))

;;
;;
;;----- fx ----------------------------------------------
;;
;;
;; Grasping event ids (unique keys) which are debouncing now 
;;
(defonce debouncing-ids
  (atom {}))

(reg-fx
 :dispatch-debounce
 (fn [{:keys [id event timeout]}]
   (js/clearTimeout (@debouncing-ids id))
   (swap! debouncing-ids
          assoc
          id
          (js/setTimeout (fn []
                           (dispatch event)
                           (swap! debouncing-ids dissoc id))
                         timeout))))

;;
;;
;;----- constatnts --------------------------------------
;;
;;
;; For http-xhrio
;;
(def request-defaults
  {:timeout 6000
   :response-format (json-response-format {:keywords? true})
   :on-failure [:set-error]})

;;
;;
;;----- events ------------------------------------------
;;
(reg-event-db
 :initialize-db
 (fn [_ _]
   db/default-db))

;;
;;
;; Set popstate Event Listner and
;; trigger initial Navigation to the URL put in the address bar.
;;
(reg-event-fx
 :init-routes
 (fn []
   {:init-popstate-listener nil
    :fx [[:dispatch [:initial-navigation]]]}))

(reg-event-fx
 :initial-navigation
 [(inject-cofx :raw-url)]
 (fn [{:keys [raw-url]} _]
   (let [URL (gurl/resolveUrl raw-url)
         path (.-pathname URL)
         q-str (.-search URL)
         query-params (utils/querystr->map q-str)]
     {:dispatch [:trigger-navigation path query-params]})))

;;
;;
;; Flow of Navigation
;;  1. event    :trigger-navigation --- set fx :navigation
;;  2. fx       :navigation         --- call history.pushState()
;;  3. function on-popstate         --- dispatch event :navigated
;;  4. event    :navigated          --- set view resource to app-db :route
;;  5. multi-method on-navigated    --- dispatch events neccessary for new view
;;
(reg-event-fx
 :trigger-navigation
 (fn [cofx [_ base-path query-params]]
   (let [old-url (get-in cofx [:db :url])]
     {:navigation [base-path query-params old-url]})))

(defmulti on-navigated (fn [resource _] resource))
(defmethod on-navigated :hs-test-app.views/list [_ _ query-params]
  {:fx [[:dispatch [:fetch-patients
                    (get query-params :keywords)
                    (get query-params :filters)]]]})
(defmethod on-navigated :hs-test-app.views/create []
  {:fx [[:dispatch [:clear-patient-reg-form]]]})
(defmethod on-navigated :hs-test-app.views/edit [_ path-params]
  {:fx [[:dispatch [:fetch-patient-by-id (:id path-params)]]]})
(defmethod on-navigated :default [_ _] nil)

(reg-event-fx
 :navigated
 (fn [cofx [_ [resource path-params query-params url]]]
   (merge {:db (assoc (:db cofx)
                      :route resource
                      :path-params path-params
                      :query-params query-params
                      :url url)}
          (on-navigated resource path-params query-params))))

(reg-event-db
 :set-error
 (fn [db [_ res]]
   (assoc db :error res)))

(reg-event-fx
 :fetch-patients
 (fn [_ [_ keywords filters]]
   {:http-xhrio (assoc request-defaults
                       :method :get
                       :uri (str config/API_URL
                                 "/patients"
                                 (utils/map->querystr {:keywords keywords
                                                       :filters filters}))
                       :on-success [:set-patients])}))

(reg-event-db
 :set-patients
 (fn [db [_ res]]
   (assoc db
          :patients res
          :patient-in-edit nil)))

(reg-event-fx
 :search-patients
 (fn [_ [_ {:keys [keywords filters]}]]
   (let [filtervecs (mapv #((juxt :field :operator :value) %)
                          filters)]
     {:dispatch-debounce {:id :search-patients
                          :event [:trigger-navigation
                                  "/patients"
                                  {:keywords keywords
                                   :filters filtervecs}]
                          :timeout 1000}})))

(reg-event-fx
 :on-change-search-keywords
 (fn [cofx [_ {:keys [keywords filters]}]]
   {:db (assoc-in (:db cofx) [:form :patient-search] keywords)
    :fx [[:dispatch [:search-patients {:keywords keywords
                                       :filters filters}]]]}))

(reg-event-fx
 :fetch-patient-by-id
 (fn [{:keys [db]} [_ patient-id]]
   {:db (assoc db :patient-in-edit nil)
    :http-xhrio (assoc request-defaults
                       :method :get
                       :uri (str config/API_URL "/patients/" patient-id)
                       :on-success [:set-patient-reg-form])}))

(reg-event-db
 :set-patient-reg-form
 (fn [db [_ res]]
   (-> db
       (assoc :patient-in-edit res)
       (assoc-in [:form :patient-reg-form]
                 (dissoc res :id)))))

(reg-event-db
 :clear-patient-reg-form
 (fn [db _]
   (assoc-in db [:form :patient-reg-form] {})))

(reg-event-fx
 :create-patient
 (fn [_ [_ {:keys [values]}]]
   {:http-xhrio (assoc request-defaults
                       :method :post
                       :uri (str config/API_URL "/patients")
                       :params values
                       :format (json-request-format)
                       :on-success [:on-success-submit])}))

(reg-event-fx
 :update-patient
 (fn [_ [_ patient-id {:keys [values]}]]
   {:fx [[:http-xhrio (assoc request-defaults
                             :method :put
                             :uri (str config/API_URL "/patients/" patient-id)
                             :params values
                             :format (json-request-format)
                             :on-success [:on-success-submit])]]}))

(reg-event-fx
 :on-success-submit
 (fn [_ _]
   {:fx [[:dispatch [:trigger-navigation "/patients"]]
         [:dispatch [:toggle-form-submitting :patient-reg-form false]]
         [:dispatch [:close-form {:form-id :patient-reg-form}]]]}))

(reg-event-fx
 :delete-patient
 (fn [_ [_ patient-id]]
   {:http-xhrio (assoc request-defaults
                       :method :delete
                       :uri (str config/API_URL "/patients/" patient-id)
                       :format (json-request-format)
                       :on-success [:fetch-patients])}))

(defn update-date-str [old-str date-unit value]
  (-> (if (empty? old-str) "--" old-str)
      (str/split #"-" -1)
      (cond->
       (= date-unit :year) (assoc 0 value)
       (= date-unit :month) (assoc 1 value)
       (= date-unit :day) (assoc 2 value))
      (->>
       (str/join "-"))))

(reg-event-db
 :update-field
 (fn [db [_ {:keys [form-id
                    field-type
                    field-id
                    value
                    validation-rules
                    date-unit]}]]
   (cond-> db
     (= field-type "date") (update-in [:form form-id field-id]
                                      update-date-str
                                      date-unit
                                      value)
     (= field-type "text") (assoc-in [:form form-id field-id] value)
     :always (#(let [form-values (get-in % [:form form-id])
                     validation-result (v/validate validation-rules form-values)]
                 (assoc-in % [:form-errors form-id] validation-result))))))

(reg-event-fx
 :on-change-field
 (fn [_ [_ {:keys [form-id
                   field-type
                   field-id
                   value
                   validation-rules
                   date-unit]}]]
   {:dispatch-debounce {:id (-> (str "update-" (name form-id) "-" (name field-id))
                                keyword)
                        :event [:update-field
                                {:field-type field-type
                                 :form-id form-id
                                 :field-id field-id
                                 :date-unit date-unit
                                 :value value
                                 :validation-rules validation-rules}]
                        :timeout 1000}}))

(reg-event-db
 :update-dynamic-field
 (fn [db [_ {:keys [form-id
                    field-type
                    index
                    field-id
                    value
                    validation-rules
                    date-unit]}]]
   (cond-> db
     (= field-id :field) (assoc-in [:form form-id index] {:field value
                                                          :operator "eq"})
     (and (not= field-id :field)
          (= field-type "date")) (update-in [:form form-id index field-id]
                                            update-date-str
                                            date-unit
                                            value)
     (and (not= field-id :field)
          (= field-type "text")) (assoc-in [:form form-id index field-id] value)
     :always (#(let [form-values (get-in % [:form form-id])
                     validation-result (v/validate validation-rules form-values)]
                 (assoc-in % [:form-errors form-id] validation-result))))))

(reg-event-fx
 :on-change-dynamic-field
 (fn [_ [_ {:keys [form-id
                   index
                   field-id]
            :as params}]]
   {:dispatch-debounce {:id (-> (str (name form-id) "/" index "/" (name field-id))
                                keyword)
                        :event [:update-dynamic-field params]
                        :timeout 1000}}))

(reg-event-db
 :update-errors
 (fn [db [_ {:keys [form-id validation-rules]}]]
   (let [form-values (get-in db [:form form-id])
         validation-result (v/validate validation-rules form-values)]
     (assoc-in db [:form-errors form-id] validation-result))))

(reg-event-db
 :toggle-form-submitting
 (fn [db [_ form-id submitting?]]
   (assoc-in db [:form-submitting? form-id] submitting?)))

(reg-event-db
 :close-form
 (fn [db [_ {:keys [form-id]}]]
   (-> db
       (update :form dissoc form-id)
       (update :form-errors dissoc form-id))))

(reg-event-db
 :add-dynamic-fieldset
 (fn [db [_ {:keys [form-id]}]]
   (update-in db [:form form-id] #(vec (conj %1 %2)) {:field nil})))

(reg-event-fx
 :add-filter
 (fn [_ [_ {:keys [form-id validation-rules]}]]
   {:fx [[:dispatch [:add-dynamic-fieldset {:form-id form-id}]]
         [:dispatch [:update-errors {:form-id form-id
                                     :validation-rules validation-rules}]]]}))

(defn drop-index [coll index]
  (vec (concat (subvec coll 0 index)
               (subvec coll (inc index)))))

(reg-event-db
 :delete-dynamic-fieldset
 (fn [db [_ {:keys [form-id index]}]]
   (-> db
       (update-in [:form form-id] drop-index index)
       (update-in [:form-errors form-id]
                  (fn [v] (remove #(= (:index %) index) v))))))

(ns hs-test-app.events
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [day8.re-frame.http-fx]
            [route-map.core :as route-map]
            [ajax.core :refer [json-request-format json-response-format]]
            [hs-test-app.config :as config]
            [hs-test-app.db :as db]
            [hs-test-app.routes :refer [routes]]
            [hs-test-app.fx :as fx]
            [hs-test-app.utils :as utils]
            [hs-test-app.validation :as v]))

(def request-defaults
  {:timeout 6000
   :response-format (json-response-format {:keywords? true})
   :on-failure [::set-error]})

(rf/reg-event-db
 ::initialize-db
 (fn [_ _]
   db/default-db))

(rf/reg-event-fx
 ::init-routes
 (fn [cofx [_ route]]
   {:db (assoc (:db cofx)
               :route route)
    :fx [[:dispatch [::init-popstate-listener []]]
         [:dispatch [::initial-navigation []]]]}))

(rf/reg-event-fx
 ::init-popstate-listener
 (fn []
   {::fx/init-popstate-listener []}))

(rf/reg-event-fx
 ::initial-navigation
 (fn []
   (let [path (.. js/window -location -pathname)
         q-str (.. js/window -location -search)
         matched (route-map/match path routes)
         route (:match matched)
         path-params (if (empty? (:params matched)) nil (:params matched))
         query-params (utils/querystr->map q-str)]
     {:dispatch [::navigated [route path-params query-params]]})))

(rf/reg-event-fx
 ::push-state
 (fn [_ [_ route-name path-params query-params]]
   {::fx/push-state [route-name path-params query-params]}))

(defmulti on-navigated (fn [view-key _] view-key))
(defmethod on-navigated :hs-test-app.views/list [_ _ query-params]
  {:fx [[:dispatch [::fetch-patients
                    (get query-params :keywords)
                    (get query-params :filters)]]]})
(defmethod on-navigated :hs-test-app.views/edit [_ path-params]
  {:fx [[:dispatch [::fetch-patient-by-id (:id path-params)]]]})
(defmethod on-navigated :default [_ _] nil)

(rf/reg-event-fx
 ::navigated
 (fn [cofx [_ [route path-params query-params]]]
   (merge {:db (assoc (:db cofx)
                      :route route
                      :path-params path-params
                      :query-params query-params)}
          (on-navigated route path-params query-params))))

(rf/reg-event-db
 ::set-error
 (fn [db [_ res]]
   (assoc db :error res)))

(rf/reg-event-fx
 ::fetch-patients
 (fn [_ [_ keywords filters]]
   {:http-xhrio (assoc request-defaults
                       :method :get
                       :uri (str config/API_URL
                                 "/patients"
                                 (utils/map->querystr {:keywords keywords
                                                       :filters filters}))
                       :on-success [::set-patients])}))

(rf/reg-event-db
 ::set-patients
 (fn [db [_ res]]
   (assoc db
          :patients res
          :patient-in-edit nil)))

(rf/reg-event-fx
 ::search-patients
 (fn [_ [_ {:keys [keywords filters]}]]
   (let [filtervecs (mapv (fn [{:keys [field operator value]}]
                            [field operator value])
                          filters)]
     {::fx/dispatch-debounce [:search-patients
                              [::push-state
                               :patients
                               nil
                               {:keywords keywords
                                :filters filtervecs}]
                              1000]})))

(rf/reg-event-fx
 ::fetch-patient-by-id
 (fn [{:keys [db]} [_ patient-id]]
   {:db (assoc db :patient-in-edit nil)
    :http-xhrio (assoc request-defaults
                       :method :get
                       :uri (str config/API_URL "/patients/" patient-id)
                       :on-success [::set-patient-reg-form])}))

(rf/reg-event-db
 ::set-patient-reg-form
 (fn [db [_ res]]
   (-> db
       (assoc :patient-in-edit res)
       (assoc-in [:form :patient-reg-form]
                 (dissoc res :id)))))

(rf/reg-event-fx
 ::create-patient
 (fn [_ [_ {:keys [values]}]]
   {:http-xhrio (assoc request-defaults
                       :method :post
                       :uri (str config/API_URL "/patients")
                       :params values
                       :format (json-request-format)
                       :on-success [::push-state :patients])}))

(rf/reg-event-fx
 ::update-patient
 (fn [_ [_ patient-id {:keys [values]}]]
   {:fx [[:http-xhrio (assoc request-defaults
                             :method :put
                             :uri (str config/API_URL "/patients/" patient-id)
                             :params values
                             :format (json-request-format)
                             :on-success [::on-success-update])]]}))

(rf/reg-event-fx
 ::on-success-update
 (fn [_ _]
   {:fx [[:dispatch [::push-state :patients]]
         [:dispatch [::close-form {:form-id :patient-reg-form}]]]}))

(rf/reg-event-fx
 ::delete-patient
 (fn [_ [_ patient-id]]
   {:http-xhrio (assoc request-defaults
                       :method :delete
                       :uri (str config/API_URL "/patients/" patient-id)
                       :format (json-request-format)
                       :on-success [::fetch-patients])}))

(defn update-date-str [old-str date-unit value]
  (-> (if (empty? old-str) "--" old-str)
      (str/split #"-" -1)
      (cond->
       (= date-unit :year) (assoc 0 value)
       (= date-unit :month) (assoc 1 value)
       (= date-unit :day) (assoc 2 value))
      (->>
       (str/join "-"))))

(rf/reg-event-db
 ::update-field
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

(rf/reg-event-fx
 ::on-change-field
 (fn [_ [_ {:keys [form-id
                   field-type
                   field-id
                   value
                   validation-rules
                   date-unit]}]]
   {::fx/dispatch-debounce [(-> (str "update-" (name form-id) "-" (name field-id))
                                keyword)
                            [::update-field
                             {:field-type field-type
                              :form-id form-id
                              :field-id field-id
                              :date-unit date-unit
                              :value value
                              :validation-rules validation-rules}]
                            1000]}))

(rf/reg-event-db
 ::update-dynamic-field
 (fn [db [_ {:keys [form-id
                    ;first-field
                    field-type
                    index
                    field-id
                    value
                    validation-rules
                    date-unit]}]]
   (cond-> db
     ;(= field-id first-field) (assoc-in [:form form-id index] {first-field value})
     (= field-id :field) (assoc-in [:form form-id index] {:field value
                                                          :operator "eq"})
     ;(and (not= field-id first-field)
     (and (not= field-id :field)
          (= field-type "date")) (update-in [:form form-id index field-id]
                                            update-date-str
                                            date-unit
                                            value)
     ;(and (not= field-id first-field)
     (and (not= field-id :field)
          (= field-type "text")) (assoc-in [:form form-id index field-id] value)
     :always (#(let [form-values (get-in % [:form form-id])
                     validation-result (v/validate validation-rules form-values)]
                 (assoc-in % [:form-errors form-id] validation-result))))))

(rf/reg-event-fx
 ::on-change-dynamic-field
 (fn [_ [_ {:keys [form-id
                   index
                   field-id]
            :as params}]]
   {::fx/dispatch-debounce [(-> (str (name form-id) "/" index "/" (name field-id))
                                keyword)
                            [::update-dynamic-field
                             params]
                            1000]}))

(rf/reg-event-db
 ::update-errors
 (fn [db [_ {:keys [form-id validation-rules]}]]
   (let [form-values (get-in db [:form form-id])
         validation-result (v/validate validation-rules form-values)]
     (assoc-in db [:form-errors form-id] validation-result))))

(rf/reg-event-db
 ::submitting-form
 (fn [db [_ {:keys [form-id]}]]
   (assoc-in db [:form-submitting? form-id] true)))

(rf/reg-event-db
 ::close-form
 (fn [db [_ {:keys [form-id]}]]
   (-> db
       (update :form dissoc form-id)
       (update :form-errors dissoc form-id))))

(rf/reg-event-db
 ::add-dynamic-fieldset
 (fn [db [_ {:keys [form-id]}]]
   (update-in db [:form form-id] #(vec (conj %1 %2)) {:field nil})))

(defn drop-index [coll index]
  (vec (concat (subvec coll 0 index)
               (subvec coll (inc index)))))

(rf/reg-event-db
 ::delete-dynamic-fieldset
 (fn [db [_ {:keys [form-id index]}]]
   (update-in db [:form form-id] drop-index index)))

(ns hs-test-app.events
  (:require [re-frame.core :as rf]
            [day8.re-frame.http-fx]
            [ajax.core :refer [json-request-format json-response-format]]
            [hs-test-app.config :as config]
            [hs-test-app.db :as db]
            [hs-test-app.fx :as fx]))

(def request-defaults
  {:timeout 6000
   :response-format (json-response-format {:keywords? true})
   :on-failure [::set-error]})

(rf/reg-event-db
 ::initialize-db
 (fn [_ _]
   db/default-db))

(defmulti on-navigate (fn [view _] view))
(defmethod on-navigate :hs-test-app.views/list [_ _]
  {:fx [[:dispatch [::fetch-patients]]]})
(defmethod on-navigate :hs-test-app.views/edit [_ params]
  {:fx [[:dispatch [::fetch-patient-by-id (:id params)]
         [:dispatch [::init-form {:form-id :patient-reg-form}]]]]})
(defmethod on-navigate :default [_ _] nil)

(rf/reg-event-fx
 ::set-current-route
 (fn [{:keys [db]} [_ {:keys [handler route-params]
                       :as route}]]
   (merge {:db (assoc db :route route)}
          (on-navigate handler route-params))))

(rf/reg-event-fx
 ::navigate
 (fn [_ [_ view params]]
   {::fx/navigate {:view view
                   :params params}}))

(rf/reg-event-db
 ::set-error
 (fn [db [_ res]]
   (assoc db :error res)))

(rf/reg-event-fx
 ::fetch-patients
 (fn [_ _]
   {:http-xhrio (assoc request-defaults
                       :method :get
                       :uri (str config/API_URL "/patients")
                       :on-success [::set-patients])}))

(rf/reg-event-db
 ::set-patients
 (fn [db [_ res]]
   (assoc db
          :patients res
          :patient-in-edit nil)))

(rf/reg-event-fx
 ::fetch-patient-by-id
 (fn [{:keys [db]} [_ patient-id]]
   {:db (assoc db :patient-in-edit nil)
    :http-xhrio (assoc request-defaults
                       :method :get
                       :uri (str config/API_URL "/patients/" patient-id)
                       :on-success [::set-patient-in-edit])}))

(rf/reg-event-db
 ::set-patient-in-edit
 ;(fn [db [_ res]]
 ;  (assoc db :patient-in-edit res)))
 (fn [db [_ res]]
   (-> db
       (assoc :patient-in-edit res)
       (assoc-in [:form :patient-reg-form]
                 (dissoc res :id :created_at :updated_at)))))

(rf/reg-event-fx
 ::create-patient
 (fn [_ [_ {:keys [values]}]]
   {:http-xhrio (assoc request-defaults
                       :method :post
                       :uri (str config/API_URL "/patients")
                       :params values
                       :format (json-request-format)
                       :on-success [::navigate :hs-test-app.views/list])}))

(rf/reg-event-fx
 ::update-patient
 (fn [_ [_ patient-id {:keys [values]}]]
   {:fx [[:http-xhrio (assoc request-defaults
                             :method :put
                             :uri (str config/API_URL "/patients/" patient-id)
                             :params values
                             :format (json-request-format)
                             :on-success [::navigate :hs-test-app.views/list])]
         [:dispatch ::close-form {:form-id :patient-reg-form}]]}))

(rf/reg-event-fx
 ::delete-patient
 (fn [_ [_ patient-id]]
   {:http-xhrio (assoc request-defaults
                       :method :delete
                       :uri (str config/API_URL "/patients/" patient-id)
                       :format (json-request-format)
                       :on-success [::fetch-patients])}))

(rf/reg-event-db
 ::init-form
 (fn [db [_ {:keys [form-id control-id-to-value]}]]
   (assoc-in db [:form form-id] nil)))

(rf/reg-event-db
 ::update-form
 (fn [db [_ {:keys [form-id control-id value]}]]
   (assoc-in db [:form form-id control-id] value)))

;(rf/reg-event-db
; ::save-form
; (fn [db [_ {:keys [patient-id values form-id]}]]
;   (rf/dispatch ::update-patient patient-id {:values values})
;   (assoc-in db [:form form-id] nil)))

(rf/reg-event-db
 ::close-form
 (fn [db [_ {:keys [form-id]}]]
   (update db :form dissoc form-id)))

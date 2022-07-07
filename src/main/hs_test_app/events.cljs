(ns hs-test-app.events
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
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
(defmethod on-navigate :hs-test-app.views/list [_ params]
  {:fx [[:dispatch [::fetch-patients (:keywords params)]]]})
(defmethod on-navigate :hs-test-app.views/edit [_ params]
  {:fx [[:dispatch [::fetch-patient-by-id (:id params)]
         [:dispatch [::init-form {:form-id :patient-reg-form}]]]]})
(defmethod on-navigate :default [_ _] nil)

(rf/reg-event-fx
 ::set-current-route
 (fn [{:keys [db]} [_ {:keys [handler route-params]
                       :as route}]]
   (println route-params)
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
 (fn [_ [_ keywords]]
   {:http-xhrio (assoc request-defaults
                       :method :get
                       :uri (str config/API_URL
                                 "/patients"
                                 (when-not (empty? keywords)
                                   (str "?keywords=" keywords)))
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
                             :on-success [::close-form {:form-id :patient-reg-form}])]
         [:dispatch [::navigate :hs-test-app.views/list]]]}))

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
 (fn [db [_ {:keys [form-id]}]]
   (assoc-in db [:form form-id] {})))

(rf/reg-event-db
 ::update-form
 (fn [db [_ {:keys [form-id control-id value]}]]
   (assoc-in db [:form form-id control-id] value)))

(rf/reg-event-db
 ::close-form
 (fn [db [_ {:keys [form-id]}]]
   (update db :form dissoc form-id)))

;(rf/reg-event-fx
; ::search
; (fn [_ [_ keywords]]
;   {::fx/dispatch-debounce [:search [::fetch-patients keywords] 1000]}))
(rf/reg-event-fx
 ::search
 (fn [_ [_ keywords]]
   {::fx/dispatch-debounce [:search [::navigate 
                                     :hs-test-app.views/list
                                     {:keywords keywords}] 1000]}))

(defn update-date-str [old-str target value]
  (-> old-str
      (str/split #"-")
      (cond->
       (= target :year) (assoc 0 value)
       (= target :month) (assoc 1 value)
       (= target :day) (assoc 2 value))
      (#(str/join "-" %))))

(comment
  (update-date-str "1990-01-01" :month "02"))

(rf/reg-event-db
 ::update-form-date
 (fn [db [_ {:keys [form-id control-id target value]}]]
   (update-in db [:form form-id control-id] update-date-str target value)))

(rf/reg-event-db
 ::update-filter-form
 (fn [db [_ {:keys [form-id index control-id value]}]]
   ;(assoc-in db [:form form-id :vals index control-id] value)))
   (if (= control-id :field)
     (assoc-in db [:form form-id :vals index] {:index index :field value})
     (assoc-in db [:form form-id :vals index control-id] value))))

(rf/reg-event-db
 ::push-vec-form
 (fn [db [_ {:keys [form-id]}]]
   (let [last-index (get-in db [:form form-id :last-index])
         index (if (nil? last-index)
                 0
                 (inc last-index))]
     (if (= index 0)
       (-> db
           (assoc-in [:form form-id :last-index] index)
           (assoc-in [:form form-id :vals] [{:index index}]))
       (-> db
           (assoc-in [:form form-id :last-index] index)
           (update-in [:form form-id :vals]
                      conj
                      {:index index}))))))

(defn drop-one [coll index]
  (vec (concat (subvec coll 0 index)
               (subvec coll (inc index)))))

(rf/reg-event-db
 ::delete-index-from-vec-form
 (fn [db [_ {:keys [form-id index]}]]
   (-> db
       (update-in [:form form-id :vals] drop-one index)
       (update-in [:form form-id :last-index] dec))))

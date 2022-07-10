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
  {:fx [[:dispatch [::fetch-patients
                    (get params :keywords)
                    (get params :filters)]]]})
(defmethod on-navigate :hs-test-app.views/edit [_ params]
  {:fx [[:dispatch [::fetch-patient-by-id (:id params)]]]})
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

(defn make-query-str [keywords filters]
  (->> (cond-> []
         (seq keywords) (conj (str "keywords=" keywords))
         (seq filters) (concat (map #(str "filter[]="
                                          (% :field) ","
                                          (% :operator) ","
                                          (% :value))
                                    filters)))
       (str/join "&")
       (#(when (seq %) (str "?" %)))))

(comment
  (make-query-str "aaa" [{:field "a" :operator "a" :value "a"}
                         {:field "b" :operator "b" :value "b"}])
  (str/join "&" []))

(rf/reg-event-fx
 ::fetch-patients
 (fn [_ [_ keywords filters]]
   {:http-xhrio (assoc request-defaults
                       :method :get
                       ;:uri (str config/API_URL
                       ;          "/patients"
                       ;          (when-not (empty? keywords)
                       ;            (str "?keywords=" keywords)))
                       :uri (str config/API_URL
                                 "/patients"
                                 (make-query-str keywords filters))
                       :on-success [::set-patients])}))

(rf/reg-event-db
 ::set-patients
 (fn [db [_ res]]
   (assoc db
          :patients res
          :patient-in-edit nil)))

;(rf/reg-event-fx
; ::search
; (fn [_ [_ keywords]]
;   {::fx/dispatch-debounce [:search [::fetch-patients keywords] 1000]}))
(rf/reg-event-fx
 ::search-patients
 (fn [_ [_ {:keys [keywords filters]}]]
   {::fx/dispatch-debounce [:search-patients
                            [::navigate
                             :hs-test-app.views/list
                             {:keywords keywords
                              :filters filters}] 1000]}))

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
                       :on-success [::navigate :hs-test-app.views/list])}))

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
   {:fx [[:dispatch [::navigate :hs-test-app.views/list]]
         [:dispatch [::close-form {:form-id :patient-reg-form}]]]}))

(rf/reg-event-fx
 ::delete-patient
 (fn [_ [_ patient-id]]
   {:http-xhrio (assoc request-defaults
                       :method :delete
                       :uri (str config/API_URL "/patients/" patient-id)
                       :format (json-request-format)
                       :on-success [::fetch-patients])}))

;(rf/reg-event-db
; ::init-form
; (fn [db [_ {:keys [form-id]}]]
;   (assoc-in db [:form form-id] {})))

(rf/reg-event-db
 ::update-control
 (fn [db [_ {:keys [form-id control-id value]}]]
   (assoc-in db [:form form-id control-id] value)))

(defn update-date-str [old-str target value]
  (-> old-str
      (#(if (empty? %) "--" %))
      (str/split #"-" -1)
      (cond->
       (= target :year) (assoc 0 value)
       (= target :month) (assoc 1 value)
       (= target :day) (assoc 2 value))
      (#(str/join "-" %))))

(rf/reg-event-db
 ::update-date-control
 (fn [db [_ {:keys [form-id control-id target value]}]]
   (update-in db [:form form-id control-id] update-date-str target value)))

(rf/reg-event-db
 ::close-form
 (fn [db [_ {:keys [form-id]}]]
   (update db :form dissoc form-id)))

(rf/reg-event-db
 ::add-dynamic-fieldset
 (fn [db [_ {:keys [form-id]}]]
   (if (vector? (get-in db [:form form-id]))
     (update-in db [:form form-id] conj {:field nil})
     (assoc-in db [:form form-id] [{:field nil}]))))

(rf/reg-event-db
 ::upsert-dynamic-control
 (fn [db [_ {:keys [form-id index control-id value]}]]
   (if (= control-id :field)
     (assoc-in db [:form form-id index] {:field value})
     (assoc-in db [:form form-id index control-id] value))))

(rf/reg-event-db
 ::upsert-dynamic-date-control
 (fn [db [_ {:keys [form-id index control-id target value]}]]
   ;(cond
   ;  (= control-id :field) (assoc-in db [:form form-id index] {:field value})
   ;  (= control-id :operator) (assoc-in db [:form form-id index control-id] value)
   ;  (= control-id :value)
   (update-in db [:form form-id index control-id] update-date-str target value)));)

(defn drop-index [coll index]
  (vec (concat (subvec coll 0 index)
               (subvec coll (inc index)))))

(rf/reg-event-db
 ::delete-dynamic-fieldset
 (fn [db [_ {:keys [form-id index]}]]
   (update-in db [:form form-id] drop-index index)))

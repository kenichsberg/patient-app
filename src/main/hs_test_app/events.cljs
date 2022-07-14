(ns hs-test-app.events
  (:require [re-frame.core :as rf]
            [clojure.string :as str]
            [day8.re-frame.http-fx]
            [route-map.core :as route-map]
            [ajax.core :refer [json-request-format json-response-format]]
            [hs-test-app.config :as config]
            [hs-test-app.db :as db]
            [hs-test-app.routes :refer [routes]]
            [hs-test-app.fx :as fx]))

(def request-defaults
  {:timeout 6000
   :response-format (json-response-format {:keywords? true})
   :on-failure [::set-error]})

(rf/reg-event-db
 ::initialize-db
 (fn [_ _]
   db/default-db))

(defn parse-query-string [q-str]
  (let [s (str/replace q-str #"^\?" "")]
    (when (seq s)
      (->> (str/split s #"&")
           (reduce (fn [acc s]
                     (let [[k v] (str/split s #"=")
                           v (cond
                               (empty? v) ""
                               (str/includes? v ",") (->> (str/split v #",")
                                                          (map js/decodeURIComponent))
                               :else (js/decodeURIComponent v))]
                       (if (nil? (re-find #"\[\]$" k))
                         (assoc acc (keyword k) v)
                         (let [kw (-> k
                                      (str/replace #"\[\]$" "")
                                      (keyword))]
                           (update acc kw #(conj (vec %1) %2) v)))))
                   {})))))

(comment
  (parse-query-string "?a")
  (parse-query-string ""))

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
         query-str (.. js/window -location -search)
         matched (route-map/match path routes)
         route (:match matched)
         path-params (if (empty? (:params matched)) nil (:params matched))
         query-params (parse-query-string query-str)]
     (println route path-params query-params)
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

(defn gen-query-string [keywords filters]
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
  (gen-query-string "aaa" [{:field "a" :operator "a" :value "a"}
                           {:field "b" :operator "b" :value "b"}])
  (str/join "&" []))

(rf/reg-event-fx
 ::fetch-patients
 (fn [_ [_ keywords filters]]
   {:http-xhrio (assoc request-defaults
                       :method :get
                       :uri (str config/API_URL
                                 "/patients"
                                 (gen-query-string keywords filters))
                       :on-success [::set-patients])}))

(rf/reg-event-db
 ::set-patients
 (fn [db [_ res]]
   (assoc db
          :patients res
          :patient-in-edit nil)))

(rf/reg-event-fx
 ::search-patients
 ;; @TODO first wait input, then push state
 (fn [_ [_ {:keys [keywords filters]}]]
   {::fx/dispatch-debounce [:search-patients
                            [::push-state
                             :patients
                             nil
                             {:keywords keywords
                              :filters filters}]
                            1000]}))

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

(rf/reg-event-db
 ::update-control
 (fn [db [_ {:keys [form-id control-id value]}]]
   (assoc-in db [:form form-id control-id] value)))

(defn update-date-str [old-str target value]
  (-> (if (empty? old-str) "--" old-str)
      (str/split #"-" -1)
      (cond->
       (= target :year) (assoc 0 value)
       (= target :month) (assoc 1 value)
       (= target :day) (assoc 2 value))
      (->>
       (str/join "-"))))

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
   (update-in db [:form form-id index control-id] update-date-str target value)));)

(defn drop-index [coll index]
  (vec (concat (subvec coll 0 index)
               (subvec coll (inc index)))))

(rf/reg-event-db
 ::delete-dynamic-fieldset
 (fn [db [_ {:keys [form-id index]}]]
   (update-in db [:form form-id] drop-index index)))

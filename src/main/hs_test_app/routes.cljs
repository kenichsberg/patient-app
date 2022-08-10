(ns hs-test-app.routes
  (:require [re-frame.core :as rf]))

(def routes
  {"patients" {:. :hs-test-app.views/list
               "create" :hs-test-app.views/create
               [:id] {"edit" :hs-test-app.views/edit}}})

;(defn on-popstate [route path-params query-params]
(defn on-popstate [js-event path-params query-params]
  ;(prn "route: " (. js/JSON stringify route nil 4)) 
  ;(. js/console table route) 
  ;(. js/console table js/window.history) 
  ;(prn (.. route state))
  (prn "path: " path-params)
  (prn "query(on-popstate):" query-params)
  ;(when route
  ;  (rf/dispatch [:hs-test-app.events/navigated 
  ;                [route path-params query-params]])))
  ;(prn "(:state event): " (:state (js->clj js-event :keywordize-keys true)))
  (. js/console log js-event)
  (. js/console log js-event.state)
  (. js/console log js-event.state.fqn)
  (prn "route: " (->  (js->clj js-event :keywordize-keys true)
                      :state))
  (prn "route2: " (keyword js-event.state.fqn))
  ;(prn "event.state: " event.state)
  ;(let [event (js->clj js-event.state :keywordize-keys true)
  ;(let [event (js->clj js-event :keywordize-keys true)
  (let [route (keyword js-event.state.fqn)]
    (rf/dispatch [:hs-test-app.events/navigated
                  [route path-params query-params]])))

(def routes2
  {"admin" {"users" {:. 'users-list-view
                     [:id] 'user-view}
            "groups" 'groups-list-view}})

(def routes3
  {:.name :root
   "admin" {:.name :admin
            "users" {:.name :admin-user
                     :. 'users-list-view
                     [:id] 'user-view}
            "groups" {:.name :admin-group
                      :. 'groups-list-view}}})

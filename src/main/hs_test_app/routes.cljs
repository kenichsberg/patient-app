(ns hs-test-app.routes
  (:require [re-frame.core :as rf]))

(def routes
  {"patients" {:. :hs-test-app.views/list
               "create" :hs-test-app.views/create
               [:id] {"edit" :hs-test-app.views/edit}}})

;(defn on-popstate [js-event path-params query-params]
(defn on-popstate [js-event path-params query-params url]
  (prn "path: " path-params)
  (prn "query(on-popstate):" query-params)
  (. js/console log js-event)
  (. js/console log js-event.state)
  (. js/console log js-event.state.fqn)
  (prn "route: " (->  (js->clj js-event :keywordize-keys true)
                      :state))
  (prn "route2: " (keyword js-event.state.fqn))
  (let [resource (keyword js-event.state.fqn)]
    (rf/dispatch [:hs-test-app.events/navigated
                  [resource path-params query-params url]])))

(ns hs-test-app.routes
  (:require [re-frame.core :refer [dispatch]]))

(def routes
  {"patients" {:. :hs-test-app.views/list
               "create" :hs-test-app.views/create
               [:id] {"edit" :hs-test-app.views/edit}}})

(defn on-popstate [js-event path-params query-params url]
  (. js/console log js-event)
  (. js/console log js-event.state)
  (let [resource (keyword js-event.state.fqn)]
    (dispatch [:navigated [resource path-params query-params url]])))

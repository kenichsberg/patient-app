(ns hs-test-app.routes
  (:require [re-frame.core :as rf]))

(def routes
  {"patients" {:. :hs-test-app.views/list
               "create" :hs-test-app.views/create
               [:id] {"edit" :hs-test-app.views/edit}}})

(defn on-popstate [js-event path-params query-params url]
  (let [resource (keyword js-event.state.fqn)]
    (rf/dispatch [:hs-test-app.events/navigated
                  [resource path-params query-params url]])))

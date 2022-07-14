(ns hs-test-app.routes
  (:require [re-frame.core :as rf]))

(def routes
  {"patients" {:. :hs-test-app.views/list
               "create" :hs-test-app.views/create
               [:id] {"edit" :hs-test-app.views/edit}}})

(defn on-popstate [route path-params query-params]
  (println route path-params query-params)
  (when route
    (rf/dispatch [:hs-test-app.events/navigated 
                  [route path-params query-params]])))

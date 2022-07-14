(ns hs-test-app.routes
  (:require ;[accountant.core :as accountant]
            ;[bidi.bidi :as bidi]))
   [re-frame.core :as rf]
   [route-map.core :as route-map]))

;(def routes
;  ["/" {"patients" :hs-test-app.views/list
;        "patients/create" :hs-test-app.views/create
;        ["patients/" [#"\d+" :id] "/edit"] :hs-test-app.views/edit}])
;
;(defn navigate
;  ([view] (navigate view {}))
;  ([view params]
;   (accountant/navigate! (apply bidi/path-for
;                                routes
;                                view
;                                (apply concat params)))))

(def routes
  {"patients" {:. :hs-test-app.views/list
               "create" :hs-test-app.views/create
               [:id] {"edit" :hs-test-app.views/edit}}})

(defn on-popstate [route path-params query-params]
  (println route path-params query-params)
  (when route
    (rf/dispatch [:hs-test-app.events/navigated 
                  [route path-params query-params]])))

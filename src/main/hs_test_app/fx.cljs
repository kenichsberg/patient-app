(ns hs-test-app.fx
  (:require [goog.events :as gevents]
            [clojure.string :as str]
            [re-frame.core :as rf]
            [route-map.core :as route-map]
            [hs-test-app.routes :as routes]
            [hs-test-app.utils :as utils]))

(defonce popstate-listener
  (atom ""))

(rf/reg-fx
 ::init-popstate-listener
 (fn []
   (gevents/unlistenByKey @popstate-listener)
   (reset! popstate-listener
           (gevents/listen js/window
                           goog.events.EventType.POPSTATE
                           routes/on-popstate
                           false))))

(comment
  (route-map/url routes/routes :patients-edit {:id 5})
  (route-map/url routes/routes :patients-create)
  (route-map/url routes/routes :create-patient)
  (route-map/url routes/routes :patients nil)
  (route-map/url routes/routes2 :admin-groups nil)
  (route-map/url routes/routes2 :groups nil)
  (route-map/url routes/routes2 :admin-users {:id 1})
  (route-map/url routes/routes3 :admin-group nil)
  (:match (route-map/match "patients/5/edit" routes/routes))
  (:match (route-map/match "patients/create" routes/routes))
  (:match (route-map/match "admin/groups" routes/routes2))
  (:match (route-map/match "admin/groups" routes/routes3))
  (utils/map->querystr {:keywords "a" :filters [["gender" "eq" "true"] ["address" "gt" "N.Y."]]}))

(rf/reg-fx
 ::push-state
 (fn [[route-name path-params query-params]]
   (prn "route-name: " route-name)
   (prn "path-params: " path-params)
   (let [path (route-map/url routes/routes route-name path-params)
         view-key (:match (route-map/match path routes/routes))
         q-str (utils/map->querystr query-params)
         url (str path q-str)]
     ;; @TODO compare current path and next one
     ;(.pushState js/window.history nil "" url)
     ;(routes/on-popstate view-key path-params query-params))))
     (.pushState js/window.history view-key "" url)
     (prn "dummy-event: " (js-obj "state" view-key))
     (routes/on-popstate (js-obj "state" view-key) path-params query-params))))

(defonce timeouts
  (atom {}))

(rf/reg-fx
 ::dispatch-debounce
 (fn [[id event-vec n]]
   (js/clearTimeout (@timeouts id))
   (swap! timeouts assoc id
          (js/setTimeout (fn []
                           (rf/dispatch event-vec)
                           (swap! timeouts dissoc id))
                         n))))

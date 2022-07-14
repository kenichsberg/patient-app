(ns hs-test-app.fx
  (:require [goog.events :as gevents]
            [re-frame.core :as rf]
            [route-map.core :as route-map]
            [hs-test-app.routes :as routes]))

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
  (route-map/url routes/routes :patients nil)
  (:match (route-map/match "patients/5/edit" routes/routes)))

(rf/reg-fx
 ::push-state
 (fn [[route-name path-params query-params]]
   (let [url (route-map/url routes/routes route-name path-params)
         view-key (:match (route-map/match url routes/routes))]
     ;; @TODO compare current path and next one
     (.pushState js/window.history nil "" url)
     (routes/on-popstate view-key path-params query-params))))

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

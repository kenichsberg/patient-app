(ns hs-test-app.fx
  (:require [goog.events :as gevents]
            [re-frame.core :as rf]
            [route-map.core :as route-map]
            [hs-test-app.routes :as routes]
            [hs-test-app.utils :as utils]))

;;
;;
;; An atom which grasps the unique key of a popstate event listener.
;;
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
  (:match (route-map/match "patients/5/edit" routes/routes))
  (:match (route-map/match "patients/create" routes/routes))
  (:match (route-map/match "patients?keywords=a" routes/routes))
  (utils/map->querystr {:keywords "a" :filters [["gender" "eq" "true"] ["address" "gt" "N.Y."]]}))

(defn push-state! [resource path-params query-params url]
  (.pushState js/window.history resource "" url)
  ;; history.pushState() call doesn't invoke a popstate event,
  ;; so manually needed to trigger it.
  (routes/on-popstate (js-obj "state" resource) path-params query-params url))

(rf/reg-fx
 ::trigger-navigation
 ;; NOTE 
 ;; Since route-map/url seems to behave inconsistently,
 ;; inevitable to use only route-map/match.
 ;; For this reason, the 1st argment of this func isn't to be a page resource,
 ;; but a URL path without query string (which isn't an efficient way,
 ;; because it requires to include path-params values as a part of URL path string,
 ;; and then, should parse it to get page resource and path-params).
 (fn [[base-path query-params old-url]]
   (let [q-str (utils/map->querystr query-params)
         url (str base-path q-str)]
     ;; Proceed only when the new URL is different from the old one.
     (when-not (= url old-url)
       (let [matched (route-map/match base-path routes/routes)
             resource (:match matched)
             path-params (:params matched)]
         (push-state! resource path-params query-params url))))))

;;
;;
;; An atom for debounce 
;;
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

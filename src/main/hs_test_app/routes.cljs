(ns hs-test-app.routes
  (:require [goog.events :as gevents]
            [re-frame.core :refer [reg-cofx reg-fx dispatch]]
            [route-map.core :as route-map]
            [hs-test-app.utils :as utils]))

(def routes
  {"patients" {:. :hs-test-app.views/list
               "create" :hs-test-app.views/create
               [:id] {"edit" :hs-test-app.views/edit}}})

(defn on-popstate [js-event path-params query-params url]
  (. js/console log js-event)
  (. js/console log js-event.state)
  (let [resource (keyword js-event.state.fqn)]
    (dispatch [:navigated [resource path-params query-params url]])))

;;
;;
;; Grasping the unique key of a popstate event listener.
;;
(defonce popstate-listener
  (atom ""))

(reg-fx
 :init-popstate-listener
 (fn []
   (gevents/unlistenByKey @popstate-listener)
   (reset! popstate-listener
           (gevents/listen js/window
                           goog.events.EventType.POPSTATE
                           on-popstate
                           false))))

(defn push-state! [resource path-params query-params url]
  (.pushState js/window.history resource "" url)
  ;; history.pushState() call doesn't invoke a popstate event,
  ;; so manually needed to trigger it.
  (on-popstate (js-obj "state" resource) path-params query-params url))

(reg-fx
 :navigation
 ;; Since route-map/url seems to behave inconsistently,
 ;; inevitable to use only route-map/match.
 ;; For this reason, the 1st argment of this func isn't to be a page resource,
 ;; but a URL path without query string (which isn't the most efficient way,
 ;; because it requires to include path-params values as a part of URL path string,
 ;; and then, should parse it to get page resource and path-params).
 (fn [[base-path query-params old-url]]
   (let [q-str (utils/map->querystr query-params)
         url (str base-path q-str)]
     ;; Proceed only when the new URL is different from the old one.
     (when-not (= url old-url)
       (let [matched (route-map/match base-path routes)
             resource (:match matched)
             path-params (:params matched)]
         (push-state! resource path-params query-params url))))))

(reg-cofx
 :raw-url
 (fn [cofx _]
    ;(let [path (.. js/window -location -pathname)
    ;     q-str (.. js/window -location -search)
    ;     ;matched (route-map/match path routes)
    ;     ;route (:match matched)
    ;     ;path-params (if (empty? (:params matched)) nil (:params matched))
    ;     ;query-params (utils/querystr->map q-str)
    ;     ]
    ;  (assoc cofx :initial-url (str path q-str) )
   (let [href (.. js/window -location -href)]
     (assoc cofx :raw-url href))))

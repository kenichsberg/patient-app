(ns hs-test-app.core
  (:require [accountant.core :as accountant]
            [bidi.bidi :as bidi]
            [re-frame.core :as rf]
            [reagent.dom :as rdom]
            [hs-test-app.config :as config]
            [hs-test-app.events :as events]
            [hs-test-app.routes :refer [routes]]
            [hs-test-app.views :as views]))

(defn dev-setup []
  (when config/debug?
    (enable-console-print!)
    (println "dev mode")))

(defn mount-root []
  (rf/clear-subscription-cache!)
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [views/main-panel] root-el)))

(defn ^:export init []
  (rf/dispatch-sync [::events/initialize-db])
  (accountant/configure-navigation!
   {:nav-handler (fn [path]
                   (rf/dispatch [::events/set-current-route
                                 (bidi/match-route routes path)]))
    :path-exists? (fn [path]
                    (boolean (bidi/match-route routes path)))
    :reload-same-path? true})
  (accountant/dispatch-current!)
  (dev-setup)
  (mount-root))

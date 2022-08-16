(ns hs-test-app.core
  (:require [re-frame.core :as rf]
            [reagent.dom :as rdom]
            [hs-test-app.config :as config]
            [hs-test-app.events]
            [hs-test-app.subs]
            [hs-test-app.fx]
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
  (rf/dispatch-sync [:initialize-db])
  (rf/dispatch [:init-routes])
  (dev-setup)
  (mount-root))

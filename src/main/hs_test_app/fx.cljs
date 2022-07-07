(ns hs-test-app.fx
  (:require [re-frame.core :as rf]
            [hs-test-app.routes :as routes]))

(rf/reg-fx
 ::navigate
 (fn [{:keys [view params]}]
   (routes/navigate view params)))

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

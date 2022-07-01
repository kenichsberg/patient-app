(ns hs-test-app.fx
  (:require [re-frame.core :as rf]
            [hs-test-app.routes :as routes]))

(rf/reg-fx
 ::navigate
 (fn [{:keys [view params]}]
   (routes/navigate view params)))

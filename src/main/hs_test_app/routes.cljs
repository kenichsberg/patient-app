(ns hs-test-app.routes
  (:require [accountant.core :as accountant]
            [bidi.bidi :as bidi]))

(def routes
  ["/" {"patients" :hs-test-app.views/list
        "patients/create" :hs-test-app.views/create
        ["patients/" [#"\d+" :id] "/edit"] :hs-test-app.views/edit}])

(defn navigate
  ([view] (navigate view {}))
  ([view params]
   (accountant/navigate! (apply bidi/path-for
                                routes
                                view
                                (apply concat params)))))

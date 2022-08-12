(ns hs-test-app.utils
  (:require [clojure.string :as str]))

;;
;; -------------------------------
;; query string <---> map
;; -------------------------------

(defn querystr->map [q-str]
  (let [s (str/replace q-str #"^\?" "")]
    (when (seq s)
      (->> (str/split s #"&")
           (reduce (fn [acc s]
                     (let [[k v] (str/split s #"=")
                           v (cond
                               (empty? v) ""
                               (str/includes? v ",") (->> (str/split v #",")
                                                          (map js/decodeURIComponent))
                               :else (js/decodeURIComponent v))]
                       (if (nil? (re-find #"\[\]$" k))
                         (assoc acc (keyword k) v)
                         (let [kw (-> k
                                      (str/replace #"\[\]$" "")
                                      (keyword))]
                           (update acc kw #(conj (vec %1) %2) v)))))
                   {})))))

(defn map->querystr [q-params]
  (->> q-params
       (reduce (fn [acc [k v]]
                 (let [v (if (coll? v) v (str v))]
                   (if (empty? v)
                     acc
                     (let [k (if (not (vector? v))
                               (name k)
                               (-> (name k)
                                   (str "[]")
                                   js/encodeURIComponent))]
                       (if (not (vector? v))
                         (conj acc (str k "=" (js/encodeURIComponent v)))
                         (let  [vs (map #(if (not (vector? %))
                                           (str k "=" (js/encodeURIComponent %))
                                           (str k "=" (->> %
                                                           (map js/encodeURIComponent)
                                                           (str/join ",")
                                                           js/encodeURIComponent)))
                                        v)]
                           (concat acc vs)))))))
               [])
       (str/join "&")
       (#(when (seq %) (str "?" %)))))

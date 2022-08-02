(ns hs-test-app.validation
  ;(:require [clojure.string :as str])
  #?(:clj (:import (java.time LocalDate)
                   (java.time.format DateTimeFormatter
                                     DateTimeParseException
                                     ResolverStyle))))

(defn- keys-in [m]
  (letfn [(children [ks]
            (let [v (get-in m ks)]
              (if (map? v)
                (map #(conj ks %) (keys v))
                [])))
          (branch? [ks]
            (-> (children ks) seq boolean))]
    (->> (keys m)
         (map vector)
         (mapcat #(tree-seq branch? children %))
         (reduce (fn [acc v]
                   (let [ks-coll (loop [ks v
                                        result []]
                                   (if (= (count ks) 0)
                                     result
                                     (recur (pop ks)
                                            (conj result ks))))]
                     (conj (-> (remove #(some #{%} ks-coll) acc)
                               vec)
                           v)))
                 []))))

;;
;;
;; params
;;   validations - A vector whose value is also a vector which is the pair of validator function and its args.
;;    e.g. [[v/require "1234567"] 
;;          [v/numerical "1234567"]]
;;
;; returns
;;   error-map-vector - A map including fieldname and vector of errors' discriptions.
;;    e.g.
;;    [{:field :zip-code
;;      :message "zip code is required."}
;;     {:field :zip-code
;;      :message "Incorrect number of digits."]} ]
;;
;;   * if there are no errors, returns nil.
;;
(defn- validate-per-field [validations]
  (->> validations
       (reduce (fn [acc [validator & args]]
       ;(reduce (fn [acc validation]
                 (prn "validator: " validator)
                 (prn "args: " args)
                 (let [{:keys [valid? field message] :as validated} (apply validator args)]
                 ;(let [{:keys [valid? field message]} (doall validation)]
                   (if valid?
                     acc
                     (conj acc {:field field
                                :message message}))))
               [])
       (#(when (seq %) %))))

;;
;;
;; params
;;   k-to-rules - A map whose keys are field names and values are validation rules 
;;    e.g. {:zip-code [[v/required "Zip Code"] [v/zip-code]]
;;          :address  [[v/required "Address"]]}
;;
;;   k-to-v - A map whose keys are field names and values are form values 
;;    e.g. {:zip-code 1234567
;;          :address  "N.Y. St.1"}
;;
;; returns
;;   error-map-vector - A map including fieldname and vector of errors' discriptions
;;    e.g.
;;    [{:field :zip-code
;;      :message "Zip Code is required."}
;;     {:field :zip-code
;;      :message "Incorrect number of digits."}
;;     {:field :address
;;      :message "Address is required."}]
;;
;;   * if there are no errors, returns nil.
;;
(defn- validate-map [k-to-rules k-to-v]
  (let [ks-coll (keys-in k-to-rules)]
    (->> ks-coll
         (reduce (fn [acc ks]
                   ;(prn "acc: " acc)
                   ;(prn "ks: " ks)
                   ;(prn "rules: " (get-in k-to-rules ks))
                   (let [rules (get-in k-to-rules ks)
                         validations (->> rules
                                          (mapv (fn [rule-args]
                                                  (let [ks*         (if (= (count ks) 1)
                                                                      (first ks)
                                                                      ks)
                                                        fixed-args  (-> []
                                                                        (conj ks*)
                                                                        (conj (get-in k-to-v ks)))
                                                        additional-args (vec (rest rule-args))
                                                        rule (first rule-args)]
                                                    ;(prn "rule-args: " rule-args)
                                                    ;(prn "ks*" ks*)
                                                    ;(prn "fixed-args" fixed-args)
                                                    ;(prn "additional-args" additional-args)
                                                    ;(prn "rule: " rule)
                                                    (concat [rule] fixed-args additional-args)))))]
                     ;(prn "rules: " rules)
                     ;(prn "validations: " validations)
                     (if-let [error-maps (validate-per-field validations)]
                       ;(or (prn "aaa" (apply conj acc error-maps)) (apply conj acc error-maps))
                       (apply conj acc error-maps)
                       acc)))
                 [])
         (#(when (seq %) %)))))

;;
;;
;; params
;;   k-to-rules - A map whose keys are field names and values are validation rules 
;;    e.g. {:zip-code [[v/required "Zip Code"] [v/zip-code]]
;;          :address  [[v/required "Address"]]}
;;
;;   k-to-vs - A vector of maps whose keys are field names and values are form values 
;;    e.g. [{:zip-code 1111111
;;           :address  "N.Y. St.1"}
;;          {:zip-code 2222222
;;           :address  "Calfornia St.2"}]
;;
;; returns
;;   error-map-vector - A map including fieldname and vector of errors' discriptions
;;    e.g.
;;    [[{:field :zip-code
;;       :message "Zip Code is required."}
;;      {:field :zip-code
;;       :message "Incorrect number of digits."}
;;      {:field :address
;;       :message "Address is required."}]
;;     [{:field :zip-code
;;       :message "Zip Code is required."}
;;      {:field :zip-code
;;       :message "Incorrect number of digits."}
;;      {:field :address
;;       :message "Address is required."}]]
;;
;;   * if there are no errors, returns nil.
;;
(defn validate-maps [k-to-rules k-to-vs]
  (->> k-to-vs
       (reduce (fn [acc m]
                 (if-let [error-maps (validate-map k-to-rules m)]
                   (conj acc error-maps)
                   acc))
               [])
       (#(when (seq %) %))))

(defn validate-map-by-fn [validator k-to-vs & args]
  (let [{:keys [valid? field message]} (apply validator k-to-vs args)]
    (when (false? valid?) {:field field
                           :message message})))

(defn validate-maps-by-fn [validator k-to-vs & args]
  (->> k-to-vs
       (reduce (fn [acc m]
                 (if-let [error-map (apply validate-map-by-fn validator m args)]
                   (conj acc [error-map])
                   acc))
               [])
       (#(when (seq %) %))))

(defn validate [k-to-rules k-to-vs & args]
  ;(prn "1st: " k-to-rules)
  ;(prn "2nd: " k-to-vs)
  ;(prn "rest: " args)
  (cond
    (and (not (map? k-to-rules))
         (not (fn? k-to-rules))) (throw #?(:clj  (Exception. "1st argument should be a map or validator function.")
                                           :cljs (js/Error.  "1st argument should be a map or validator function.")))
    (and (map? k-to-rules)
         (map? k-to-vs)) (validate-map k-to-rules k-to-vs)
    (and (map? k-to-rules)
         (vector? k-to-vs)) (validate-maps k-to-rules k-to-vs)
    (and (fn? k-to-rules)
         (map? k-to-vs)) (apply validate-map-by-fn k-to-rules k-to-vs args)
    (and (fn? k-to-rules)
         (vector? k-to-vs)) (apply validate-maps-by-fn k-to-rules k-to-vs args)
    :else (throw #?(:clj  (Exception. "2nd argument should be a map or a vector of maps")
                    :cljs (js/Error.  "2nd argument should be a map or a vector of maps")))))

(defn required [field value label]
  {:valid? (-> value str seq boolean)
   :field  field
   :message (str label " is required.")});)

;(defn boolean-string [field value]
;  {:valid? (some? (some #{(str/lower-case value)} ["true" "false"]))
;   :field  field
;   :message "Invalid value"});)

(defn numerical [field value]
  {:valid? (re-find #"^\d+$" value)
   :field  field
   :message "Please input numbers."});)

(defn valid-date-string [field value]
  {:valid? #?(:clj (try
                     (. LocalDate parse
                        value
                        (-> (. DateTimeFormatter ofPattern "uuuu-M-d")
                            (.withResolverStyle (. ResolverStyle STRICT))))
                     true
                     (catch DateTimeParseException _
                       false))
              :cljs (not (-> value js/Date. js/isNaN)))
   :field  field
   :message "Invalid date."})

(defn health-insurance-number [field value]
  {:valid? (some? (re-find #"^\d{12}$" value))
   :field field
   :message "Incorrect number of digits."})

(defn valid-filter [m]
  (let [get-error (fn [{:keys [field operator value]}]
                    (cond
                      (empty? field) {:field "field"
                                      :error-type :required}
                      (empty? operator) {:field "operator"
                                         :error-type :required}
                      (empty? value) {:field "value"
                                      :error-type :required}
                      (and (= field "birth")
                           (false? (:valid?
                                    (valid-date-string nil value)))) {:field "value"
                                                                      :error-type :invalid-date}
                      :else nil))
        error (get-error m)
        message-map {:required (str (:field error) " is required.")
                     :invalid-date "Invalid date."}]
    (prn error)
    {:valid? (nil? error)
     :field (:field error)
     :message (get message-map (:error-type error))}))

(comment
  (validate-per-field [[required :name ""]])
  (validate-map {:name required} {:name ""})
  (validate {:name required} {:name ""})
  (def k-to-vs {:first_name ""
                :last_name "b"
                :gender true
                :birth "2020-01-01"
                :address "Moscow"
                :health_insurance_number "0123456789012"})
  (def k-to-rules {:first_name [[required "first name"]]
                   :last_name [[required "last name"]]
                   :gender [[required "gender"]]
                   :birth [[required "birth"] [valid-date-string]]
                   :address [[required "address"]]
                   :health_insurance_number [[required "health insurance number"]
                                             [health-insurance-number]]})
  (validate k-to-rules k-to-vs)
  (def k-to-vs' [{:field "gender"
                  :operator "eq"
                  :value "true"}
                 {:field "birth"
                  :operator "eq"
                  :value "220-01-01"}
                 {:field "last_name"
                  :operator "gt"
                  :value ""}])
  (def k-to-vs'' [{:field "last_name"}])
  (valid-filter {:field "last_name"
                 :operator "gt"
                 :value ""})
  (validate  valid-filter k-to-vs'')
  (apply required '(:first_name "" "first name"))
  (required :first_name "" "first name"))

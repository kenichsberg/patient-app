(ns hs-test-app.validation
  #?(:clj (:import (java.time LocalDate)
                   (java.time.format DateTimeFormatter
                                     DateTimeParseException
                                     ResolverStyle))))

;;
;; -------------------------------
;; validation
;; -------------------------------

(defprotocol Validator
  (validate-one [this]))
(defrecord Rule [rulename predicate error-msg])
(defrecord Validation [rule args]
  Validator
  (validate-one [this]
    (apply (get-in this [:rule :predicate]) (:args this))))

;(defmacro defvalidrule
;  [rulename & args]
;  (let [{:keys [predicate error-msg docstring]} (first args)]
;    `(do (def ~rulename (with-meta (Rule. (keyword ~rulename)
;                                                 ~predicate
;                                                 ~error-msg)
;                          ~docstring)))))

;(defmacro defvalidrule
;  [{:keys [predicate error-message docstring]}]
;  `(with-meta (Rule. ~predicate ~error-message)
;     ~(meta &form)))

(defn create-rule
  [{:keys [rulename predicate error-message]}]
  (Rule. rulename predicate error-message))

(defn validate-per [validations]
  (reduce (fn [acc [{:keys [rulename error-msg] :as rule} args]]
            (let [validation (Validation. rule args)]
              (if (validate-one validation)
                acc
                (conj acc {:error-name rulename
                           :message error-msg}))))
          []
          validations))

(defn- keys-in [m]
  (letfn [(children [ks]
            (let [v (get-in m ks)]
              (if (not= (type v) hs-test-app.validation/Rule)
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

(defn keys-in2 [m]
  (let [walk (fn walk [ks]
               (lazy-seq
                (concat ks
                        (mapcat (fn [k]
                                  (let [v (get-in m k)]
                                    (if (not= (type v) hs-test-app.validation/Rule)
                                      (->> (keys (get-in m k))
                                           (mapv #(conj k %))
                                           walk)
                                      [])))
                                ks))))]
    (->> (keys m)
         (mapv vector)
         walk
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
;; Private function
;;
;; Targeting for one field of input, executes Validator protocol method "validate-one"
;; with multiple rules in argument "validations".
;;
;; params
;;   ks - A vector of keys to where the value to be validated,
;;        which is used for "get-in" function.
;;    e.g. [:person :address :zip-code]
;;
;;   validations - A vector of constructed Validation records.
;;    e.g. [(Validation. v/require ["1234567"]) 
;;          (Validation. v/numerical ["1234567"])]
;;
;;   field-formatter
;;
;; returns
;;   error-map - A map including fieldname and vector of errors' discriptions
;;    e.g.
;;    {:field [:person :address :zip-code]
;;     :errors [{:error-name "requrired"
;;               :message    "this field is required"}]}
;;
;;   * if there are no errors, returns nil.
;;
(defn- validate-per-field [ks validations & {:keys [field-formatter]
                                             :or {field-formatter nil}}]
  (let [field-formatter (if (nil? field-formatter)
                          #(if (= (count %) 1) (first %) %)
                          field-formatter)]
    (->> validations
         (reduce (fn [acc {:keys [rule] :as validation}]
                   (if (validate-one validation)
                     acc
                     (let [{:keys [rulename error-msg]} rule]
                       (update acc :errors conj {:error-name rulename
                                                 :message error-msg}))))
                 {:field (field-formatter ks)
                  :errors []})
         (#(when (seq (get % :errors)) %)))))

;;
;;
;; Private function
;;
;; Executes all received validations.
;;
;; params
;;   key-to-rules - A map which is the same structure as key-to-values,
;;                  and instead of values, it should have Rule objects 
;;                  (or vectors of them).
;;    e.g. {:person {:address {:zip-code [v/required v/numerical]
;;                             :city v/required
;;                             :street v/required}
;;
;;   key-to-values - A map where there are values to be validated.
;;    e.g. {:person {:address {:zip-code 1234567
;;                             :city "N.Y."
;;                             :street "1st St."}
;;
;;   field-formatter
;;
;; returns
;;   error-maps - A vector of maps including fieldname and vector of errors' discriptions
;;    e.g.
;;    [{:field [:person :address :zip-code]
;;      :errors [{:error-name "numerical"
;;               :message    "Please input Numbers"}]}
;;     {:field [:person :address :city]
;;      :errors [{:error-name "requrired"
;;               :message    "this field is required"}]}]
;;
;;   * if there are no errors, returns empty vector.
;;
(defn- validate-map [key-to-rules key-to-value & {:keys [field-formatter]
                                                  :or {field-formatter nil}}]
  (let [keys-coll (keys-in key-to-rules)]
    (reduce (fn [acc ks]
              (let [rules (-> (get-in key-to-rules ks)
                              (#(if (vector? %) % (vector %))))
                    validations (->> rules
                                     (map #(if (vector? %) % (vector %)))
                                     (map (fn [[rule & args]]
                                            (let [args (->> (vec args)
                                                            (cons (get-in
                                                                   key-to-value
                                                                   ks)))]
                                              (Validation. rule args)))))]
                (if-let [error-map (validate-per-field ks validations
                                                       :field-formatter field-formatter)]
                  (conj acc error-map)
                  acc)))
            []
            keys-coll)))

(defn validate [key-to-rules key-to-values & {:keys [field-formatter]
                                              :or {field-formatter nil}}]
  (cond
    (map? key-to-values) (validate-map key-to-rules key-to-values
                                       :field-formatter field-formatter)
    (vector? key-to-values) (->> key-to-values
                                 (mapv #(validate-map key-to-rules %
                                                      :field-formatter field-formatter)))
    :else (throw #?(:clj  (Exception. "2nd argument should be a map or a vector of maps")
                    :cljs (js/Error.  "2nd argument should be a map or a vector of maps")))))

;;
;;
;; Validation rules 
;;
(def required
  (create-rule {:rulename "required"
                :predicate #(-> % str seq boolean)
                :error-message "This field is required."}))

(def numerical
  (create-rule {:rulename "numerical"
                :predicate #(some? (re-find #"^\d+$" %))
                :error-message "Please input numbers."}))

(def valid-date-string
  (create-rule {:rulename "valid-date-string"
                :predicate #?(:clj #(try
                                      (. LocalDate parse
                                         %
                                         (-> (. DateTimeFormatter ofPattern "uuuu-M-d")
                                             (.withResolverStyle (. ResolverStyle STRICT))))
                                      true
                                      (catch DateTimeParseException _
                                        false))
                              :cljs #(not (-> % js/Date. js/isNaN)))
                :error-message "Invalid date."}))

(def health-insurance-number
  (create-rule {:rulename "health-insurance-number"
                :predicate #(some? (re-find #"^\d{12}$" %))
                :error-message "Incorrect number of digits."}))

(comment
  (keys-in {:name {:value required}
            :person {:address {:city required
                               :zip required}
                     :phone required}
            :a required})
  (validate-per-field [:name] [(Validation. required [""])])
  (validate {:name required} {:name ""})
  ((:predicate numerical) "20200101")
  ((:predicate valid-date-string) "2020-01-01")
  ((:predicate health-insurance-number) "23456789012")
  (prn (.. js/Object -prototype -toString (call (js/Date.))))
  (def key-to-values {:first_name ""
                      :last_name "b"
                      :gender true
                      :birth "2020-01-01"
                      :address "Moscow"
                      :health_insurance_number "0123456789012"})
  (def key-to-rules {:first_name required
                     :last_name required
                     :gender required
                     :birth valid-date-string
                     :address required
                     :health_insurance_number health-insurance-number})
  (validate key-to-rules key-to-values))

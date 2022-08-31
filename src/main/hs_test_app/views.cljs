(ns hs-test-app.views
  (:require  [react :as react]
             [reagent.core :as r]
             [re-frame.core :refer [dispatch subscribe]]
             [stylo.rule :refer [rule defrules]]
             [hs-test-app.validation :as v])
  (:require-macros [stylo.core :refer [c]]))

;;
;;
;; -------------------------------------------------
;; Custom rules for macroCSS
;;
;; -------------------------------------------------
(defmethod rule :w-max-screen-2xl [_]
  [[:& {:max-width "87.5rem"}]])

(defmethod rule :gap-x
  ([_] [[:& {:column-gap "1px"}]])
  ([_ x] [[:& {:column-gap (str (* x 0.25) "rem")}]]))

(defmethod rule :outline-none [_]
  [[:& {:outline "2px" "solid" "transparent"
        :outline-offset "2px"}]])

;;
;;
;; -------------------------------------------------
;; Maps for select-picker options
;;
;; -------------------------------------------------
(def month-options
  [{:label "Jan" :value "01"}
   {:label "Feb" :value "02"}
   {:label "Mar" :value "03"}
   {:label "Apr" :value "04"}
   {:label "May" :value "05"}
   {:label "Jun" :value "06"}
   {:label "Jul" :value "07"}
   {:label "Aug" :value "08"}
   {:label "Sep" :value "09"}
   {:label "Oct" :value "10"}
   {:label "Nov" :value "11"}
   {:label "Dec" :value "12"}])

(def gender-options
  [{:label "Male" :value true}
   {:label "Female" :value false}])

(def filter-options
  [{:label "First Name" :value "first_name"}
   {:label "Last Name" :value "last_name"}
   {:label "Gender" :value "gender"}
   {:label "Birth" :value "birth"}
   {:label "Address" :value "address"}
   {:label "Health Insurance Number" :value "health_insurance_number"}])

;;
;;
;; -------------------------------------------------
;; For validation
;;
;; -------------------------------------------------
(defn valid-filter [m]
  (let [get-error (fn [{:keys [field operator value]}]
                    (cond
                      (empty? field)
                      {:field :field
                       :error-type :required}

                      (empty? operator)
                      {:field :operator
                       :error-type :required}

                      (empty? value)
                      {:field :value
                       :error-type :required}

                      ;; added
                      (and (= field "first_name")
                           (false? (:valid? (v/alphabets nil value))))
                      {:field :value
                       :error-type :alphabets}

                      (and (= field "last_name")
                           (false? (:valid? (v/alphabets nil value))))
                      {:field :value
                       :error-type :alphabets}

                      (and (= field "birth")
                           (false? (:valid? (v/date-string-filled nil value))))
                      {:field :value
                       :error-type :date-unfilled}

                      (and (= field "birth")
                           (false? (:valid? (v/date-string-numerical nil value))))
                      {:field :value
                       :error-type :date-numerical}

                      (and (= field "birth")
                           (false? (:valid? (v/valid-date-string nil value))))
                      {:field :value
                       :error-type :invalid-date}

                      (and (= field "health_insurance_number")
                           (false? (:valid? (v/numerical nil value))))
                      {:field :value
                       :error-type :numerical}

                      (and (= field "health_insurance_number")
                           (false? (:valid? (v/health-insurance-number nil value))))
                      {:field :value
                       :error-type :health_insurance_number}

                      :else nil))

        error-map (get-error m)
        errortype->message {:required "Required."
                            :alphabets "Please input alphabets."
                            :date-unfilled "Please fill all input of date."
                            :date-numerical "Please input numbers."
                            :invalid-date "Invalid date."
                            :numerical "Please input numbers."
                            :health_insurance_number "Incorrect number of digits."}]
    {:valid? (nil? error-map)
     :field (:field error-map)
     :message (get errortype->message (:error-type error-map))}))

(def validator-map
  {:patient-reg-form {:first_name [[v/required "First Name"] [v/alphabets]]
                      :last_name [[v/required "Last Name"] [v/alphabets]]
                      :gender [[v/required "Gender"]]
                      :birth [[v/required "Birth"]
                              [v/date-string-filled]
                              [v/date-string-numerical]
                              [v/valid-date-string]]
                      :address [[v/required "Address"]]
                      :health_insurance_number [[v/required "Health Insurance Number"]
                                                [v/numerical]
                                                [v/health-insurance-number]]}
   :patient-filter-form valid-filter})

;;
;;
;; -------------------------------------------------
;; Form input components
;;
;; -------------------------------------------------
(defonce error-context (react/createContext {:error? false
                                             :error-message nil}))
(def ErrorProvider (.-Provider error-context))

;;
;;
;; :class-atrrs 
;; {:fieldset {:class (c xxx)  :disable-default? boolean}
;;  :label    {:class (c xxx)  :disable-default? boolean}}
(defn input-frame [{:keys [label
                           frame-style
                           ;size
                           class-attrs
                           error?
                           error-message]}
                   & children]
  (let [fieldset-class (get-in class-attrs [:fieldset :class])
        label-class (get-in class-attrs [:label :class])
        disable-default-fieldset-class? (get-in class-attrs [:fieldset :disable-default?])
        disable-default-label-class? (get-in class-attrs [:label :disable-default?])
        default-fieldset-class (cond
                                 disable-default-fieldset-class? (c)
                                 (= frame-style :outline) (c [:bg :white]
                                                             [:rounded :lg]
                                                             :transition [:duration 200])
                                 (= frame-style :fill) (c [:bg :gray-200]
                                                          [:border 2 :gray-200]
                                                          [:rounded :lg]
                                                          :transition [:duration 200])
                                 :else (throw (js/Error. (str "frame-style: " frame-style " doesn't exist.\n"
                                                              "label: " label))))
        default-label-class (cond
                              disable-default-label-class? (c)
                              (= frame-style :outline) (c :block
                                                          [:mx 2]
                                                          [:px 1] [:py 1]
                                                          :transition [:duration 200])
                              (= frame-style :fill) (c :block
                                                       [:mx 2]
                                                       [:px 1] [:py 1]
                                                       :transition [:duration 200])
                              :else (throw (js/Error. (str "frame-style: " frame-style " doesn't exist.\n"
                                                           "label: " label))))
        fieldset-color-class (cond
                               (and error?
                                    (= frame-style :outline))  (c [:border 2 :red-500])
                               (and (not error?)
                                    (= frame-style :outline))  (c [:border 2 :gray-500]
                                                                  [:focus-within [:border 2 :teal-500]])
                               (= frame-style :fill)  (c [:border 2 :gray-200])
                               :else (throw (js/Error. (str "frame-style: " frame-style " doesn't exist.\n"
                                                            "label: " label))))
        label-color-class (cond
                            error?  (c [:text :red-500] :font-semibold)
                            (= frame-style :outline)  (c [:text :gray-600]
                                                         [:focus-within [:text :teal-600] :font-semibold])
                            (= frame-style :fill)  (c [:text :gray-600]
                                                      [:focus-within [:text :teal-600] :font-semibold])
                            :else (throw (js/Error. (str "frame-style: " frame-style " doesn't exist.\n"
                                                         "label: " label))))]
    [:field-set {:class [fieldset-class
                         fieldset-color-class
                         default-fieldset-class]}
     [:label {:class [label-class
                      label-color-class
                      default-label-class]}
      [:span {:class (c :block)}
       label]
      (doall
       (map-indexed (fn [i v]
                      [:<> {:key i}
                       (r/create-element ErrorProvider
                                         #js {:value {:error? error?
                                                      :error-message error-message}}
                                         (r/as-element v))])
                    children))]]))

(defn valid-feedback [error?
                      error-message]
  (let [error-context (react/useContext error-context)
        error? (if error-context (:error? error-context) error?)
        error-message (if error-context (:error-message error-context) error-message)
        visibility (if error? (c) (c :invisible))]
    [:p {:class [(c [:h 5]
                    [:text :red-500] :text-xs :font-semibold
                    [:ml 3])
                 visibility]}
     (or error-message "")]))

(defn embedded-input
  [{:keys [field-name
           input-type
           class-attrs
           default-value
           placeholder
           on-change
           on-blur]}]
  (let [additional-class (get-in class-attrs [:input :class])
        disable-default? (get-in class-attrs [:input :disable-default?])
        default-class (if disable-default?
                        (c)
                        (c :w-full [:h 8]
                           [:bg :white] [:bg-opacity 0]
                           [:text :black] :text-base
                           [:focus :outline-none]
                           [:px 5]))]
    [:input {:class [default-class
                     additional-class]
             :name field-name
             :type input-type
             :default-value default-value
             :placeholder placeholder
             :on-change on-change
             :on-blur on-blur}]))

(defn embedded-select [{:keys [field-name
                               class-attrs
                               default-value
                               options
                               placeholder
                               omit-empty-option?
                               on-change
                               on-blur]}]
  (let [additional-class (get-in class-attrs [:select :class])
        disable-default? (get-in class-attrs [:select :disable-default?])
        default-class (if disable-default?
                        (c)
                        (c :w-full [:h 8]
                           [:bg :white] [:bg-opacity 0]
                           [:text :black] :text-base
                           [:focus :outline-none]
                           [:px 5]
                           :cursor-pointer))
        placeholder (or placeholder "Please select")]
    [:select {:key default-value    ; A key is set to force rerender component 
                                    ; in React's world on changing default-value. 
                                    ; This happens right after components mounted, 
                                    ; because the subscribed values are added to 
                                    ; re-frame signal graph right after the first DOM 
                                    ; is rendered (first rendering occurs with empty values). 
              :class [default-class
                      additional-class]
              :name field-name
              :default-value default-value
              :on-change on-change
              :on-blur on-blur}
     (when-not omit-empty-option?
       [:option {:value ""} placeholder])
     (map (fn [o]
            [:option {:key (:label o) :value (:value o)} (:label o)])
          options)]))

;;
;;
;; :class-atrrs 
;; {:input-frame   {:fieldset {:class (c xxx)  :disable-default? boolean}
;;                  :label    {:class (c xxx)  :disable-default? boolean}}
;;  :embedded-input {:input    {:class (c xxx)  :disable-default? boolean}}}
(defn text-field [{:keys [label
                          frame-style
                          class-attrs
                          field-name
                          input-type
                          default-value
                          placeholder
                          on-change
                          on-blur
                          omit-valid-feedback?
                          error?
                          error-message]}]
  [input-frame {:label label
                :frame-style frame-style
                :class-attrs (get class-attrs :input-frame)
                :error? error?
                :error-message error-message}
   [embedded-input {:field-name field-name
                    :input-type input-type
                    :class-attrs (get class-attrs :input)
                    :default-value default-value
                    :placeholder placeholder
                    :on-change on-change
                    :on-blur on-blur}]
   (when-not omit-valid-feedback?
     [:f> valid-feedback {:error? error?
                          :error-message error-message}])])

;;
;;
;; :class-atrrs 
;; {:input-frame    {:fieldset {:class (c xxx)  :disable-default? boolean}
;;                   :label    {:class (c xxx)  :disable-default? boolean}}
;;  :embedded-select {:select   {:class (c xxx)  :disable-default? boolean}}}
(defn select-picker [{:keys [label
                             frame-style
                             class-attrs
                             field-name
                             default-value
                             options
                             placeholder
                             omit-empty-option?
                             on-change
                             on-blur
                             omit-valid-feedback?
                             error?
                             error-message]}]
  [input-frame {:label label
                :frame-style frame-style
                :fieldset-class (get-in class-attrs [:input-frame :fieldset :class])
                :legend-class (get-in class-attrs [:input-frame :label :class])
                :disable-default-fieldset-class? (get-in class-attrs
                                                         [:fieldset :disable-default?]
                                                         false)
                :disable-default-legend-class? (get-in class-attrs
                                                       [:fieldset :disable-default?]
                                                       false)
                :error? error?
                :error-message error-message}
   [embedded-select {:field-name field-name
                     :class-attr (get-in class-attrs [:select :class])
                     :default-value default-value
                     :options options
                     :placeholder placeholder
                     :omit-empty-option? omit-empty-option?
                     :on-change on-change
                     :on-blur on-blur}]
   (when-not omit-valid-feedback?
     [:f> valid-feedback {:error? error?
                          :error-message error-message}])])

;;
;;
;; :class-atrrs 
;; {:date  {:input-frame    {:fieldset {:class (c xxx)  :disable-default? boolean}
;;                           :label    {:class (c xxx)  :disable-default? boolean}}
;;          :embedded-input {:input    {:class (c xxx)  :disable-default? boolean}}}
;;  :year  {:input-frame    {:fieldset {:class (c xxx)  :disable-default? boolean}
;;                           :label    {:class (c xxx)  :disable-default? boolean}}
;;          :embedded-input {:input    {:class (c xxx)  :disable-default? boolean}}}
;;  :month {:input-frame    {:fieldset {:class (c xxx)  :disable-default? boolean}
;;                           :label    {:class (c xxx)  :disable-default? boolean}}
;;          :embedded-select {:select  {:class (c xxx)  :disable-default? boolean}}}
;;  :day   {:input-frame    {:fieldset {:class (c xxx)  :disable-default? boolean}
;;                           :label    {:class (c xxx)  :disable-default? boolean}}
;;          :embedded-input {:input    {:class (c xxx)  :disable-default? boolean}}}}
(defn date-input [{:keys [label
                          size
                          ;class-attrs
                          year-field-name
                          month-field-name
                          day-field-name
                          year
                          month
                          day
                          on-change
                          error?
                          error-message]
                   :or {size :md}}]
  (let [{:keys [year-label
                month-label
                day-label
                month-placeholder]} (case size
                                      :sm {:year-label ""
                                           :month-label ""
                                           :day-label ""
                                           :month-placeholder "MM"}
                                      :md {:year-label "Year"
                                           :month-label "Month"
                                           :day-label "Day"
                                           :month-placeholder nil}
                                      (throw (js/Error. (str "size: " size "doesn't exit.\n"
                                                             "label: " label))))]
    [input-frame {:label label
                  :frame-style :outline
                  :class-attrs {:input-frame {:fieldset {:class (c [:w 100])
                                                         :disable-default? false}}}
                  :error? error?
                  :error-message error-message}
     [:div {:class (c :flex :flex-col :justify-center :items-center)}
      [:div {:class (c :flex :flex-row :justify-center :items-center)
             :style {:gap "0.5rem"}}
       [text-field {:label day-label
                    :frame-style :fill
                    :class-attrs {:input-frame {:fieldset {:class (c [:w 30])
                                                           :disable-default? false}}}
                    :field-name day-field-name
                    :input-type "tel"
                    :default-value day
                    :placeholder "DD"
                    :on-change (partial on-change :day)
                    :omit-valid-feedback? true
                    :error? error?}]
       [select-picker {:label month-label
                       :frame-style :fill
                       :class-attrs {:fieldset {:class (c [:w 40])
                                                :disable-default? false}}
                       :field-name month-field-name
                       :default-value month
                       :placeholder month-placeholder
                       :options month-options
                       :on-change (partial on-change :month)
                       :omit-valid-feedback? true
                       :error? error?}]
       [text-field {:label year-label
                    :frame-style :fill
                    :class-attrs {:input-frame {:fieldset {:class (c [:w 50])
                                                           :disable-default? false}}}
                    :field-name year-field-name
                    :input-type "tel"
                    :default-value year
                    :placeholder "YYYY"
                    :on-change (partial on-change :year)
                    :omit-valid-feedback? true
                    :error? error?}]]
      [:f> valid-feedback {:error? error?
                           :error-message error-message}]]]))

(defn primary-button [{:keys [class-attr
                              button-type
                              disabled?
                              on-click]}
                      & children]
  (let [default-class (c [:h 15]
                         [:text :white] :text-base :font-semibold
                         [:rounded :lg]
                         :focus-outline-none
                         :transition [:duration 200])
        color-class (if disabled?
                      (c [:bg :green-300])
                      (c [:bg :green-600]
                         [:hover [:bg :green-700]]
                         [:active [:bg :green-800]]))]
    [:button {:class [class-attr
                      color-class
                      default-class]
              :type button-type
              :disabled disabled?
              :on-click on-click
              :style {:outline "2px solid transparent"
                      :outline-offset "2px"}}
     children]))

(defn secondary-button [{:keys [class-attr
                                button-type
                                disabled?
                                on-click]}
                        & children]
  (let [default-class (c [:h 15]
                         [:text :indigo-700] :text-base :font-semibold
                         [:hover [:text :indigo-500]]
                         :focus-outline-none
                         :transition [:duration 200])]
    [:button {:class [class-attr
                      default-class]
              :type button-type
              :disabled disabled?
              :on-click on-click
              :style {:outline "2px solid transparent"
                      :outline-offset "2px"}}
     children]))

(defn outline-button [{:keys [class-attr
                              button-type
                              disabled?
                              on-click]}
                      & children]
  (let [default-class (c [:h 15]
                         [:bg :white]
                         :text-base :font-semibold
                         [:border 2 :indigo-600] [:rounded :lg]
                         :focus-outline-none
                         :transition [:duration 200])
        color-class (if disabled?
                      (c [:text :indigo-300]
                         [:border :indigo-300])
                      (c [:text :indigo-600]
                         [:hover [:bg :indigo-600]
                          [:text :white]]
                         [:active [:bg :indigo-700]
                          [:border :indigo-700]
                          [:text :white]]))]
    [:button {:class [class-attr
                      color-class
                      default-class]
              :type button-type
              :disabled disabled?
              :on-click on-click
              :style {:outline "2px solid transparent"
                      :outline-offset "2px"}}
     children]))

;;
;;
;; -------------------------------------------------
;; Functions for form input components
;;
;; -------------------------------------------------
(defn change-field
  ;; ality - 4
  ([form-id
    validation-rules
    {:keys [field-type field-id]}
    event]                        ;; without date-unit
   (dispatch
    [:on-change-field {:form-id form-id
                       :field-type field-type
                       :field-id field-id
                       :value (-> event
                                  .-target
                                  .-value)
                       :validation-rules validation-rules}]))
  ;; ality - 5
  ([form-id
    validation-rules
    {:keys [field-type field-id]}
    date-unit                     ;; with date-unit
    event]
   (dispatch
    [:on-change-field {:form-id form-id
                       :field-type field-type
                       :field-id field-id
                       :date-unit date-unit
                       :value (-> event
                                  .-target
                                  .-value)
                       :validation-rules validation-rules}])))

(defn change-dynamic-field
  ;; ality - 4
  ([form-id
    validation-rules
    {:keys [field-type index field-id]}
    event]                        ;; without date-unit
   (dispatch
    [:on-change-dynamic-field {:form-id form-id
                               :field-type field-type
                               :index index
                               :field-id field-id
                               :value (-> event
                                          .-target
                                          .-value)
                               :validation-rules validation-rules}]))
  ;; ality - 5
  ([form-id
    validation-rules
    {:keys [field-type index field-id]}
    date-unit                     ;; with date-unit
    event]
   (dispatch
    [:on-change-dynamic-field {:form-id form-id
                               :field-type field-type
                               :index index
                               :field-id field-id
                               :date-unit date-unit
                               :value (-> event
                                          .-target
                                          .-value)
                               :validation-rules validation-rules}])))

(defn blur-field [form-id validation-rules]
  (dispatch
   [:update-errors {:form-id form-id
                    :validation-rules validation-rules}]))

;;
;;
;; -------------------------------------------------
;; Form components
;;
;; -------------------------------------------------
;;
;; Register Form
;;
(defn patient-reg-form [handle-submit]
  (let [validation-rules (:patient-reg-form validator-map)
        handle-change (partial change-field :patient-reg-form validation-rules)
        handle-blur (partial blur-field :patient-reg-form validation-rules)
        has-errors? @(subscribe [:has-form-errors? :patient-reg-form])
        submitting? @(subscribe [:form-submitting? :patient-reg-form])]
    [:form {:id :patient-reg-form
            :on-submit handle-submit}
     [:div {:class (c :flex :flex-row [:gap-x 20]
                      [:mb 5])
            :style {:gap "0.5rem"}}
      [text-field {:label "First Name"
                   :frame-style :outline
                   :class-attrs {:input-frame {:fieldset {:class (c [:w 60])
                                                          :disable-default? false}}}
                   :field-name :first_name
                   :input-type "text"
                   :default-value @(subscribe
                                    [:form-field-value :patient-reg-form :first_name])
                   :on-change (partial handle-change {:field-type "text"
                                                      :field-id :first_name})
                   :on-blur handle-blur
                   :error? @(subscribe
                             [:form-field-error? :patient-reg-form :first_name])
                   :error-message @(subscribe
                                    [:form-field-error-message :patient-reg-form :first_name])}]
      [text-field {:label "Last Name"
                   :frame-style :outline
                   :field-name :last_name
                   :class-attrs {:input-frame {:fieldset {:class (c [:w 60])
                                                          :disable-default? false}}}
                   :input-type "text"
                   :default-value @(subscribe
                                    [:form-field-value :patient-reg-form :last_name])
                   :on-change (partial handle-change {:field-type "text"
                                                      :field-id :last_name})
                   :on-blur handle-blur
                   :error? @(subscribe
                             [:form-field-error? :patient-reg-form :last_name])
                   :error-message @(subscribe
                                    [:form-field-error-message :patient-reg-form :last_name])}]
      [select-picker {:label "Gender"
                      :frame-style :outline
                      :class-attrs {:input-frame {:fieldset {:class (c [:w 60])
                                                             :disable-default? false}}}
                      :field-name :gender
                      :default-value @(subscribe
                                       [:form-field-value :patient-reg-form :gender])
                      :options gender-options
                      :on-change (partial handle-change {:field-type "text"
                                                         :field-id :gender})
                      :on-blur handle-blur
                      :error? @(subscribe
                                [:form-field-error? :patient-reg-form :gender])
                      :error-message @(subscribe
                                       [:form-field-error-message :patient-reg-form :gender])}]]
     [:div {:class (c :flex :flex-row [:gap-x 20]
                      [:mb 5])}
      (let [{:keys [year
                    month
                    day
                    year-field-name
                    month-field-name
                    day-field-name]} @(subscribe
                                       [:form-date-value :patient-reg-form :birth])]
        [date-input {:label "Birth"
                     :class-attrs {:date {:input-frame {:fieldset {:class (c [:w 140])
                                                                   :disable-default? false}}}}
                     :year-field-name year-field-name
                     :month-field-name month-field-name
                     :day-field-name day-field-name
                     :year year
                     :month month
                     :day day
                     :on-change (partial handle-change {:field-type "date"
                                                        :field-id :birth})
                     :error? @(subscribe
                               [:form-field-error? :patient-reg-form :birth])
                     :error-message @(subscribe
                                      [:form-field-error-message :patient-reg-form :birth])}])]
     [:div {:class (c :flex :flex-row
                      [:mb 5])}
      [text-field {:label "Address"
                   :frame-style :outline
                   :field-name :address
                   :class-attrs {:input-frame {:fieldset {:class (c [:w 150])
                                                          :disable-default? false}}}
                   :input-type "text"
                   :default-value @(subscribe
                                    [:form-field-value :patient-reg-form :address])
                   :on-change (partial handle-change {:field-type "text"
                                                      :field-id :address})
                   :on-blur handle-blur
                   :error? @(subscribe
                             [:form-field-error? :patient-reg-form :address])
                   :error-message @(subscribe
                                    [:form-field-error-message :patient-reg-form :address])}]]
     [:div {:class (c :flex :flex-row
                      [:mb 20])}
      [text-field {:label "Health Insurance Number"
                   :frame-style :outline
                   :field-name :health_insurance_number
                   :class-attrs {:input-frame {:fieldset {:class (c [:w 80])
                                                          :disable-default? false}}}
                   :input-type "tel"
                   :default-value @(subscribe
                                    [:form-field-value :patient-reg-form :health_insurance_number])
                   :on-change (partial handle-change {:field-type "text"
                                                      :field-id :health_insurance_number})
                   :on-blur handle-blur
                   :error? @(subscribe
                             [:form-field-error? :patient-reg-form :health_insurance_number])
                   :error-message @(subscribe
                                    [:form-field-error-message :patient-reg-form :health_insurance_number])}]]
     [primary-button {:class-attr (c [:w 50])
                      :button-type "submit"
                      :disabled? (or has-errors? submitting?)
                      :on-click #(dispatch [:toggle-form-submitting :patient-reg-form true])}
      "Save"]]))

;;
;;
;; Common template for Create / Edit Page
;;
(defn patient-reg-page [{:keys [title handle-submit]}]
  [:article
   [:h2 {:class (c [:text :gray-600] :font-extrabold :text-4xl)}
    title]
   [:div {:class (c [:mt 4] [:mb 8])}
    [secondary-button {:class-attr (c [:w 70]
                                      :text-left)
                       :button-type "button"
                       :on-click #(dispatch [:trigger-navigation
                                             "/patients"])}
     "< Back to Patient List"]]
   [patient-reg-form  handle-submit]])

;;
;;
;; For patient-filter-form
;;
(defn operator-input [{:keys [field
                              index
                              operator
                              operator-id
                              on-change
                              on-blur]}]
  (when (some? field)
    (let [field-type (if (= field "birth") "date" "text")
          operator-options (cond
                             (= field "gender") [{:label "Is" :value "eq"}]
                             (= field "birth") [{:label "After" :value "gt"}
                                                {:label "Before" :value "lt"}
                                                {:label "Is" :value "eq"}]
                             :else [{:label "Is" :value "eq"}
                                    {:label "Contains" :value "gt"}])]
      [select-picker {:label ""
                      :frame-style :outline
                      :class-attrs {:input-frame {:fieldset {:class (c [:w 40])
                                                             :disable-default? false}
                                                  :label {:class (c :invisible)
                                                          :disable-default? false}}}
                      :field-name operator-id
                      :default-value operator
                      :options operator-options
                      :omit-empty-option? true
                      :on-change (partial on-change {:field-type field-type
                                                     :index index
                                                     :field-id :operator})
                      :on-blur on-blur
                      :error? @(subscribe
                                [:array-form-field-error? :patient-filter-form index :operator])
                      :error-message @(subscribe
                                       [:array-form-field-error-message :patient-filter-form index :operator])}])))

;;
;;
;; For patient-filter-form
;;
(defn value-input [{:keys [field
                           index
                           value
                           value-id
                           on-change
                           on-blur]}]
  (when (some? field)
    (cond
      (= field "gender") [select-picker {:label ""
                                         :frame-style :outline
                                         :class-attrs {:input-frame {:fieldset {:class (c [:w 30])
                                                                                :disable-default? false}
                                                                     :label {:class (c :invisible)
                                                                             :disable-default? false}}}
                                         :field-name value-id
                                         :default-value value
                                         :options gender-options
                                         :on-change (partial on-change {:field-type "text"
                                                                        :index index
                                                                        :field-id :value})
                                         :on-blur on-blur
                                         :error? @(subscribe
                                                   [:array-form-field-error? :patient-filter-form index :value])
                                         :error-message @(subscribe
                                                          [:array-form-field-error-message
                                                           :patient-filter-form
                                                           index
                                                           :value])}]
      (= field "birth") (let [{:keys [year
                                      month
                                      day
                                      year-field-name
                                      month-field-name
                                      day-field-name]} @(subscribe
                                                         [:array-form-date-value
                                                          :patient-filter-form
                                                          index
                                                          :value])]
                          [date-input {:label ""
                                       :size :sm
                                       :class-attrs {:date {:input-frame {:fieldset {:class (c [:w 80])
                                                                                     :disable-default? false}}}}
                                       :year-field-name year-field-name
                                       :month-field-name month-field-name
                                       :day-field-name day-field-name
                                       :year year
                                       :month month
                                       :day day
                                       :on-change (partial on-change {:field-type "date"
                                                                      :index index
                                                                      :field-id :value})
                                       :error? @(subscribe
                                                 [:array-form-field-error? :patient-filter-form index :value])
                                       :error-message @(subscribe
                                                        [:array-form-field-error-message
                                                         :patient-filter-form
                                                         index
                                                         :value])}])
      :else [text-field {:label ""
                         :frame-style :outline
                         :field-name :value
                         :class-attrs {:input-frame {:fieldset {:class (c [:w 80])
                                                                :disable-default? false}}}
                         :input-type "text"
                         :default-value value
                         :on-change (partial on-change {:field-type "text"
                                                        :index index
                                                        :field-id :value})
                         :on-blur on-blur
                         :error? @(subscribe
                                   [:array-form-field-error? :patient-filter-form index :value])
                         :error-message @(subscribe
                                          [:array-form-field-error-message
                                           :patient-filter-form
                                           index
                                           :value])}])))

;;
;;
;; Filter Array Form
;;
(defn patient-filter-form []
  (let [filters @(subscribe [:array-form-indexed :patient-filter-form])
        keywords @(subscribe [:form :patient-search])
        validation-rules (:patient-filter-form validator-map)
        handle-change (partial change-dynamic-field
                               :patient-filter-form
                               validation-rules)
        handle-blur (partial blur-field :patient-filter-form validation-rules)
        has-errors? @(subscribe [:has-form-errors? :patient-filter-form])]
    [:form {:id :patient-filter-form}
     (doall
      (map (fn [{:keys [index
                        uuid
                        field
                        operator
                        value
                        field-id
                        operator-id
                        value-id]}]
             [:fieldset {:key uuid
                         :class (c [:w 260] [:h 24]
                                   :flex :flex-row :justify-between
                                   [:bg :gray-200]
                                   [:border 2 :gray-300] [:rounded :xl]
                                   [:px 5] [:py 2] [:mb 2])}
              [:div {:class (c :flex :flex-row :justify-start :items-center [:gap-x 2])
                     :style {:gap "1rem"}}
               [select-picker {:label ""
                               :frame-style :outline
                               :class-attrs {:input-frame {:fieldset {:class (c [:w 30])
                                                                      :disable-default? false}}}
                               :field-name field-id
                               :default-value field
                               :options filter-options
                               :placeholder "Select Target Field"
                               :on-change (partial handle-change {:field-type "text"
                                                                  :index index
                                                                  :field-id :field})
                               :on-blur handle-blur
                               :error? @(subscribe
                                         [:array-form-field-error? :patient-filter-form index :field])
                               :error-message @(subscribe
                                                [:array-form-field-error-message :patient-filter-form index :field])}]
               [operator-input {:index index
                                :field field
                                :operator operator
                                :operator-id operator-id
                                :on-change handle-change
                                :on-blur handle-blur}]
               [value-input {:index index
                             :field field
                             :value value
                             :value-id value-id
                             :on-change handle-change
                             :on-blur handle-blur}]]
              [:div {:class (c :flex :flex-row :justify-end :items-center)}
               [:button {:class (c [:bg-opacity 0]
                                   [:text :gray-600] :text-3xl
                                   [:hover [:text :red-600]]
                                   :transition [:duration 200])
                         :type "button"
                         :style {:outline "2px solid transparent"
                                 :outline-offset "2px"}
                         :on-click  #(dispatch [:delete-dynamic-fieldset
                                                {:form-id :patient-filter-form
                                                 :index index}])}
                "x"]]])
           filters))
     [secondary-button {:class-attr (c [:w 25])
                        :button-type "button"
                        :on-click #(dispatch [:add-filter
                                              {:form-id :patient-filter-form
                                               :validation-rules validation-rules}])}

      "+ Add Filter"]
     (when (seq filters)
       [primary-button {:class-attr (c [:w 25]
                                       [:ml 6])
                        :button-type "button"
                        :disabled? has-errors?
                        :on-click  #(dispatch [:search-patients
                                               {:keywords keywords
                                                :filters filters}])}
        "Apply"])]))

;;
;;
;; -------------------------------------------------
;;
;; View rendering functions for the entire pages
;;
;; -------------------------------------------------
(defmulti view identity)

;(defmethod view ::list [_]
(defmethod view :default [_]
  [:article
   [:div {:class (c :flex :flex-row :justify-between)}
    [:h2 {:class (c [:text :gray-600] :font-extrabold :text-4xl)}
     "Patient List"]

    [outline-button {:class-attr (c [:w 50])
                     :button-type "button"
                     :on-click #(dispatch [:trigger-navigation
                                           "/patients/create"])}
     "Create New Patient"]]
   [:div {:class (c [:w 180] [:h 24]
                    :flex :flex-row :justify-between
                    [:bg :indigo-100]
                    [:border 2 :gray-300] [:rounded :xl]
                    [:mt 5] [:mb 3] [:px 5] [:py 4])}
    [:form {:id :patient-search
            :class (c :w-full)}
     [:input {:class (c :w-full :w-max-2xl :w-min-full
                        [:h 15]
                        :text-base
                        [:border 2 :gray-500]
                        [:focus-within [:border 2 :teal-500]]
                        [:rounded :lg]
                        [:pl 10])
              :type "text"
              :name "keywords"
              :placeholder "Name or Health Insurance Number"
              :value @(subscribe [:form :patient-search])
              :on-change #(dispatch [:on-change-search-keywords
                                     {:keywords (-> %
                                                    .-target
                                                    .-value)
                                      :filter @(subscribe
                                                [:array-form-indexed
                                                 :patient-filter-form])}])
              :style {:outline "2px solid transparent"
                      :outline-offset "2px"}}]]]
   [patient-filter-form]
   [:ul {:class (c [:mt 7])}
    [:li {:class (c [:h 18]
                    :flex :flex-row :justify-around :items-center
                    [:bg :indigo-100]
                    [:border 2 :gray-300] [:rounded :xl]
                    :shadow
                    [:text :gray-700] :text-lg :text-center :font-semibold
                    [:my 2] [:px 5] [:py 2])}
     [:span {:class (c [:w 50])} "NAME"]
     [:span {:class (c [:w 10])} "GENDER"]
     [:span {:class (c [:w 50])} "BIRTH"]
     [:span {:class (c [:w 70])} "ADDRESS"]
     [:span {:class (c [:w 60]
                       :text-base)}
      "HEALTH INSURANCE NUMBER"]
     [:span {:class (c [:w 10])} ""]]
    (map (fn [{:keys [id
                      name
                      gender
                      birth
                      address
                      health_insurance_number]}]
           [:li {:key id
                 :class (c [:h 24]
                           :flex :flex-row :justify-around :items-center
                           [:bg :white]
                           [:border 2 :gray-300] [:rounded :xl]
                           :shadow
                           [:hover [:border 3 :green-500]
                            :shadow-lg]
                           [:text :gray-800] :text-center :text-base :font-sans
                           [:my 2] [:px 5] [:py 2]
                           :cursor-pointer
                           :transition [:duration 200])
                 :on-click #(dispatch [:trigger-navigation
                                       (str "/patients/" id "/edit")])}
            [:span {:class (c [:w 50]
                              :text-lg)}
             name]
            [:span {:class (c [:w 10])} gender]
            [:span {:class (c [:w 50])} birth]
            [:span {:class (c [:w 70])} address]
            [:span {:class (c [:w 60])} health_insurance_number]
            [:button {:class (c [:bg-opacity 0]
                                [:text :gray-600] :text-3xl
                                [:hover [:text :red-600]]
                                :transition [:duration 200])
                      :type "button"
                      :style {:outline "2px solid transparent"
                              :outline-offset "2px"}
                      :on-click #((.stopPropagation %)
                                  (dispatch [:delete-patient id]))}
             "x"]])
         @(subscribe [:patients-formatted]))]])
;)

(defmethod view ::edit []
  [patient-reg-page {:title "Edit Patient Data"
                     :handle-submit (fn [event]
                                      (let [id (:id @(subscribe [:patient-in-edit]))
                                            data @(subscribe [:form
                                                              :patient-reg-form])]
                                        (.preventDefault event)
                                        (dispatch [:update-patient
                                                   id
                                                   {:values data}])))}])

(defmethod view ::create []
  [patient-reg-page {:title "Create Patient Data"
                     :handle-submit (fn [event]
                                      (let [data @(subscribe [:form
                                                              :patient-reg-form])]
                                        (.preventDefault event)
                                        (dispatch [:create-patient
                                                   {:values data}])))}])

(defn main-panel []
  [:main {:class (c :h-min-screen
                    [:bg :gray-200]
                    :font-mono
                    [:px 15] [:pt 8] [:pb 10])}
   [:div {:class (c :flex :flex-row :justify-center)}
    [:h1 {:class (c [:w 400]
                    :flex :flex-row :justify-end
                    [:text :gray-700] :font-bold
                    [:mb 7])} "Health Samurai Test App"]]
   [:div {:class (c :flex :flex-row :justify-center)}
    [:div {:class
           (c [:w 400]
              [:bg :white]
              [:rounded 24]
              [:px 10] [:pt 8] [:pb 10])}
     [view @(subscribe [:current-route])]]]])

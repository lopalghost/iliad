(ns iliad.html
  (:require [clojure.set :as set]
            [iliad.core :as iliad :refer [defelement defcontext validator re-validator]]
            [hiccup.core :as hic]
            [clojure.string :as str]
            [clojure.spec.alpha :as spec]))



(def zip-regex #"^\d{5}(-\d{4})?$")

(defelement ::zip
  "An input expected to be a zipcode."
  :extends ::iliad/text
  ;; Hope this is not too strict
  :validate [(re-validator zip-regex :invalid/invalid-zipcode)])

(defelement ::phone
  "An input expected to be a phone number."
  :extends ::iliad/text
  :validate [(re-validator #"^[\d\(\)-\. ]+$" :invalid/invalid-phone)])

(defelement ::email
  "An input expected to be an email."
  :extends ::iliad/text
  :validate [(re-validator #"^.+@.+\..+" :invalid/invalid-email)])

(defelement ::textarea
  "A textbox, same as ::iliad/text for validation purposes."
  :extends ::iliad/text
  :el-spec (spec/merge ::iliad/text (spec/keys :opt-un [::rows])))

(defelement ::radio-group :extends ::iliad/choose-one)

(defelement ::checkbox-group :extends ::iliad/choose-many)


(defn id->text
  [id]
  (if (keyword? id)
    (-> id
        str
        (subs 1))
    (-> (cast String id)
        (or "")
        (str/trim)
        (str/replace #"\W+" "-"))))


(defn attr-map
  "Creates an attribute map from all keys in the :html/ namespace"
  [opts]
  (reduce (fn [m [k v]]
            (if (= "html" (namespace k))
              (assoc m k v)
              m))
          {} opts))


(defn ^:private bs4-input
  [id label attrs]
  (let [id (id->text id)
        attrs (assoc attrs :id id :name id)]
    [:div.form-group
     [:label {:for id} label]
     [:input (merge {:type "text"} attrs)]]))


(defn ^:private bs4-textarea
  [id label attrs]
  (let [id (id->text id)
        attrs (assoc attrs :id id :name id)]
    [:div.form-group
     [:label {:for id} label]
     [:textarea (merge {:rows "3"} attrs)]]))


(defn ^:private bs4-checkbox
  [type id name label attrs]
  (assert #{:radio :checkbox} type)
  (let [id (id->text id)
        name (id->text name)
        attrs (assoc attrs :id id :name name)]
    [:div.form-check
     [:input.form-check-input (merge {:type (clojure.core/name type)
                                      :value label}
                                     attrs)]
     [:label.form-check-label {:for id} label]]))


(defn ^:private bs4-checkboxes
  "Renders a group of checkboxes/radios. Takes as an optional fourth argument a
function of [name label index] that determines the id of each checkbox. Defaults
to appending the name with the index."
  ([type name attrs labels+values]
   (bs4-checkboxes type name attrs (fn [n _ i] (str (id->text n) i)) labels+values))
  ([type name attrs id-fn labels+values]
   [:div
    (for [[l+v i] (map vector labels+values (range))]
      (let [vec? (vector? l+v)
            label (if vec? (first l+v) l+v)
            val (if vec? (second l+v) l+v)]
        (bs4-checkbox type (id-fn name label i) name label (assoc attrs :value val))))]))


(defn ^:private bs4-select
  [id label attrs opts]
  (let [id (id->text id)
        attrs (assoc attrs :id id :name id)]
    [:div.form-group
     [:label {:for id} label]
     [:select attrs (for [opt opts]
                      [:option opt])]]))


(defmulti bs4-renderer (fn
                         ([type opts] type)
                         ([type] ::no-opts)))


(defmethod bs4-renderer ::no-opts
  [type]
  (bs4-renderer type {}))


(defmethod bs4-renderer :input
  [_ attrs]
  (fn [el]
    (let [{:keys [::iliad/id ::iliad/prompt]} el
          attrs (merge attrs (attr-map el))]
      (bs4-input id prompt attrs))))


(defmethod bs4-renderer :checkbox
  [type attrs]
  (fn [el]
    (let [{:keys [::iliad/id ::iliad/prompt name]} el
          attrs (merge attrs (attr-map el))]
      (bs4-checkbox type id (or name id) prompt attrs))))


(defmethod bs4-renderer :radio
  [type attrs]
  (fn [el]
    (let [{:keys [::iliad/id ::iliad/prompt name]} el
          attrs (merge attrs (attr-map el))]
      (bs4-checkbox type id (or name id) prompt attrs))))


(defmethod bs4-renderer :select
  [type attrs]
  (fn [el]
    (let [{:keys [::iliad/id ::iliad/prompt options]} el
          attrs (merge attrs (attr-map el))]
      (bs4-select id prompt attrs options))))


(defmethod bs4-renderer :textarea
  [_ attrs]
  (fn [el]
    (let [{:keys [::iliad/id ::iliad/prompt]} el
          attrs (merge attrs (attr-map el))]
      (bs4-textarea id prompt attrs))))


(defmethod bs4-renderer :checkbox-group
  [_ attrs]
  (fn [el]
    (let [name (::iliad/id el)
          opts (::iliad/children el)]
      (bs4-checkboxes :checkbox name {} opts))))


(defmethod bs4-renderer :radio-group
  [_ attrs]
  (fn [el]
    (let [name (::iliad/id el)
          opts (::iliad/children el)]
      (bs4-checkboxes :radio name {} opts))))


(defcontext ::hiccup-bs4
  "A context for rendering to Bootstrap 4 using hiccup."
  :extends ::iliad/default-context
  ;; TODO: Maybe a multimethod for input type after all?
  :render [::iliad/text (bs4-renderer :input)
           ::iliad/num (bs4-renderer :input {:type "number"})
           ::phone (bs4-renderer :input {:type "tel"})
           ::zip (bs4-renderer :input {:type "number" :html/pattern (str zip-regex)})
           ::email (bs4-renderer :input {:type "email"})
           ::textarea (bs4-renderer :textarea)
           ::radio-group (bs4-renderer :radio-group)
           ::checkbox-group (bs4-renderer :checkbox-group)]
  )

(comment
  ;; Sample form def
  (def sample-form-def
    [[::name ::iliad/text "Name"]
     [::age ::iliad/num "Age"]
     [::email ::email "Email"]
     [::phone ::phone "Phone Number"]
     [::zip ::zip "Zip Code"]
     [::description ::textarea "Describe Yourself"]
     [::yes-no ::radio-group "Yes or no?"
      ["Yes" "No"]]
     [::adjectives ::checkbox-group "Choose some adjectives"
      ["Forlorn" "Merciless" "Insightful" "Indefatigable"]]])

  (def sample-form-model
    (mapv iliad/parse-element-data sample-form-def))

  (iliad/defform sample-form
    {:form-inputs sample-form-def})

  (iliad/defform sample-form
    {:form-inputs [[::name ::iliad/text "Name"]
                   [::age ::iliad/num "Age"]
                   [::email ::email "Email"]
                   [::phone ::phone "Phone Number"]
                   [::zip ::zip "Zip Code"]
                   [::description ::textarea "Describe Yourself"]
                   [::yes-no ::radio-group "Yes or no?"
                    ["Yes" "No"]]
                   [::adjectives ::checkbox-group "Choose some adjectives"
                    ["Forlorn" "Merciless" "Insightful" "Indefatigable"]]]
     :required-inputs [::name ::age ::email ::phone ::zip ::description ::yes-no ::adjectives]})

  ;; End of comment
  nil)


(defn render-form-hiccup
  [{:keys [form-inputs required-inputs]} {:keys [context action id] :as opts}]
  (let [required-inputs (set required-inputs)
        working-input (mapv #(if (required-inputs (::iliad/id %))
                               (assoc % :html/required true
                                      ::iliad/context context)
                               (assoc % ::iliad/context context))
                            form-inputs)
        form-opts (-> {:action action
                       :id id
                       :method "POST"}
                      (merge (attr-map opts)))]
    [:form form-opts
     (map iliad/render working-input)]))



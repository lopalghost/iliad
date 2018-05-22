(ns iliad.html
  (:require [clojure.set :as set]
            [iliad.core2 :as iliad :refer [defelement defcontext]]
            [hiccup.core :as hic]
            [clojure.string :as str]
            [clojure.spec.alpha :as spec]))


(defn validator
  "Helper function for making a compliant validator. Passes thru nil values.
  Returns :invalid/invalid by default."
  ([f] (validator f :invalid/invalid))
  ([f kw]
   (fn [v _]
     (cond
       (nil? v) v
       (not (f v)) kw
       :else v))))

(defn re-validator
  [re & kw]
  (apply validator #(re-find re %) kw))

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

(defelement ::choose-one
  "Multiple choice input with a single value, e.g. radio buttons."
  :extends ::iliad/multi-input
  :validate [(fn [v e]
               (if (contains? (::iliad/children e) v)
                 v
                 :invalid/invalid-choice))])

(defelement ::radio-group :extends ::choose-one)

(defelement ::choose-many
  "Checkbox input (multiple)."
  :extends ::iliad/multi-input
  :coerce (fn [v _] (if (sequential? v)
                      (seq v)
                      :invalid/not-seq))
  :validate [(fn [v e]
               (if (set/subset? (set v) (set (::iliad/children e)))
                 v
                 :invalid/invalid-choice))])

(defelement ::checkbox-group :extends ::choose-many)


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


(defn nest-vec
  [v]
  (cond
    (empty? v) []
    (vector?   (first v)) v
    :else      (vector v)))


(spec/def ::html-element-map
  (spec/keys :req-un [::tag]
             :opt-un [:html/id ::classes ::content]))


(spec/def ::html-input-map
  (spec/merge ::html-element-map
              (spec/keys :req [:html/type :html/id]
                         :opt [:html/name :html/value :html/pattern])))


(spec/def ::html-input-group-map
  (spec/keys :req-un [::input]
             :opt-un [::parent ::before ::after]))


(spec/def ::tag (spec/or keyword? string?))

(spec/def :html/type (spec/or keyword? string?))

(spec/def :html/id (spec/or keyword? string?))

(spec/def ::classes (spec/coll-of string?))

(spec/def ::content (spec/* (spec/alt :text    string?
                                      :element ::html-element-map)))

(spec/def :html/name ::id)

(spec/def :html/pattern (spec/or string? #(instance? java.util.regex.Pattern %)))

(spec/def ::input ::html-input-map)

(spec/def ::parent ::html-element-map)

(spec/def ::before ::content)

(spec/def ::after ::content)


(defprotocol Hiccupable
  (build-hiccup [opts]))


(defn build-hiccup-from-map
  [{:keys [tag classes content] :as opts}]
  (let [attr-map (reduce (fn [m [k v]]
                           (if (= "html" (namespace k))
                             ;; NOTE: Does it need to be a keyword?
                             (assoc m (name k) v)
                             m))
                         (array-map :class (str/join " " classes))
                         opts)]
    (into [tag attr-map] (map build-hiccup content))))


(extend-protocol Hiccupable
  String
  (build-hiccup [s] s)

  clojure.lang.IPersistentMap
  (build-hiccup [opts]
    (build-hiccup-from-map opts))

  clojure.lang.IPersistentVector
  (build-hiccup [v] v))


(defn build-hiccup-input-group
  [{:keys [id parent before after tag classes name type] :as opts}]
  (let [id (id->text id)
        name (or name id)
        parent (or :parent {:tag :div :content []})]))


(defn bs4-checkbox
  [{:keys [label name id value radio-or-checkbox]}]
  (build-hiccup-input {:parent [:div.form-check]
                       :id id
                       :name (or name id)
                       :value (:or value label)
                       :tag :input
                       :classes ["form-check-input"]
                       :after [:label
                               {:for id
                                :class "form-check-label"}
                               label]
                       :type (clojure.core/name radio-or-checkbox)}))


;;TODO: Figure a way to assign ids to individual boxes in a group.
#_(defn bs4-choice-input
  [inputs radio-or-checkbox]
  (into [:div]
        (for [checkbox-opts inputs]
          (bs4-checkbox {:radio-or-checkbox radio-or-checkbox
                         :id (str (name id))}))))


(defn merge-input-opts
  [m1 m2]
  (reduce (fn [m [k v]]
            (case k
              :attrs (update m :attrs merge)
              :before (update m :before (fnil #(into % (nest-vec)) []))
              :after (update m :after (fnil #(into % (nest-vec)) []))
              :classes (update m :classes conj)
              (assoc m k v)))
          m1 m2))


(defn bs4-basic-input
  ([label id]
   (bs4-basic-input label id {}))
  ([label id opts]
   (let [id-str (id->text id)]
     (build-hiccup-input
      (merge-input-opts {:id id-str
                         :tag :input
                         :before [:label {:for id-str} label]
                         :classes ["form-control"]
                         :parent [:div.form-group]}
                        opts)))))


(comment
  (build-hiccup el-def
                {:parent [:div.form-group]
                 :attrs {}
                 :before [:label {:for (id->text ::id)} "What is the thing?"]
                 :after [:div.error-message {:for (id->text ::id)}]
                 :tag :input
                 :id (id->text ::id)
                 :classes ["form-control"]
                 :label "Label"
                 :type "text"}))


(comment
  "I think this is how it should be done:"
  (defn build-hiccup-input
    [opts])

  (defmulti html-input (juxt ::iliad/context ::type))

  (defmethod html-input [::hiccup :basic]
    [opts])

  (defmethod html-input [::hiccup :checkbox]
    [opts])

  (defmethod html-input [::hiccup :multi]
    [opts])

  (defmethod html-input [::hiccup :select]
    [opts])

  (defn bs4-renderer
    "Do some stuff to the input before calling the render method. E.g.,
     turn keys in :html/ ns to plain keys."
    [type opts]
    (fn [el]
      (html-input (do-stuff el (assoc opts ::type type)))))

  "Actually, new way"

  (defn bs4-renderer
    [type opts]
    (fn [html-data]
      (-> html-data
          (html-merge {:type type})
          bs4-styling
          ->hiccup-element
          hiccup-render)))

  (defmulti merge-html-attr
    "Merge a new value into an existing value in an attribute map"
    (fn [m k v] k))

  (defn html-merge
    "Merge a map of options into an html attribute map nested in html data.
  Ignores any namespaced keywords outside of the :html/ namespace."
    [html-data attrs]
    (update html-data :attrs
            #(reduce (fn [m [k v]]
                       (case (namespace k)
                         nil    (merge-html-attr m k v)
                         "html" (merge-html-attr m (keyword (name k)) v)
                         m))
                     % attrs)))

  (defmethod merge-html-attr :default
    [m k v]
    (assoc m k v))

  (defmethod merge-html-attr :class
    ))


(defn bs4-renderer
  [opts]
  (fn [el+opts]
    (bs4-basic-input (::iliad/prompt el+opts)
                     (::iliad/id el+opts)
                     opts)))


#_(defcontext ::hiccup-bs4
  "A context for rendering to Bootstrap 4 using hiccup."
  :extends ::iliad/default-context
  ;; TODO: Maybe a multimethod for input type after all?
  :render [::iliad/text (bs4-renderer {:type "text"})
           ::iliad/num (bs4-renderer {:type "number"})
           ::phone (bs4-renderer {:type "tel"})
           ::zip (bs4-renderer {:attrs {:type "text"
                                        :pattern (str zip-regex)}})
           ::email (bs4-renderer {:type "email"})
           ::textarea (fn [_ el+opts]
                       (bs4-basic-input (::iliad/prompt el+opts)
                                        (::iliad/id el+opts)
                                        {:tag :textarea
                                         :attrs {:rows (or (:rows el+opts) 3)}}))
           #_::radio-group ]

  ;; TODO: Define renderers for other types
  )

(comment

  "OK, everything above is WAYYY too much. Let's try this:"
  (defn input
    [type & [attrs]]
    [:input (assoc attrs :type type)])

  (defn input-group
    [label type & [attrs]]
    [:div
     [:label label]
     [:input attrs label]])

  (defn update-attribute
    [hic-el k f & args]
    (if (map (hic-el 1))
      (apply update-in hic-el [1 k] f args)
      (into [(first hic-el) (apply update {} k f args)] (rest hic-el))))

  (defn add-class
    [hic-el & classes]
    (update-attribute hic-el :class #(if (empty? %)
                                       (str/join " " classes)
                                       (str/join " " (conj classes %))))))

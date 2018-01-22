(ns iliad.core
  (:require [clojure.spec.alpha :as spec]))

(spec/def ::input-element
  (spec/keys :req [::id ::type ::prompt]
             :opt [::children]
             :opt-un [::tip]))

(spec/def ::id keyword?)
(spec/def ::type (spec/and keyword? namespace))
(spec/def ::prompt string?)
(spec/def ::tip string?)
(spec/def ::children (spec/coll-of (spec/or :string string? :element ::input-element)))

;;Basic types: single-input (with text, num), title, group, multi-input


(defmulti coerce
  "Coerce an input into a different type/form. Performed before validation.
  Should throw ex-info on failure."
  (fn [_ element context]
    [(::type element) context]))


(defmulti validate
  "Validates an input, performed after coercion.
  Must be called on a coerced value.
  Passes thru value on success, returns :iliad.core/invalid on failure."
  (fn [_ element]
    (::type element)))


(defmulti error-msg
  "Generates an error message appropriate for the context.
  Called on the uncoerced value of the input."
  (fn [_ element context]
    [(::type element) context]))


(defmulti render
  "Renders an input element for display in the appropriate context."
  (fn [element context]
    [(::type element) context]))


(defn valid?
  [v e c]
  (let [coerced (try (coerce v e c)
                     (catch #?(:clj Throwable :cljc js/Object) _ false))]
    (and coerced (not= ::invalid (validate coerced e)))))


(defmethod coerce :default
  [value _ _]
  value)


(defmethod validate :default
  [value _ _]
  value)


(defmethod error-msg :default
  [v e c]
  (if (valid? v e c)
    ""
    (let [{:keys [::prompt ::tip]} e]
      (str "The value entered for \"" prompt "\" is invalid. " tip))))


(defmethod render :default
  [element context]
  (let [t (::type element)]
    (throw (ex-info (str "No method to render element type " t " in context " context)))))


(defmacro try-coerce
  [body msg map]
  `(try ~body
        (catch #?(:clj Throwable :cljs js/Object) ex#
          (throw (ex-info ~msg ~map ex#)))))


(defn html-input
  [id prompt]
  [:div [:label prompt]
   [:input {:name id :id id}]])


(comment
  (derive ::text ::single-input)

  (defmethod coerce [::text ::default-context]
    [v e c]
    (try (clojure.string/trim (str v))
         (catch #?(:clj Throwable :cljs js/Object) ex
           (throw (ex-info (str "Value of " (::id e) " could not be coerced to a string.")
                           {:value v
                            :element e
                            :context c}
                           ex)))))


  (defmethod render [::single-input ::html]
    [element _]
    (let [{:keys [::id ::prompt]} element]
      (html-input id prompt))))


(defmulti method-def first)

(defmethod method-def 'coerce [_]
  (spec/cat :method-name #{'coerce}
            :argv (spec/coll-of symbol? :kind vector? :count 3)
            :body any?))

(defmethod method-def 'error-msg [_]
  (spec/cat :method-name #{'error-msg}
            :argv (spec/coll-of symbol? :kind vector? :count 3)
            :body any?))

(defmethod method-def 'validate [_]
  (spec/cat :method-name #{'validate}
            :argv (spec/coll-of symbol? :kind vector? :count 2)
            :body any?))

(defmethod method-def 'render [_]
  (spec/cat :method-name #{'render}
            :argv (spec/coll-of symbol? :kind vector? :count 2)
            :body any?))


(def ^:private this-ns *ns*)
(defn method-helper
  [type context {:keys [method-name argv body]}]
  (let [d-val (case method-name
                coerce [type context]
                validate type
                error-msg [type context]
                render [type context])]
    `(defmethod ~(symbol (str this-ns) (name method-name)) ~d-val
       ~argv
       ~body)))


(spec/def ::defelement-args
  (spec/cat
   :name (spec/and keyword? namespace)
   :ex-sym #{'extends}
   :parent (spec/and keyword? namespace)
   :doc? (spec/? string?)
   :spec (spec/? (spec/spec (spec/cat :spec #{'spec} :form any?)))
   :body (spec/* (spec/multi-spec method-def ::method-def))))

(spec/fdef defelement
           :args ::defelement-args)

(defmacro defelement
  [& args]
  (let [{:keys [name parent doc? spec body]} (spec/conform ::defelement-args args)
        d-val [name ::default-context]]
    `(do
       (derive ~name ~parent)
       ~(when spec
          `(spec/def ~name (spec/merge ::input-element ~spec)))
       ~@(for [form body]
           (method-helper name ::default-context form)))))


(comment
  ;; Old way
  (defelement ::text extends ::single-input
    "This is a genereic text input element"
    :coerce (fn [v e c] (try-coerce (clojure.string/trim (str v))
                                    (str "Value of " (::id e) " could not be coerced to a string.")
                                    {:value v
                                     :element e
                                     :context c}))
    :render (fn [e c] (let [{:keys [::id ::prompt]} e]
                        (html-input id prompt))))

  ;; New Way
  (defelement ::text extends ::single-input
    "This is a generic text input element."
    (coerce [v e c] (try-coerce (clojure.string/trim (str v))
                                (str "Value of " (::id e) " could not be coerced to a string.")
                                {:value v
                                 :element e
                                 :context c}))
    (render [e c] (let [{:keys [::id ::prompt]} e]
                    (html-input id prompt)))))


(defn parse-num
  [n]
  #?(:clj
     (Double/parseDouble n)
     :cljs
     (let [res (Number/parseFloat n)]
       (if (= res ##NaN)
         (throw (ex-info (str n " could not be parsed as a number")))))))


(defmacro catch-fail
  [body]
  `(try ~body
        (catch #?(:clj Throwable :cljc js/Object) _# ::fail)))


(comment
  (derive ::num ::single-input)

  (spec/def ::num (spec/merge ::input-element
                              (spec/keys :opt-un [::num-low ::num-high]))))


(defn coerce-num
  [v e c]
  (try (parse-num v)
       (catch #?(:clj Throwable :cljs js/Object) ex
         (throw (ex-info (str "Value of " (::id e) " could not be coerced to a number.")
                         {:value v
                          :element e
                          :context c}
                         ex)))))


(spec/def ::num-low number?)
(spec/def ::num-high number?)

(defn validate-num
  [v e]
  (let [low (or (:num-low e) (Double/NEGATIVE_INFINITY))
        high (or (:num-high e) (Double/POSITIVE_INFINITY))]
    (if (<= low v high)
      v
      ::invalid)))



(defn error-msg-num
  [v e c]
  (let [coerced (catch-fail (coerce v e c))
        prompt (::prompt e)
        low (:num-low e)
        high (:num-high e)]
    (cond (valid? v e c)
          ""

          (= ::fail coerced)
          (str prompt " must be a number")

          (and low (< coerced low))
          (str prompt " must be at least " low)

          (and high (> coerced high))
          (str prompt " must be no greater than " high))))


(comment
  (defelement ::num extends ::single-input
    "An input that should be coerced to a number, with high/low values"
    :spec (spec/keys :opt-un [::num-low ::num-high])
    :coerce coerce-num
    :validate validate-num
    :error-msg error-msg-num))


(def text-el-test
  {::id :text-el-test
   ::type ::text
   ::prompt "Test text element"
   :tip "This is a test text element"})

(def num-el-test
  {::id :num-el-test
   ::type ::num
   ::prompt "Test number element"
   :tip "This is a test number element"
   :num-low 0
   :num-high 99})

(comment
  (error-msg "abc" num-el-test ::default-context)
  (error-msg "-4" num-el-test ::default-context)
  (error-msg "100" num-el-test ::default-context)
  (error-msg "7" num-el-test ::default-context))


(defmulti context-method-def first)

(defmethod context-method-def 'coerce [_]
  (spec/cat :method-name #{'coerce}
            :type (spec/and keyword? namespace)
            :argv (spec/coll-of symbol? :kind vector? :count 3)
            :body any?))

(defmethod context-method-def 'error-msg [_]
  (spec/cat :method-name #{'error-msg}
            :type (spec/and keyword? namespace)
            :argv (spec/coll-of symbol? :kind vector? :count 3)
            :body any?))

(defmethod context-method-def 'render [_]
  (spec/cat :method-name #{'render}
            :type (spec/and keyword? namespace)
            :argv (spec/coll-of symbol? :kind vector? :count 2)
            :body any?))


(spec/def ::defcontext-args
  (spec/cat
   :name (spec/and keyword? namespace)
   :ex-sym #{'extends}
   :parent (spec/and keyword? namespace)
   :doc? (spec/? string?)
   :body (spec/* (spec/multi-spec context-method-def ::context-method-def))))

(defmacro defcontext
  [& args]
  (let [{:keys [name parent doc? body]} (spec/conform ::defcontext-args args)]
    `(do
       (derive ~name ~parent)
       ~@(for [{:keys [type] :as form} body]
           (method-helper type name form)))))

(comment
  (defcontext ::html extends ::default-context
    "A context for html forms."
    :render ::single-input (fn [e c] (let [{:keys [::id ::prompt]} e]
                                       (html-input id prompt))))

  ;; Which syntax looks better?
  (defcontext ::html extends ::default-context
    "A context for html forms."
    (render ::single-input [e c]
            (let [{:keys [::id ::prompt]} e]
              (html-input id prompt)))
    (error-msg ::single-input [v e c]
               (str v " is not a valid value.")))

  (defcontext ::json extends ::default-context
    "A context for json input"
    :coerce ::num (fn [v _ _] (if (number? v) v (throw (ex-info "Not a number"))))))


(spec/def ::element-data-form
  (spec/cat ::id ::id
            ::type ::type
            ::prompt ::prompt
            ::map (spec/map-of keyword? any?)
            ::children (spec/* (spec/or :string string? :element ::element-data-form))))


(comment
  (def text-el-form-test
    [:text-el-test ::text "Test text element" {:tip "This is a test text element"}])

  (def num-el-form-test
    [:num-el-test ::num "Test number element" {:tip "This is a test number element"
                                               :num-low 0
                                               :num-high 99}]))


(defn parse-element-data
  [form]
  (if (spec/valid? ::element-data-form form)
    (let [{:keys [::map ::type] :as conformed} (spec/conform ::element-data-form form)
          flat-map (-> conformed
                       (merge map)
                       (dissoc ::map))]
      (cond
        (not (spec/get-spec type))
        flat-map

        (spec/valid? type flat-map)
        flat-map

        :else
        (throw (ex-info (spec/explain-str type flat-map)
                        {:element-def form
                         :element-map flat-map
                         :explain-data (spec/explain-data type flat-map)}))))
    (throw (ex-info (spec/explain-str ::element-data-form form)
                    {:element-def form
                     :explain-data (spec/explain-data ::element-data-form form)}))))


(spec/def ::form-def
  (spec/keys
   :req-un [::form-inputs]
   :opt-un [::required-inputs]))

(spec/def ::form-inputs (spec/coll-of ::input-element))

(spec/def ::required-inputs (spec/coll-of ::id))

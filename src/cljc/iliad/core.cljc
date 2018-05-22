(ns iliad.core
  (:require [clojure.spec.alpha :as spec]))

(spec/def ::input-element
  (spec/keys :req [::id ::type ::prompt]
             :opt [::children]))

(spec/def ::id keyword?)
(spec/def ::type (spec/and keyword? namespace))
(spec/def ::prompt string?)
(spec/def ::children (spec/coll-of (spec/or :string string? :element ::input-element)))

;;Basic types: single-input (with text, num), title, group, multi-input


;;----------------------------------------
;; Basic multimethods for using form elements.


(defmulti coerce
  "Coerce an input into a different type/form. Performed before validation.
  Should throw ex-info on failure.
  Values should only coerce to nil to indicate absence."
  (fn [_ element context]
    [(::type element) context]))


(defn invalid-result?
  "Helper function to determine whether the result of `validate` is invalid."
  [x]
  (and (keyword? x)
       (= "invalid" (namespace x))))

(defmulti validate
  "Validates an input, performed after coercion.
  Must be called on a coerced value, because it assumes data is in the form in
  which it will be used by the application (hence it is independent of context).
  Passes thru value on success, returns a keyword in the `invalid`namespace on
  failure, e.g. `:invalid/invalid`."
  (fn [_ element]
    (::type element)))


(defmulti error-msg
  "Generates an error message appropriate for the context.
  Called on the uncoerced value of the input."
  (fn [_ element context]
    [(::type element) context]))


(defmulti render
  "Renders an input element for display in the appropriate context."
  (fn [_ element context]
    [(::type element) context]))


;;----------------------------------------
;; Default behaviors


(defmethod coerce :default
  [value _ _]
  value)


(defmethod validate :default
  [value _ _]
  value)


(defn valid?
  "Check whether a particular raw input is valid."
  [v e c]
  (let [coerced (try (coerce v e c)
                     (catch #?(:clj Throwable :cljc js/Object) _ :invalid/invalid))]
    (and (not= :invalid/invalid coerced)
         (not (invalid-result? (validate coerced e))))))


;; Provides a generic error message
(defmethod error-msg :default
  [v e c]
  (if (valid? v e c)
    ""
    (let [prompt (::prompt e)]
      (str "The value entered for \"" prompt "\" is invalid."))))


;; Throw an exception when calling render when not defined for the type/context.
(defmethod render :default
  [value element context]
  (let [t (::type element)]
    (throw (ex-info (str "No method to render element type " t " in context " context)))))


;;----------------------------------------
;; Element Definition Macro
;; Automatically derives element types and defines methods for the default
;; context.


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
            :argv (spec/coll-of symbol? :kind vector? :count 3)
            :body any?))


;; TODO: Seems like a hack, is this needed?
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
   :defs (spec/* (spec/multi-spec method-def ::method-def))))

(spec/fdef defelement
           :args ::defelement-args)

(defmacro defelement
  [& args]
  (let [{:keys [name parent doc? defs]} (spec/conform ::defelement-args args)
        d-val [name ::default-context]]
    `(do
       (derive ~name ~parent)
       ;; TODO: Had some trouble spec-ing the spec def. Fix this above.
       #_~(when spec
          `(spec/def ~name (spec/merge ::input-element ~spec-form)))
       ~@(for [form defs]
           (method-helper name ::default-context form)))))


;;----------------------------------------
;; Context Definition Macro
;; Automatically derives from the default context and defines methods for
;; various types. No definitions for validate here as it is independent of
;; context.


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
            :argv (spec/coll-of symbol? :kind vector? :count 3)
            :body any?))


(spec/def ::defcontext-args
  (spec/cat
   :name (spec/and keyword? namespace)
   :ex-sym #{'extends}
   :parent (spec/and keyword? namespace)
   :doc? (spec/? string?)
   :body (spec/* (spec/multi-spec context-method-def ::context-method-def))))

(spec/fdef defcontext
           :args ::defcontext-args)

(defmacro defcontext
  [& args]
  (let [{:keys [name parent doc? body]} (spec/conform ::defcontext-args args)]
    `(do
       (derive ~name ~parent)
       ~@(for [{:keys [type] :as form} body]
           (method-helper type name form)))))


;;----------------------------------------
;; Data DSL Spec and Parser
;; Allows developers to specify elements in an input form using vectors.


(spec/def ::element-data-seq
  (spec/cat ::id ::id
            ::type ::type
            ::prompt ::prompt
            ::map (spec/map-of keyword? any?)
            ;; TODO: This should either be a seq of strings or data seqs, not both
            ::children (spec/* (spec/or :string string? :element ::element-data-seq))))


(comment
  (def text-el-form-test
    [:text-el-test ::text "Test text element" {:tip "This is a test text element"}])

  (def num-el-form-test
    [:num-el-test ::num "Test number element" {:tip "This is a test number element"
                                               :num-low 0
                                               :num-high 99}]))


(defn parse-element-data
  [data-seq]
  (if (spec/valid? ::element-data-seq data-seq)
    (let [{:keys [::map ::type] :as conformed} (spec/conform ::element-data-seq data-seq)
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
                        {:element-def data-seq
                         :element-map flat-map
                         :explain-data (spec/explain-data type flat-map)}))))
    (throw (ex-info (spec/explain-str ::element-data-seq data-seq)
                    {:element-def data-seq
                     :explain-data (spec/explain-data ::element-data-seq data-seq)}))))


;; Required inputs are specified separately, as they may be required or not
;; in different situations.
(spec/def ::form-def
  (spec/keys
   :req-un [::form-inputs]
   :opt-un [::required-inputs]))

(spec/def ::form-inputs (spec/coll-of ::input-element))

(spec/def ::required-inputs (spec/coll-of ::id))


(spec/fdef process-form
           :args (spec/cat
                  :input (spec/map-of :keyword? any?)
                  :form-def ::form-def
                  :context keyword?
                  :opts (spec/alt :post-fn fn?
                                  :pre-fn fn?))

           :ret (spec/keys :req-un [:form/input :form/result :form/error-data :form/error-msgs
                                    :form/valid? :form/def]))

(defn map-form-fn
  "Maps coerce, validate, or error-msg over a form map."
  ([f form-map element-map]
   (map (fn [[k v]]
          (f v (get element-map k)))
        form-map))
  ([f form-map element-map context]
   (map (fn [[k v]]
          (f v (get element-map k) context))
        form-map)))

(defn remove-empty-keys
  [m]
  (reduce (fn [ma [k v]] (if (nil? v) ma (assoc ma k v)))))

(defn merge-keys-nil
  [m ks]
  (merge (zipmap ks (repeat nil)) m))

(defn validate-pass-nil
 "Helper function to avoid validating nil values."
 [v e]
 (if (nil? v) nil (validate v e)))

(defn process-form
  [input form-def context & {:keys [post-val-fn]}]
  (let [input-defs (:form-inputs form-def)
        required-inputs (:required-inputs form-def)
        ;; Create a map of elements by id
        element-map (reduce (fn [m input-def]
                              ;; Skip elements that are titles
                              ;; TODO: Deal with nexted titles?
                              (if (isa? (::type input-def) ::title)
                                m
                                (assoc m (::id input-def) input-def)))
                            {} input-defs)
        ;; working input excludes all keys not included in the form def,
        ;; except those listed as required, which are all included
        ;; TODO: is there a chance for error if an element not included
        ;; in the def is listed as required?
        working-input (-> input
                          (select-keys (keys element-map))
                          (remove-empty-keys)
                          (merge-keys-nil required-inputs))
        coerced (map-form-fn coerce working-input element-map context)
        validations (map-form-fn validate-pass-nil working-input element-map)
        error-data (reduce (fn [errs [k v]]
                             (assoc errs k (cond
                                             ;; Check that required vals are there
                                             (and (nil? v) (contains? required-inputs k))
                                             :invalid/required
                                             ;; Check for invalid results
                                             (invalid-result? v)
                                             v
                                             ;; Valid returns nil
                                             :else-valid
                                             nil))))
        error-msgs (map-form-fn error-msg working-input element-map context)]
    {:form/input input
     :form/result validations
     :form/error-data error-data
     :form/error-msgs error-msgs
     :form/valid? (every? nil? error-data)
     :form/form-def form-def}))


;;----------------------------------------
;; Base element definitions


;; TODO: Do we actually need this function?
(defmacro try-coerce
  [body msg map]
  `(try ~body
        (catch #?(:clj Throwable :cljs js/Object) ex#
          (throw (ex-info ~msg ~map ex#)))))


(defelement ::text extends ::single-input
  "This is a generic text input element. Blank strings coerce to nil."

  (coerce [v e c] (try-coerce (let [s (clojure.string/trim (str v))]
                                (if (clojure.string/blank? s)
                                  nil
                                  s))
                              (str "Value of " (::id e) " could not be coerced to a string.")
                              {:value v
                               :element e
                               :context c})))


(defn parse-num
  [n]
  #?(:clj
     (Double/parseDouble n)
     :cljs
     (let [res (Number/parseFloat n)]
       (if (= res ##NaN)
         (throw (ex-info (str n " could not be parsed as a number")))))))


(spec/def ::num-low number?)
(spec/def ::num-high number?)


(defelement ::num extends ::single-input
  "An input that should be coerced to a number, with optional high/low values."

  (coerce [v e c] (try (parse-num v)
                       (catch #?(:clj Throwable :cljs js/Object) _
                         :invalid/NaN)))

  (validate [v e] (let [low (or (:num-low e) (Double/NEGATIVE_INFINITY))
                        high (or (:num-high e) (Double/POSITIVE_INFINITY))]
                    (cond
                      (invalid-result? v) v
                      (< v low) :invalid/too-low
                      (< v high) :invalid/too-high
                      :valid v)))

  (error-msg [v e c] (let [validation (validate (coerce v e c) e)
                           prompt (::prompt e)]
                       (if (invalid-result? validation)
                         (case validation
                           :invalid/NaN      (str prompt " must be a number")
                           :invalid/too-low  (str prompt " must be at least " (:num-low e))
                           :invalid/too-high (str prompt " must be no greater than " (:num-high e))
                           :invalid/invalid)
                         ""))))


(comment
  (derive ::num ::single-input)

  (spec/def ::num (spec/merge ::input-element
                              (spec/keys :opt-un [::num-low ::num-high]))))


(comment (def text-el-test
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

         (error-msg "abc" num-el-test ::default-context)
         (error-msg "-4" num-el-test ::default-context)
         (error-msg "100" num-el-test ::default-context)
         (error-msg "7" num-el-test ::default-context))




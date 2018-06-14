(ns iliad.core
  (:require [clojure.spec.alpha :as spec]
            [clojure.string :as str]
            [clojure.set :as set]))


(spec/def ::input-element
  (spec/keys :req [::id ::type ::prompt]
             :opt [::children]))

(spec/def ::ns-kw (spec/and keyword? #(namespace %)))

(spec/def ::id keyword?)
(spec/def ::type ::ns-kw)
(spec/def ::prompt string?)
(spec/def ::children (spec/or :strings (spec/coll-of (spec/or :label string?
                                                              :label+value (spec/tuple string? string?)))
                              :elements (spec/coll-of ::input-element)))

;;Basic types: single-input (with text, num), title, group, multi-input
;; Actually, Non-input types such as titles should be input to the whole form
;; rendering function, no need to include them in the element list, eh?


;;----------------------------------------
;; Basic multimethods for writing/extendings elements/contexts.

(defmulti coerce (fn [_ m] ((juxt ::type ::context) m)))


(defmulti validate (fn [_ opts] (::type opts)))


(defn invalid-result?
  "Returns true on a kw in the invalid ns or a coll of such keywords."
  [x]
  (let [invalid-kw? #(and (keyword? %)
                          (= "invalid" (namespace %)))]
    (if (and (sequential? x) (seq x))
      (every? invalid-kw? x)
      (invalid-kw? x))))


(defmulti error-msg
  ;; If a map is provided, dispatch on type and context to coerce and validate
  ;; first. If an error keyword is provided, dispatch on kw. If a seq of
  ;; keywords is provided, dispatch on the special value ::invalid-seq
  (fn
    ([_ opts-or-inv]
     (cond
       (map? opts-or-inv)                  ((juxt ::type ::context) opts-or-inv)
       (keyword? opts-or-inv)              opts-or-inv
       (and (sequential? opts-or-inv)
            (every? keyword? opts-or-inv)) ::invalid-seq))
    ([_ inv opts]
     (let [{:keys [::type ::context]} opts]
       (if (sequential? inv)
         [::invalid-seq type context]
         [inv type context])))))


(defmulti render (juxt ::type ::context))


(defmethod error-msg :default
  ([v opts-or-inv]
   (cond
     (map? opts-or-inv) (let [coerced (coerce v opts-or-inv)
                             result (cond
                                      (invalid-result? coerced) coerced
                                      :else (validate coerced opts-or-inv))
                             valid? (not (invalid-result? result))]
                         (if (not valid?)
                           (error-msg v result)))

     (keyword? opts-or-inv) (error-msg v :invalid/invalid)))
  ([v inv opts]
   (error-msg v inv)))


(defmethod error-msg :invalid/invalid
  [v kw]
  "The value entered is not valid.")


(defmethod error-msg :invalid/required
  [v kw]
  "This field is required.")


(defmethod error-msg ::invalid-seq
  ([v kws]
   (mapv (partial error-msg v) kws))
  ([v kws opts]
   (mapv #(error-msg v % opts) kws)))


;;----------------------------------------
;; Element Definition Macro
;; Automatically derives element types and defines methods for the default
;; context.


;; Do we want to expose this or just return the value/errors?
(spec/def ::validation-result
  (spec/keys :req-un [::errors ::valid? ::value]))


(defn ^:private merge-val-results
  [res new]
  (let [is-valid (not (invalid-result? new))]
    (-> res
        (update :errors #(if is-valid % (conj % new)))
        (update :valid? (fnil #(and %1 is-valid) true)))))


(defn ^:private lift-validation-fn
  [f]
  (fn [res opts]
    (merge-val-results res (f (:value res) opts))))


(defn do-validations
  "Helper function for generating a single validator from a list of fns."
  [fs v opts]
  (let [{:keys [value errors valid?]} (reduce #(%2 %1 opts)
                                              {:value v}
                                              (map lift-validation-fn fs))]
    (if valid?
      value
      errors)))


(defn ^:private pred->validator
  [f invalid-kw]
  (fn [x]
    (if (f x)
      x
      invalid-kw)))


(comment
  (def num-vals [(pred->validator #(= 0 (mod % 3)) :invalid/not-div-3)
                 (pred->validator #(< 1 %) :invalid/less-than-1)
                 (pred->validator odd? :invalid/too-even)])

  (do
    (println '(do-validations num-vals 9))
    (println (do-validations num-vals 9))

    (println '(do-validations num-vals 5))
    (println (do-validations num-vals 5))

    (println '(do-validations num-vals -8))
    (println (do-validations num-vals -8))))


#_(spec/def ::defelement-args
  (spec/map-of #{:extends :coerce :validate :error-msg :render :el-spec} any?))


(defn ^:private method-gen
  [k v ename context]
  (case k
    :coerce `(defmethod coerce [~ename ~context]
               [v# opts#]
               (~v v# opts#))
    :validate `(defmethod validate ~ename
                 [v# opts#]
                 (do-validations ~v v# opts#))
    :error-msg `(defmethod error-msg [~ename ~context]
                  [v# opts#]
                  (~v v# opts#))
    :render `(defmethod render [~ename ~context]
               [opts#]
               (~v opts#))
    nil))


;; TODO: Make this spec better
(spec/fdef defelement
           :args (spec/cat :name ::ns-kw
                           :doc? (spec/? string?)
                           :defelement-args (spec/* (spec/cat :kw keyword? :val any?))))

(defmacro defelement
  [ename & [doc? & kvs]]
  (let [kvs (if (string? doc?) kvs (cons doc? kvs))
        {:keys [extends coerce validate error-msg render el-spec] :as args} (apply array-map kvs)]
    `(do
       (derive ~ename ~(or extends ::single-input))
       (spec/def ~ename ~(or el-spec extends))
       ~@(for [[k v] args
               :let [form (method-gen k v ename ::default-context)]
               :when form] form))))


;;----------------------------------------
;; Context Definition Macro
;; Automatically derives from the default context and defines methods for
;; various types. No definitions for validate here as it is independent of
;; context.


;; TODO: Make this spec better
(spec/fdef defcontext
           :args (spec/cat :name ::ns-kw
                           :doc? (spec/? string?)
                           :extends (spec/cat :kw #{:extends} :parent ::ns-kw)
                           :defcontext-args (spec/* (spec/cat :kw keyword?
                                                              :vals (spec/spec (spec/* (spec/cat :el ::ns-kw :val any?)))))))

(defmacro defcontext
  [cname & [doc? & kvs]]
  (let [kvs (if (string? doc?) kvs (cons doc? kvs))
        {:keys [extends coerce error-msg render] :as args} (apply array-map kvs)]
    `(do
       (derive ~cname ~(or extends ::default-context))
       ;; TODO: Can we do better than the stupid hack below?
       ~@(for [[k vs] (dissoc args :extends)
               [ename v] (partition 2 vs)
               :let [form (method-gen k v ename cname)]
               :when form] form))))

(comment
  (defcontext ::html-form
    :coerce [::num (fn [v _] (try (Double/parseDouble v) (catch Throwable _ :invalid/NaN)))
             ::text (fn [v _] (if (clojure.string/blank? v) nil (clojure.string/trim v)))]))


;;----------------------------------------
;; Data DSL Spec and Parser
;; Allows developers to specify elements in an input form using vectors.


(spec/def ::element-data-seq
  (spec/and
   vector?
   (spec/cat ::id ::id
             ::type ::type
             ::prompt ::prompt
             ::map (spec/? (spec/map-of keyword? any?))
             ;; TODO: Should multiple nestings be allowed?
             ::children (spec/? (spec/or :strings (spec/* (spec/alt :label string?
                                                                    :label+value (spec/tuple string? string?)))
                                         :elements (spec/* ::element-data-seq))))))


(comment
  (def text-el-form-test
    [:text-el-test ::text "Test text element" {:tip "This is a test text element"}])

  (def num-el-form-test
    [:num-el-test ::num "Test number element" {:tip "This is a test number element"
                                               :num-low 0
                                               :num-high 99}]))

(defn ^:private update-when
  "Only updates a map when the key is present"
  [m k f & args]
  (if (k m)
    (apply update m k f args)
    m))

(defn parse-element-data
  [data-seq]
  (if (spec/valid? ::element-data-seq data-seq)
    (let [{:keys [::map ::type] :as conformed} (spec/conform ::element-data-seq data-seq)
          flat-map (-> conformed
                       ;; NOTE: This results in all parsed element defs containing ::children
                       (update-when ::children #(case (first %)
                                                  nil nil
                                                  :strings (mapv second (second %))
                                                  :elements (mapv second %)))
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


(defmacro defform
  [name {:keys [form-inputs required-inputs]}]
  `(def ~name
     {:form-inputs (mapv parse-element-data ~form-inputs)
      :required-inputs ~required-inputs}))


;;----------------------------------------
;; Form data coercion and validation


(defn coerce+validate
  "Used to coerce and validate a single value in one step."
  [v opts]
  (let [coerced (coerce v opts)]
    (if (invalid-result? coerced)
      coerced
      (validate coerced opts))))


;; Required inputs are specified separately, as they may be required or not
;; in different situations.
(spec/def ::form-def
  (spec/keys
   :req-un [::form-inputs]
   :opt-un [::required-inputs]))

(spec/def ::form-inputs (spec/coll-of ::input-element))

(spec/def ::required-inputs (spec/coll-of ::id))

(spec/def :form/input (spec/map-of keyword? any?))

(spec/def :form/result (spec/map-of keyword? any?))

(spec/def :form/error-data (spec/or :single invalid-result?
                                    :multi  (spec/coll-of invalid-result?)))

(spec/def :form/error-msg (spec/or :single string?
                                   :multi  (spec/coll-of string?)))

(spec/def :form/valid? boolean?)

(spec/def :form/def ::form-def)

(spec/fdef process-form
           :args (spec/cat
                  :input (spec/map-of :keyword? any?)
                  :form-def ::form-def
                  :context keyword?)

           :ret (spec/keys :req-un [:form/input :form/result :form/error-data :form/error-msg
                                    :form/valid? :form/def]))

;; This is slow. But too slow?
;; TODO: still need to make a fn that returns a form validator
(defn process-form
  [input form-def context]
  (let [input-defs (:form-inputs form-def)
        required-inputs (set (:required-inputs form-def))
        ;; Below returns a sequence of [key value opts] vectors
        working-input (let [form-def-by-id (zipmap (map ::id input-defs) input-defs)
                            defined-key-set (set (keys form-def-by-id))]
                        (if (not (set/subset? required-inputs defined-key-set))
                          (throw (ex-info "The provided required inputs are not completely defined in the form definition" form-def))
                          (->> input
                               (merge (zipmap defined-key-set (repeat nil)))
                               (filter (comp defined-key-set first))
                               (map (fn [[k v]] [k v (assoc (get form-def-by-id k) ::context context)])))))]
    (reduce (fn [ret [k v opts]]
              (let [coerced (coerce v opts)
                    result (cond
                             (and (nil? coerced) (required-inputs k))
                             :invalid/required

                             (invalid-result? coerced)
                             coerced

                             :else
                             (validate coerced opts))
                    valid? (not (invalid-result? result))
                    err-msg (if (not valid?)
                              (error-msg v result))]
                (-> ret
                    (update :form/result assoc k result)
                    (update :form/error-data assoc k (if-not valid? result))
                    (update :form/error-msg assoc k err-msg)
                    (update :form/valid? #(and % valid?)))))
            {:form/def form-def
             :form/input input
             :form/valid? true
             :form/result {}
             :form/error-data {}
             :form/error-msg {}}
            working-input)))


(defmacro ^:private try>
  "Try to eval, return a pre-specified value on failure."
  [body fail-val]
  `(try ~body
        (catch Throwable _# ~fail-val)))


(spec/def ::single-input ::input-element)

(spec/def ::multi-input (spec/merge ::input-element
                                    (spec/keys :req [::children])))


;; TODO: Is validation specification optional??
(defelement ::text
  :extends ::single-input
  ;; Should the raw value be coerced to a string first?
  :coerce (fn [v _] (if (nil? v)
                      v
                      (let [s (str/trim v)] (if-not (str/blank? s) s))))
  :validate [(fn [v _] v)])


(defelement ::num
  :extends ::single-input
  :coerce (fn [v _] (if (number? v) v (try> (Double/parseDouble v) :invalid/NaN)))
  :validate [(fn [v {:keys [::num-max ::num-min]}]
               (cond
                 (and num-min (< v num-min)) :invalid/too-low
                 (and num-max (> v num-max)) :invalid/too-high
                 :else-valid v))]
  :el-spec (spec/keys :opt [::max-val ::min-val]))


(comment
  (def test-form-def
    {:form-inputs [{::type ::text
                    ::id ::a
                    ::prompt "A"}
                   {::id ::b
                    ::type ::num
                    ::prompt "B"}]
     :required-inputs [::a ::b]})

  (process-form {::a "Hi" ::b 2} test-form-def ::default-context))


;; TODO: These base elements need to implement all methods
(defelement ::choose-one
  "Multiple choice input with a single value, e.g. radio buttons."
  :extends ::multi-input
  :validate [(fn [v e]
               (if (contains? (::children e) v)
                 v
                 :invalid/invalid-choice))])


(defelement ::choose-many
  "Checkbox input (multiple)."
  :extends ::multi-input
  :coerce (fn [v _] (if (sequential? v)
                      (seq v)
                      :invalid/not-seq))
  :validate [(fn [v e]
               (if (set/subset? (set v) (set (::children e)))
                 v
                 :invalid/invalid-choice))])

;;----------------------------------------
;; Some useful helper functions

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

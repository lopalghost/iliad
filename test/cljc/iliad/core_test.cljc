(ns iliad.core-test
  (:require [iliad.core :as core]
            #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer :all :include-macros true])))

(deftest defelement
  (testing "Macro produces correct code"
    (is (= (macroexpand-1 '(iliad.core/defelement ::text extends :iliad.core/single-input
                             "This is a generic text input element."
                             (coerce [v e c] (try-coerce (clojure.string/trim (str v))
                                                         (str "Value of " (::id e) " could not be coerced to a string.")
                                                         {:value v
                                                          :element e
                                                          :context c}))
                             (render [e c] (let [{:keys [::id ::prompt]} e]
                                             (html-input id prompt)))))
           '(do
             (clojure.core/derive ::text :iliad.core/single-input)
             nil
             (clojure.core/defmethod
               iliad.core/coerce
               [::text :iliad.core/default-context]
               [v e c]
               (try-coerce
                (clojure.string/trim (str v))
                (str "Value of " (::id e) " could not be coerced to a string.")
                {:value v, :element e, :context c}))
             (clojure.core/defmethod
               iliad.core/render
               [::text :iliad.core/default-context]
               [e c]
               (let [{:keys [::id ::prompt]} e] (html-input id prompt))))))))


(deftest defcontext
  (testing "Macro produces correct code"
    (is (= (macroexpand-1 '(iliad.core/defcontext ::html extends :iliad.core/default-context
                             "A context for html forms."
                             (render :iliad.core/single-input [e c]
                                     (let [{:keys [::id ::prompt]} e]
                                       (html-input id prompt)))
                             (error-msg :iliad.core/single-input [v e c]
                                        (str v " is not a valid value."))))
           '(do
              (clojure.core/derive
               :iliad.core-test/html
               :iliad.core/default-context)
              (clojure.core/defmethod
                iliad.core/render
                [:iliad.core/single-input :iliad.core-test/html]
                [e c]
                (let
                    [{:keys [:iliad.core-test/id :iliad.core-test/prompt]} e]
                  (html-input id prompt)))
              (clojure.core/defmethod
                iliad.core/error-msg
                [:iliad.core/single-input :iliad.core-test/html]
                [v e c]
                (str v " is not a valid value.")))))))

(ns clj-xml-validation.core-test
  (:require [clojure.test :refer :all]
            [clj-xml-validation.core :refer :all]))

(def validate (create-validation-fn (clojure.java.io/resource "example.xsd")))
(def validate-two (create-validation-fn (clojure.java.io/resource "example2.xsd") (clojure.java.io/resource "example3.xsd")))

(deftest obviously-wrong
  (testing "Validator throws exception on unparseable XML"
    (is (thrown? clojure.lang.ExceptionInfo (validate "<wrong>")))))

(deftest malformed
  (testing "Validator throws exception on malformed XML"
    (is (thrown? clojure.lang.ExceptionInfo (validate "<wrong<")))))

(deftest obviously-right
  (testing "Validator succeeds on obviously correct XML"
    (is (valid? (validate "<note><to></to><from></from><heading></heading><body></body></note>")))))

(deftest missing-element
  (testing "Should return an not valid? result"
    (is (not (valid? (validate "<note><to></to><heading></heading><body></body></note>")))))
  (testing "Error should be returned"
    (is (= (:message (first (errors (validate "<note><to></to><heading></heading><body></body></note>"))))
           "cvc-complex-type.2.4.a: Invalid content was found starting with element 'heading'. One of '{from}' is expected."))))

(deftest added-element
  (testing "Validator fails on XML with a non-schema element"
    (is (not (valid? (validate "<note><to></to><from></from><heading></heading><body></body><spurious/></note>")))))
  (testing "Error should be returned"
    (is (= (:message (first (errors (validate "<note><to></to><heading></heading><body></body></note>"))))
           "cvc-complex-type.2.4.a: Invalid content was found starting with element 'heading'. One of '{from}' is expected."))))

(deftest validates-either
  (testing "validator with 2 schemas validates both types of xml correctly"
    (is (valid? (validate-two (slurp "resources/example2.xml"))))
    (is (valid? (validate-two (slurp "resources/example3.xml"))))))

(deftest either-malformed
  (testing "validator with 2 schemas fails if either type of xml is malformed"
    (is (thrown? clojure.lang.ExceptionInfo (validate-two "<wrong<")))))


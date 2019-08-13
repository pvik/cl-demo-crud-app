(defpackage first-app/tests/main
  (:use :cl
        :first-app
        :rove))
(in-package :first-app/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :first-app)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))

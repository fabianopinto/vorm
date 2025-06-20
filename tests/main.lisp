;;;; main.lisp - Test implementation for VORM

(in-package :vorm/tests)

;; Define the test suite
(def-suite :vorm-tests
  :description "Test suite for the VORM system")

;; Use the test suite for the tests in this file
(in-suite :vorm-tests)

;; Main test functions will be added here as the project evolves

(defun run-tests ()
  "Run all the tests in the VORM test suite."
  (run! :vorm-tests))

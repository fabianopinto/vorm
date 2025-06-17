(in-package :vorm.tests)

;;;; Main test orchestration for vorm project

;; Define the main test suite for the entire system
(def-suite vorm-tests
  :description "Tests for the vorm system")

;; Helper function for comparing floating point values with a tolerance
;; This is duplicated from src/utils/math.lisp to avoid circular dependencies in testing
(defun approx= (a b &optional (epsilon 0.00001))
  "Compare two floating point numbers for approximate equality."
  (<= (abs (- a b)) epsilon))

;;;; NOTE: The test files are loaded automatically by ASDF through the system definition.
;;;; We don't need to manually load them here.

;; Function to run all tests in the system
(defun run-tests ()
  ;; Run all test suites
  (let ((results (run! 'vorm-tests)))
    ;; Print a summary of results
    (format t "~&~%=== Test Summary ===")
    (format t "~&Running all vorm test suites...")
    (format t "~&- Core arithmetic tests")
    (format t "~&- Utility math tests")
    (format t "~&====================~%")
    results))

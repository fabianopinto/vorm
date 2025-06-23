(in-package :vorm.tests)

;;;; Main test orchestration for vorm project

;; Define the main test suite for the entire system
(def-suite vorm-tests
  :description "Tests for the vorm system")

;; Helper function for comparing floating point values with a tolerance
(defun approximately-equal (a b &optional (epsilon 1.0e-6))
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
    (format t "~&- Tolerance tests")
    (format t "~&- Shape tests")
    (format t "~&- Grammar tests")
    (format t "~&- Transformation tests")
    (format t "~&====================~%")
    results))

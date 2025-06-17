(in-package :vorm.tests)

;; Tests for the math utility functions
(def-suite utils-math-tests
  :description "Tests for math utility functions"
  :in vorm-tests)

(in-suite utils-math-tests)

;; Tests for the approx= function
(test test-approx=
  "Test the approximate equality function for floating point numbers."
  (is (approx= 0.1 0.1))
  (is (approx= 0.1 0.10000001))
  (is (approx= 0.0 0.0))
  (is (approx= 1.0 1.0))
  (is (not (approx= 0.1 0.2)))
  (is (not (approx= 1.0 1.1)))
  ;; Test with custom epsilon
  (is (approx= 1.0 1.05 0.1))
  (is (not (approx= 1.0 1.05 0.01))))

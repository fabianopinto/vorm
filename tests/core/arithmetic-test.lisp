(in-package :vorm.tests)

;; Tests for the core arithmetic operations
(def-suite core-arithmetic-tests
  :description "Tests for the core arithmetic operations"
  :in vorm-tests)

(in-suite core-arithmetic-tests)

;; Tests for the add function
(test test-add
  "Test the add function with various inputs."
  (is (= 5 (add 2 3)))
  (is (= 0 (add 0 0)))
  (is (= -1 (add -2 1)))
  (is (= 0.5 (add 0.3 0.2))))

;; Tests for the subtract function
(test test-subtract
  "Test the subtract function with various inputs."
  (is (= 1 (subtract 3 2)))
  (is (= 0 (subtract 0 0)))
  (is (= -3 (subtract -2 1)))
  ;; Use approximate equality for floating point values
  (is (approx= 0.1 (subtract 0.3 0.2))))

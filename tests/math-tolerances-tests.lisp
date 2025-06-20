;;;; math-tolerances-tests.lisp - Tests for mathematical tolerance functions in the VORM system

(in-package :vorm/tests)

;; Ensure we're using the VORM-TESTS suite
(in-suite :vorm-tests)

;; Tests for linear comparison
(test linear-equal-test
  "Test the linear-equal function"
  ;; Equal values
  (is (linear-equal 1.0 1.0))
  (is (linear-equal 0.0 0.0))
  (is (linear-equal -5.0 -5.0))
  
  ;; Values within tolerance
  (is (linear-equal 1.0 (+ 1.0 (* 0.5 *linear-tolerance*))))
  (is (linear-equal 1.0 (- 1.0 (* 0.5 *linear-tolerance*))))
  
  ;; Values outside tolerance
  (is-false (linear-equal 1.0 (+ 1.0 (* 2.0 *linear-tolerance*))))
  (is-false (linear-equal 1.0 (- 1.0 (* 2.0 *linear-tolerance*)))))

;; Tests for angular comparison
(test angular-equal-test
  "Test the angular-equal function"
  ;; Equal angles
  (is (angular-equal 0.0 0.0))
  (is (angular-equal pi pi))
  (is (angular-equal (* 2 pi) (* 2 pi)))
  
  ;; Angles within tolerance
  (is (angular-equal pi (+ pi (* 0.5 *angular-tolerance*))))
  (is (angular-equal pi (- pi (* 0.5 *angular-tolerance*))))
  
  ;; Equivalent angles across 2π boundaries (now should pass with normalization)
  (is (angular-equal 0.0 (* 2 pi)))
  (is (angular-equal (* -1 pi) pi))
  (is (angular-equal (* -0.1 pi) (* 1.9 pi)))
  
  ;; Angles outside tolerance even after normalization
  (is-false (angular-equal 0.0 (* 2.0 *angular-tolerance*))))

;; Tests for normalize-angle
(test normalize-angle-test
  "Test the normalize-angle function"
  ;; Values in range should remain the same
  (is (= (normalize-angle 0.0) 0.0))
  (is (= (normalize-angle pi) pi))
  (is (< (abs (- (normalize-angle (* 1.5 pi)) (* 1.5 pi))) *angular-tolerance*))
  
  ;; Values outside range should be normalized
  (is (< (abs (- (normalize-angle (* 2 pi)) 0.0)) *angular-tolerance*))
  (is (< (abs (- (normalize-angle (* 3 pi)) pi)) *angular-tolerance*))
  (is (< (abs (- (normalize-angle (* -1 pi)) pi)) *angular-tolerance*))
  (is (< (abs (- (normalize-angle (* -0.5 pi)) (* 1.5 pi))) *angular-tolerance*)))

;; Property-based tests for angle normalization
(test normalize-angle-properties
  "Test mathematical properties of angle normalization"
  
  ;; Property: Normalization is idempotent (applying it twice is same as once)
  (let ((test-angles (list 0.0 pi (* 2 pi) (* -1 pi) (* 10 pi) (* -7.5 pi))))
    (dolist (angle test-angles)
      (let ((normalized (normalize-angle angle)))
        (is (angular-equal normalized (normalize-angle normalized))
            "Normalization should be idempotent: angle ~a" angle))))
  
  ;; Property: Normalized angles should be in [0, 2π)
  (let ((test-angles (list 0.0 pi (* 2 pi) (* -1 pi) (* 10 pi) (* -7.5 pi))))
    (dolist (angle test-angles)
      (let ((normalized (normalize-angle angle)))
        (is (and (>= normalized 0.0) (< normalized +two-pi+))
            "Normalized angle should be in range [0, 2π): ~a" normalized))))
  
  ;; Property: Adding 2π to the input shouldn't change the normalized result
  (let ((test-angles (list 0.0 pi (* 2 pi) (* -1 pi) (* 10 pi) (* -7.5 pi))))
    (dolist (angle test-angles)
      (is (angular-equal (normalize-angle angle) 
                          (normalize-angle (+ angle +two-pi+)))
          "Adding 2π shouldn't change normalized result: ~a" angle))))

;; Edge case tests for angle utilities
(test angle-edge-cases
  "Test angle utilities with extreme cases"
  
  ;; Very large positive angle
  (let ((large-angle (* 1000000 +two-pi+)))
    (is (< (abs (- (normalize-angle large-angle) 0.0)) *angular-tolerance*)
        "Very large angle should normalize correctly"))
  
  ;; Very large negative angle
  (let ((large-negative-angle (* -1000000 +two-pi+)))
    (is (< (abs (- (normalize-angle large-negative-angle) 0.0)) *angular-tolerance*)
        "Very large negative angle should normalize correctly"))
  
  ;; Nearly equal angles but outside tolerance
  (let ((beyond-tolerance (* 2 *angular-tolerance*)))
    (is-false (angular-equal 0.0 beyond-tolerance)
              "Angles outside tolerance should not be equal")))

;; Test for the with-custom-tolerance macro
(test custom-tolerance-test
  "Test the with-custom-tolerance macro"
  
  ;; Test values that are not equal with default tolerance
  (let ((a 0.0)
        (b 0.000002))
    (is-false (linear-equal a b)
              "Values should not be equal with default tolerance")
    
    ;; Test with increased tolerance - fix syntax for macro call
    (with-custom-tolerance (0.00001 0.00001)
      (is (linear-equal a b)
          "Values should be equal with increased tolerance"))
    
    ;; Test that tolerance reverts to original value
    (is-false (linear-equal a b)
              "Tolerance should be restored after with-custom-tolerance")))

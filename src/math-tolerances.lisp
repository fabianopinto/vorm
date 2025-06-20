;;;; math-tolerances.lisp - Mathematical tolerance functions for the VORM system

(in-package :vorm)

;; Constants for comparison tolerance
(defparameter *linear-tolerance* 1.0e-6
  "Tolerance used for linear value comparisons. Two values are considered
  equal if they differ by less than this amount.")

(defparameter *angular-tolerance* 1.0e-6
  "Tolerance used for angular value comparisons. Two angles are considered
  equal if they differ by less than this amount.")

;; Constant for 2*pi to avoid repeated calculations
(defconstant +two-pi+ (* 2 pi)
  "The value of 2π, used for angle normalization and comparison.")

;; Comparison functions
(defun linear-equal (a b)
  "Compare two linear values with tolerance.
   Returns T if the absolute difference is less than *LINEAR-TOLERANCE*."
  (<= (abs (- a b)) *linear-tolerance*))

;; Utility function to normalize an angle to [0, 2π)
(defun normalize-angle (angle)
  "Normalize an angle to the range [0, 2π) using modular arithmetic.
   Optimized version using pre-calculated +TWO-PI+ constant."
  (let ((mod-angle (rem angle +two-pi+)))
    (if (minusp mod-angle)
        (+ mod-angle +two-pi+)
        mod-angle)))

;; Angular comparison that incorporates normalization
(defun angular-equal (angle1 angle2)
  "Compare two angular values with tolerance, after normalizing them to [0, 2π).
   Returns T if the normalized angles differ by less than *ANGULAR-TOLERANCE*
   or if they differ by approximately 2π."
  (let* ((norm-angle1 (normalize-angle angle1))
         (norm-angle2 (normalize-angle angle2))
         (diff (abs (- norm-angle1 norm-angle2))))
    ;; Check if the difference is within tolerance or approximately 2π
    (or (<= diff *angular-tolerance*)
        (<= (abs (- diff +two-pi+)) *angular-tolerance*))))

;; Configuration macro for custom tolerance settings
(defmacro with-custom-tolerance ((linear-tol angular-tol) &body body)
  "Execute body with temporarily adjusted tolerance values.
   
   Example usage:
   (with-custom-tolerance (1.0e-4 1.0e-4)
     (linear-equal 0.0001 0.0002))  ; => T with the custom tolerance"
  `(let ((*linear-tolerance* ,linear-tol)
         (*angular-tolerance* ,angular-tol))
     ,@body))

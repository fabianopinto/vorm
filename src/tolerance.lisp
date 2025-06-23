(in-package :vorm)

;;;; Tolerance and angle normalization utilities for the VORM shape grammar system

;;; Tolerance parameters and constants
(defparameter *linear-tolerance* 1.0e-6
  "Tolerance used for linear value comparisons. Two values are considered
  equal if they differ by less than this amount.")

(defparameter *angular-tolerance* 1.0e-6
  "Tolerance used for angular value comparisons. Two angles are considered
  equal if they differ by less than this amount.")

(defconstant +two-pi+ (* 2 pi)
  "The value of 2π, used for angle normalization and comparison.")

;;; Linear comparison functions
(defun linear-equal (a b)
  "Compare two linear values with tolerance.
   
   Parameters:
     A - First value to compare
     B - Second value to compare
   
   Returns:
     T if the absolute difference is less than *LINEAR-TOLERANCE*, NIL otherwise
   
   Example:
     (linear-equal 1.0 1.0000001) ; => T
     (linear-equal 1.0 1.01) ; => NIL
   
   See also:
     *LINEAR-TOLERANCE* - The tolerance value used for comparison"
  (<= (abs (- a b)) *linear-tolerance*))

;;; Angle normalization and comparison
(defun normalize-angle (angle)
  "Normalize an angle to the range [0, 2π) using modular arithmetic.
   
   Parameters:
     ANGLE - Any angle value in radians
   
   Returns:
     Equivalent angle normalized to the range [0, 2π)
   
   Example:
     (normalize-angle (* 2.5 pi)) ; => (approximately 0.5π)
     (normalize-angle (* -0.5 pi)) ; => (approximately 1.5π)
   
   See also:
     +TWO-PI+ - Constant used in calculations
     ANGULAR-EQUAL - For comparing angles with tolerance"
  (let ((mod-angle (rem angle +two-pi+)))
    (if (minusp mod-angle)
        (+ mod-angle +two-pi+)
        mod-angle)))

(defun angular-equal (angle1 angle2)
  "Compare two angular values with tolerance, after normalizing them to [0, 2π).
   
   Parameters:
     ANGLE1 - First angle to compare (in radians)
     ANGLE2 - Second angle to compare (in radians)
   
   Returns:
     T if the normalized angles differ by less than *ANGULAR-TOLERANCE*
     or if they differ by approximately 2π, NIL otherwise
   
   Example:
     (angular-equal 0.0 (* 2 pi)) ; => T (equivalent angles)
     (angular-equal pi (* -1 pi)) ; => T (equivalent angles after normalization)
   
   See also:
     NORMALIZE-ANGLE - Used to normalize angles before comparison
     *ANGULAR-TOLERANCE* - The tolerance used for comparison"
  (let* ((norm-angle1 (normalize-angle angle1))
         (norm-angle2 (normalize-angle angle2))
         (diff (abs (- norm-angle1 norm-angle2))))
    ;; Check if the difference is within tolerance or approximately 2π
    (or (<= diff *angular-tolerance*)
        (<= (abs (- diff +two-pi+)) *angular-tolerance*))))

;;; Tolerance configuration
(defmacro with-custom-tolerance ((linear-tol angular-tol) &body body)
  "Execute body with temporarily adjusted tolerance values.
   
   Parameters:
     LINEAR-TOL - Temporary linear tolerance value to use
     ANGULAR-TOL - Temporary angular tolerance value to use
     BODY - Forms to execute with the adjusted tolerance values
   
   Returns:
     The result of the last form in BODY
   
   Example:
     (with-custom-tolerance (1.0e-4 1.0e-4)
       (linear-equal 0.0001 0.0002))  ; => T with the custom tolerance
   
   See also:
     *LINEAR-TOLERANCE* - Global linear tolerance parameter
     *ANGULAR-TOLERANCE* - Global angular tolerance parameter"
  `(let ((*linear-tolerance* ,linear-tol)
         (*angular-tolerance* ,angular-tol))
      ,@body))

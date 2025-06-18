(in-package :vorm)

;;;; Utility functions for the VORM shape grammar system

(defun ensure-list (x)
  "If X is not a list, wrap it in a list.
   
   Parameters:
     X - Any Lisp object
   
   Returns:
     X if X is already a list, otherwise (list X)
   
   Example:
     (ensure-list 5) ; => (5)
     (ensure-list '(1 2 3)) ; => (1 2 3)"
  (if (listp x) x (list x)))

(defmacro with-gensyms ((&rest names) &body body)
  "Binds each variable in NAMES to a unique symbol and evaluates BODY."
  `(let ,(loop for name in names
               collect `(,name (gensym ,(string name))))
     ,@body))

(defun almost-equal (a b &optional (epsilon 1.0e-6))
  "Check if two floating point numbers are almost equal within EPSILON.
   
   Parameters:
     A - First number
     B - Second number
     EPSILON - Optional tolerance (default: 1.0e-6)
   
   Returns:
     T if the absolute difference between A and B is less than EPSILON, NIL otherwise
   
   Example:
     (almost-equal 0.1 0.10000001) ; => T
     (almost-equal 1.0 1.1) ; => NIL"
  (< (abs (- a b)) epsilon))

(defun point-distance (x1 y1 x2 y2)
  "Calculate the Euclidean distance between two points.
   
   Parameters:
     X1 - X coordinate of the first point
     Y1 - Y coordinate of the first point
     X2 - X coordinate of the second point
     Y2 - Y coordinate of the second point
   
   Returns:
     The Euclidean distance between points (X1,Y1) and (X2,Y2)
   
   Example:
     (point-distance 0 0 3 4) ; => 5.0 (distance from origin to point (3,4))"
  (sqrt (+ (expt (- x2 x1) 2)
           (expt (- y2 y1) 2))))

(defun degrees-to-radians (degrees)
  "Convert degrees to radians.
   
   Parameters:
     DEGREES - Angle in degrees
   
   Returns:
     Equivalent angle in radians
   
   Example:
     (degrees-to-radians 180) ; => 3.141592... (approximately π)"
  (* degrees (/ pi 180.0)))

(defun radians-to-degrees (radians)
  "Convert radians to degrees.
   
   Parameters:
     RADIANS - Angle in radians
   
   Returns:
     Equivalent angle in degrees
   
   Example:
     (radians-to-degrees pi) ; => 180.0"
  (* radians (/ 180.0 pi)))

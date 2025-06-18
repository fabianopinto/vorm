(in-package :vorm)

;;;; Utility functions for the VORM shape grammar system

(defun ensure-list (x)
  "If X is not a list, wrap it in a list."
  (if (listp x) x (list x)))

(defmacro with-gensyms ((&rest names) &body body)
  "Binds each variable in NAMES to a unique symbol and evaluates BODY."
  `(let ,(loop for name in names
               collect `(,name (gensym ,(string name))))
     ,@body))

(defun almost-equal (a b &optional (epsilon 1.0e-6))
  "Check if two floating point numbers are almost equal within EPSILON."
  (< (abs (- a b)) epsilon))

(defun point-distance (x1 y1 x2 y2)
  "Calculate the Euclidean distance between two points."
  (sqrt (+ (expt (- x2 x1) 2)
           (expt (- y2 y1) 2))))

(defun degrees-to-radians (degrees)
  "Convert degrees to radians."
  (* degrees (/ pi 180.0)))

(defun radians-to-degrees (radians)
  "Convert radians to degrees."
  (* radians (/ 180.0 pi)))

(in-package :vorm)

;;; Math utility functions

(defun approx= (a b &optional (epsilon 0.00001))
  "Compare two floating point numbers for approximate equality."
  (<= (abs (- a b)) epsilon))

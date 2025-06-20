;;;; package.lisp - Package definition for VORM

(defpackage :vorm
  (:documentation "Main package for the VORM system.")
  (:use :cl :alexandria) ;; Using Alexandria for common utilities
  (:export 
   ;; Math tolerance constants
   #:*linear-tolerance*
   #:*angular-tolerance*
   #:+two-pi+
   
   ;; Math tolerance functions
   #:linear-equal
   #:normalize-angle
   #:angular-equal
   #:with-custom-tolerance))

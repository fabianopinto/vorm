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
   #:with-custom-tolerance
   
   ;; Segment structure and accessors
   #:segment
   #:segment-lower
   #:segment-upper
   #:make-segment
   
   ;; Segment operations
   #:segments-overlap-p
   #:segments-merge
   
   ;; Line structure and accessors
   #:line
   #:line-segments
   #:make-line
   
   ;; Line operations
   #:line-add-segments))

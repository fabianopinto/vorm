;;;; package.lisp - Test package definition for VORM

(defpackage :vorm/tests
  (:documentation "Test package for the VORM system.")
  (:use :cl :fiveam :vorm)
  (:export #:run-tests))

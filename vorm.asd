;;;; vorm.asd - System definition for VORM

(defpackage :vorm-system
  (:documentation "ASDF System definition for the VORM system")
  (:use :cl :asdf))

(in-package :vorm-system)

(defsystem :vorm
  :name "vorm"
  :version "0.2.0"
  :description "Common Lisp library for shape structures optimized for pattern recognition."
  :author "Fabiano Pinto <fabiano.pinto@gmail.com>"
  :license "MIT"
  :depends-on (:alexandria)  ;; Using Alexandria for common utilities
  :serial nil
  :components ((:module "src"
                :serial nil
                :components ((:file "package")
                             (:file "math-tolerances" :depends-on ("package"))
                             (:file "main" :depends-on ("package" "math-tolerances")))))
  :in-order-to ((test-op (test-op :vorm/tests)))
  :perform (load-op :after (op c)
                    (pushnew :vorm *features*)))

(defsystem :vorm/tests
  :description "Test system for the VORM system"
  :depends-on (:vorm :fiveam)
  :serial nil
  :components ((:module "tests"
                :serial nil
                :components ((:file "package")
                             (:file "main" :depends-on ("package"))
                             (:file "math-tolerances-tests" :depends-on ("package" "main")))))
  :perform (test-op (o c) (funcall (intern "RUN!" :fiveam) :vorm-tests)))

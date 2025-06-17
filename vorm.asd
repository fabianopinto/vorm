;;; System definition for vorm

(require :asdf)

(asdf:defsystem "vorm"
  :version "0.1.0"
  :author "Fabiano Pinto"
  :license "MIT"
  :description "vorm: a Common Lisp project"
  :homepage "https://github.com/fabianopinto/vorm"
  :bug-tracker "https://github.com/fabianopinto/vorm/issues"
  :source-control (:git "https://github.com/fabianopinto/vorm.git")
  :depends-on ()
  :components ((:file "src/package")
               (:file "src/math"
                :depends-on ("src/package"))
               (:file "src/arithmetic"
                :depends-on ("src/package" "src/math"))
               (:file "src/main" 
                :depends-on ("src/package" "src/arithmetic")))
  :in-order-to ((asdf:test-op (asdf:test-op "vorm/test"))))

(asdf:defsystem "vorm/test"
  :author "Fabiano Pinto"
  :license "MIT"
  :depends-on ("vorm"
               "fiveam")
  :pathname "tests"
  :components ((:file "package")
               (:file "main" :depends-on ("package"))
               (:file "arithmetic-test" :depends-on ("package" "main"))
               (:file "math-test" :depends-on ("package" "main")))
  :description "Test system for vorm"
  :perform (asdf:test-op (op c) (uiop:symbol-call :vorm.tests :run-tests)))

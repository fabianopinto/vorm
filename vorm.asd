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
  :pathname "src"
  :components ((:file "package")
               (:file "utils"
                :depends-on ("package"))
               (:file "shapes"
                :depends-on ("package" "utils"))
               (:file "transformations"
                :depends-on ("package" "utils" "shapes"))
               (:file "grammar"
                :depends-on ("package" "utils" "shapes" "transformations"))
               (:file "main" 
                :depends-on ("package" "utils" "shapes" "transformations" "grammar")))
  :in-order-to ((asdf:test-op (asdf:test-op "vorm/test"))))

(asdf:defsystem "vorm/test"
  :author "Fabiano Pinto"
  :license "MIT"
  :description "Test system for vorm"
  :depends-on ("vorm"
               "fiveam")
  :pathname "tests"
  :components ((:file "package")
               (:file "main" :depends-on ("package"))
               (:file "shapes-tests" :depends-on ("package" "main"))
               (:file "transformations-tests" :depends-on ("package" "main"))
               (:file "grammar-tests" :depends-on ("package" "main")))
  :perform (asdf:test-op (op c) (uiop:symbol-call :vorm.tests :run-tests)))

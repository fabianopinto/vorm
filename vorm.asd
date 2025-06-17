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
               (:module "src/utils"
                :depends-on ("src/package")
                :components
                ((:file "math")))
               (:module "src/core"
                :depends-on ("src/package" "src/utils")
                :components
                ((:file "arithmetic")))
               (:file "src/main" :depends-on ("src/package" "src/core")))
  :in-order-to ((asdf:test-op (asdf:test-op "vorm/test"))))

(asdf:defsystem "vorm/test"
  :author "Fabiano Pinto"
  :license "MIT"
  :depends-on ("vorm"
               "fiveam")
  :pathname "tests"
  :components ((:file "package")
               (:file "main" :depends-on ("package"))
               (:module "core"
                :depends-on ("package" "main")
                :components
                ((:file "arithmetic-test")))
               (:module "utils"
                :depends-on ("package" "main")
                :components
                ((:file "math-test"))))
  :description "Test system for vorm"
  :perform (asdf:test-op (op c) (uiop:symbol-call :vorm.tests :run-tests)))

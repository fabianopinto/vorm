(asdf:defsystem :vorm-examples
  :description "Examples for the VORM shape grammar system"
  :author "VORM Development Team"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:vorm)
  :components ((:file "package")
               (:file "koch-curve" :depends-on ("package"))
               (:file "sierpinski-triangle" :depends-on ("package"))
               (:file "square-subdivision" :depends-on ("package"))
               (:file "l-system-plants" :depends-on ("package")))
  :in-order-to ((test-op (test-op :vorm-examples/test))))

(asdf:defsystem :vorm-examples/test
  :depends-on (:vorm-examples :fiveam)
  :components ((:file "test-package"))
  :perform (test-op (o c) (uiop:symbol-call :vorm.examples.test :run-tests)))

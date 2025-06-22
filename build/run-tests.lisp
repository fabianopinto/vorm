;;;; run-tests.lisp - Script to run the VORM tests

(require :asdf)

(pushnew (truename ".") asdf:*central-registry* :test #'equal)

;; Load the test system
(asdf:load-system :vorm/tests :force t)

;; Run the tests
(format t "~%Running VORM tests...~%~%")
(funcall (intern "RUN-TESTS" :vorm/tests))

;;;; load-system.lisp - Script to load the VORM system

(require :asdf)

(pushnew (truename ".") asdf:*central-registry* :test #'equal)

(asdf:load-system :vorm :force t)

(format t "~%~%System VORM loaded successfully!~%")
(format t "You can now use the VORM package.~%~%")

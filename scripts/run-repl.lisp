;; Script to start a REPL with the vorm system loaded
;; Get the directory of this script and the project root
(defparameter *this-file* *load-pathname*)
(defparameter *this-directory* (directory-namestring *this-file*))
(defparameter *project-root* (directory-namestring 
                             (truename (merge-pathnames "../" *this-directory*))))

(format t "~&Script directory: ~a~%" *this-directory*)
(format t "~&Project root: ~a~%" *project-root*)

;; Add the project root directory to ASDF's search paths
(push *project-root* asdf:*central-registry*)
(format t "~&Added ~a to ASDF's central registry~%" *project-root*)

;; Try to compile and load vorm.asd
(handler-case 
    (progn
      (format t "~&Loading vorm system definition...~%")
      (load (merge-pathnames "vorm.asd" *project-root*))
      (format t "~&Successfully loaded system definition~%"))
  (error (e)
    (format t "~&Error loading system definition: ~a~%" e)))

;; Load the system
(handler-case 
    (progn
      (format t "~&Loading vorm system...~%")
      (asdf:load-system :vorm)
      (format t "~&Successfully loaded vorm system~%"))
  (error (e)
    (format t "~&Error loading vorm system: ~a~%" e)
    (uiop:quit 1)))

;; Print some instructions to the user
(format t "~&~%~%")
(format t "~&=================================~%")
(format t "~&Welcome to the VORM REPL~%")
(format t "~&=================================~%")
(format t "~&You need to switch to the vorm package first:~%")
(format t "~&  (in-package :vorm)~%")
(format t "~&~%")
(format t "~&Then you can use functions like:~%")
(format t "~&  (add 10 20)~%")
(format t "~&  (subtract 50 30)~%")
(format t "~&=================================~%~%")

;; This will be evaluated when the REPL starts, putting the user in the vorm package
(eval-when (:execute)
  (in-package :vorm))

;; Start user in vorm package automatically
(setf *package* (find-package :vorm))

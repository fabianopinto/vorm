;; Script to build a standalone executable for vorm

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
    (format t "~&Error loading system definition: ~a~%" e)
    (uiop:quit 1)))

;; Load the system
(handler-case 
    (progn
      (format t "~&Loading vorm system...~%")
      (asdf:load-system :vorm)
      (format t "~&Successfully loaded vorm system~%"))
  (error (e)
    (format t "~&Error loading vorm system: ~a~%" e)
    (uiop:quit 1)))

;; Save the executable
(handler-case
    (progn
      (format t "~&Building standalone executable...~%")
      ;; Determine the output path
      (let ((output-path (merge-pathnames "vorm" *project-root*)))
        (sb-ext:save-lisp-and-die
         output-path
         :toplevel #'vorm:main
         :executable t
         :compression t)
        (format t "~&Executable saved to: ~a~%" output-path)))
  (error (e)
    (format t "~&Error building executable: ~a~%" e)
    (uiop:quit 1)))

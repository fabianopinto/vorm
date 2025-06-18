;;;; VORM REPL Initialization Script
;;;;
;;;; This script initializes a Common Lisp REPL with the VORM system loaded
;;;; and provides helpful usage examples for the user. It handles loading both
;;;; the main system and example packages, displaying informative messages
;;;; throughout the process.
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

;; Add the examples directory to ASDF's search paths
(let ((examples-dir (merge-pathnames "examples/" *project-root*)))
  (push examples-dir asdf:*central-registry*)
  (format t "~&Added ~a to ASDF's central registry~%" examples-dir))

;; Try to compile and load vorm.asd
(handler-case 
    (progn
      (format t "~&Loading vorm system definition...~%")
      (load (merge-pathnames "vorm.asd" *project-root*))
      (format t "~&Successfully loaded system definition~%"))
  (error (e)
    (format t "~&Error loading system definition: ~a~%" e)))

;; Load the vorm system
(handler-case 
    (progn
      (format t "~&Loading vorm system...~%")
      (asdf:load-system :vorm)
      (format t "~&Successfully loaded vorm system~%"))
  (error (e)
    (format t "~&Error loading vorm system: ~a~%" e)
    (uiop:quit 1)))

;; Load the examples package
(handler-case 
    (progn
      (format t "~&Loading vorm examples package...~%")
      (asdf:load-system :vorm-examples)
      (format t "~&Successfully loaded vorm examples~%")
      ;; Ensure the package is available
      (unless (find-package :vorm.examples)
        (error "Package VORM.EXAMPLES was not created during system load")))
  (error (e)
    (format t "~&Warning: Could not load vorm examples: ~a~%" e)
    (format t "~&Make sure the examples directory is properly set up~%")))

;; Print some instructions to the user
(format t "~&~%~%")
(format t "~&=================================~%")
(format t "~&Welcome to the VORM REPL~%")
(format t "~&=================================~%")
(format t "~&Available packages:~%")
(format t "~&  (in-package :vorm)          ;; Main package~%")
(format t "~&  (in-package :vorm.examples) ;; Examples package~%")
(format t "~&~%")
(format t "~&Basic Shape Creation:~%")
(format t "~&  (make-point 10 20)~%")
(format t "~&  (make-line 10 20 30 40)~%")
(format t "~&  (make-circle 10 20 5)~%")
(format t "~&  (make-polygon (list (make-point 10 20) (make-point 30 40) (make-point 50 60)))~%")
(format t "~&  (make-rectangle 10 20 5 10)~%")
(format t "~&~%")
(format t "~&Working with Grammars:~%")
(format t "~&  ;; Parse a grammar from s-expression~%")
(format t "~&  (defvar *my-grammar* (parse-grammar~%")
(format t "~&    '(grammar \"my-grammar\"~%")
(format t "~&       (circle (50 50) 20)~%")
(format t "~&       :rules ((rule (circle (50 50) 20)~%")
(format t "~&                   (polygon (30 30) (70 30) (50 70))~%")
(format t "~&                   :label \"circle-to-triangle\")))))~%")
(format t "~&~%")
(format t "~&  ;; Apply a grammar for N iterations~%")
(format t "~&  ;; With a parsed grammar object:~%")
(format t "~&  (generate-shapes *my-grammar* 3)~%")
(format t "~&~%")
(format t "~&  ;; Or directly from s-expression:~%")
(format t "~&  (interpret-grammar~%")
(format t "~&    '(grammar \"my-grammar\"~%")
(format t "~&       (circle (50 50) 20)~%")
(format t "~&       :rules ((rule (circle (50 50) 20)~%")
(format t "~&                   (polygon (30 30) (70 30) (50 70))~%")
(format t "~&                   :label \"circle-to-triangle\")))~%")
(format t "~&    3)~%")
(format t "~&~%")
(format t "~&  ;; Trace grammar execution step-by-step~%")
(format t "~&  (trace-grammar-execution *my-grammar* 3)~%")
(format t "~&~%")
(format t "~&Example Grammars:~%")
(format t "~&  ;; Load pre-defined examples (requires vorm-examples package)~%")
(format t "~&  (vorm.examples:load-koch-curve 4)           ;; Koch curve with 4 iterations~%")
(format t "~&  (vorm.examples:load-koch-snowflake 4)       ;; Koch snowflake fractal~%")
(format t "~&  (vorm.examples:load-sierpinski-triangle 5)   ;; Sierpinski triangle fractal~%")
(format t "~&  (vorm.examples:load-mondrian-pattern 5)     ;; Mondrian style rectangles~%")
(format t "~&  (vorm.examples:load-architectural-pattern 4) ;; Architectural subdivision~%")
(format t "~&  (vorm.examples:load-plant-structure)        ;; L-system plant~%")
(format t "~&  (vorm.examples:load-fern-structure)         ;; L-system fern~%")
(format t "~&  (vorm.examples:load-tree-structure)         ;; L-system tree~%")
(format t "~&=================================~%~%")

;; This will be evaluated when the REPL starts, putting the user in the vorm package
(eval-when (:execute)
  (in-package :vorm))

;; Start user in vorm package automatically
(setf *package* (find-package :vorm))

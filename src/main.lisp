(in-package :vorm)

;;;; Main Entry Module
;;;; 
;;;; This file implements the command-line interface and executable entry point
;;;; for the VORM system. It provides a simple CLI for interacting with the
;;;; shape grammar functionality.
(defun main ()
  "Main entry point for the vorm executable.
   
   Parameters:
     None (command-line arguments are accessed via uiop:command-line-arguments)
   
   Returns:
     Exit code (0 for success)
   
   This function processes command-line arguments and provides a simple
   CLI interface to the vorm system. Available commands include:
   - vorm shapes - Show available shape operations
   - vorm help - Show help information"
  (let ((args (uiop:command-line-arguments)))
    (format t "~&Welcome to vorm binary!~%")
    (format t "~&Version: ~a~%" (asdf:component-version (asdf:find-system :vorm)))
    
    ;; If arguments are provided, try to process them
    (when args
      (format t "~&Command line arguments:~%")
      (loop for arg in args
            for i from 1
            do (format t "~&  ~a: ~a~%" i arg))
      
      ;; Display information about the shape system capabilities
      (when (>= (length args) 1)
        (let* ((op (first args)))
          (cond ((string-equal op "shapes")
                 (format t "~&Available shape operations:~%")
                 (format t "~&  - Create points, lines, polygons, and circles~%")
                 (format t "~&  - Apply transformations (translation, rotation, scaling)~%")
                 (format t "~&  - Define and apply shape grammars~%"))
                ((string-equal op "help")
                 (format t "~&VORM is a shape grammar system for Common Lisp~%")
                 (format t "~&It allows you to create and transform shapes using grammar rules~%"))))))
    
    ;; If no valid operation, show usage
    (format t "~&~%Usage:~%")
    (format t "~&  vorm shapes       - Show available shape operations~%")
    (format t "~&  vorm help         - Show help information~%"))
  
  ;; Return success
  0)

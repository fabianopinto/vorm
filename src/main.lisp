(in-package :vorm)

;; Main entry function for the executable
(defun main ()
  "Main entry point for the vorm executable."
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

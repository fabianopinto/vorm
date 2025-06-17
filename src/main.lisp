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
      
      ;; Simple example of processing arguments
      (when (>= (length args) 2)
        (let* ((op (first args))
               (a (parse-integer (second args) :junk-allowed t))
               (b (when (>= (length args) 3)
                    (parse-integer (third args) :junk-allowed t))))
          (cond ((and a b (string-equal op "add"))
                 (format t "~&Result of add: ~a~%" (add a b)))
                ((and a b (string-equal op "subtract")) 
                 (format t "~&Result of subtract: ~a~%" (subtract a b)))))))
    
    ;; If no valid operation, show usage
    (format t "~&~%Usage:~%")
    (format t "~&  vorm add NUMBER1 NUMBER2       - Add two numbers~%")
    (format t "~&  vorm subtract NUMBER1 NUMBER2  - Subtract NUMBER2 from NUMBER1~%")))

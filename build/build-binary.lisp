;;;; build-binary.lisp - Script to build a standalone executable for the VORM system

;; Load ASDF and ensure our system is in the registry
(require :asdf)
(pushnew (truename ".") asdf:*central-registry* :test #'equal)

;; Load the VORM system
(asdf:load-system :vorm)

;; Entry point function that processes command-line arguments
(defun main ()
  ;; Get command line arguments, removing the executable name
  (let ((args (rest sb-ext:*posix-argv*)))
    (if args
        ;; The generate-shape-cli function expects a single list argument, not multiple args
        (vorm:generate-shape-cli args)
        (progn
          (format t "~%VORM Random Shape Generator~%")
          (format t "Usage: vorm-shape-gen OUTPUT_FILE [OPTIONS]~%~%")
          (format t "Options:~%")
          (format t "  --parallel-count N      Number of parallels at different angles~%")
          (format t "  --line-count N          Number of lines per parallel~%")
          (format t "  --segment-count N       Number of segments per line~%")
          (format t "  --lower-limit X         Lower coordinate boundary~%")
          (format t "  --upper-limit X         Upper coordinate boundary~%")
          (format t "  --position-min-spacing X Minimum spacing between line positions~%")
          (format t "  --segment-min-spacing X Minimum segment size and spacing~%")
          (format t "  --width N               Width of the output SVG~%")
          (format t "  --height N              Height of the output SVG~%")
          (format t "  --stroke COLOR          Line color (e.g., \"blue\", \"#ff0000\")~%")
          (format t "  --stroke-width N        Line thickness~%~%")
          (format t "Example:~%")
          (format t "  vorm-shape-gen output.svg --parallel-count 7 --stroke \"blue\"~%~%")
          1)))) ;; Return non-zero exit code for error

;; Save executable
(sb-ext:save-lisp-and-die 
 "vorm-shape-gen" 
 :executable t 
 :toplevel #'main
 :compression t
 :purify t)

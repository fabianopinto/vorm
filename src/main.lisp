(in-package :vorm)

;;;-----------------------------------------------------------------------------
;;; Random Shape Generation and SVG Output
;;;-----------------------------------------------------------------------------

(defun create-random-shape-svg (filename &key 
                                  (parallel-count 5)
                                  (line-count 10) 
                                  (lower-limit 0.0)
                                  (upper-limit 1000.0)
                                  (position-min-spacing 20.0)
                                  (segment-count 5)
                                  (segment-min-spacing 20.0)
                                  (width 800)
                                  (height 800)
                                  (stroke "black")
                                  (stroke-width 1))
  "Create a random shape and save it as an SVG file.
   
   Parameters:
   - filename: The name of the output SVG file
   - parallel-count: Number of parallels at different angles (default: 5)
   - line-count: Number of lines per parallel (default: 10)
   - lower-limit: Lower coordinate boundary (default: 0.0)
   - upper-limit: Upper coordinate boundary (default: 1000.0)
   - position-min-spacing: Minimum spacing between line positions (default: 20.0)
   - segment-count: Number of segments per line (default: 5)
   - segment-min-spacing: Minimum segment size and spacing (default: 20.0)
   - width: Width of the output SVG (default: 800)
   - height: Height of the output SVG (default: 800)
   - stroke: Line color (default: \"black\")
   - stroke-width: Line thickness (default: 1)
   
   Returns:
   - The filename on success"
  
  ;; Generate a random shape with the specified parameters
  (let ((shape (generate-random-shape :parallel-count parallel-count
                                     :line-count line-count
                                     :lower-limit lower-limit
                                     :upper-limit upper-limit
                                     :position-min-spacing position-min-spacing
                                     :segment-count segment-count
                                     :segment-min-spacing segment-min-spacing)))
    
    ;; Render the shape to an SVG file and return filename on success
    (render-shape-to-file shape filename
                         :width width
                         :height height
                         :auto-viewbox t
                         :stroke stroke
                         :stroke-width stroke-width)))

;;;-----------------------------------------------------------------------------
;;; Command Line Interface
;;;-----------------------------------------------------------------------------

(defun generate-shape-cli (args)
  "Command line interface for generating random shapes.
   
   Parameters:
   - args: List of command-line arguments
   
   Usage examples:
   - (generate-shape-cli (list \"output.svg\"))
   - (generate-shape-cli (list \"output.svg\" \"--parallel-count\" \"7\"))"
  
  (let ((filename (first args))
        (options nil))
    
    ;; Parse command line arguments and collect keyword options in proper order
    (loop for i from 1 below (length args) by 2
          for key = (nth i args)
          for val = (nth (1+ i) args)
          when (and key val)
          do (cond ((string= key "--parallel-count") 
                    (setf options (append options (list :parallel-count (parse-integer val)))))
                   ((string= key "--line-count") 
                    (setf options (append options (list :line-count (parse-integer val)))))
                   ((string= key "--segment-count") 
                    (setf options (append options (list :segment-count (parse-integer val)))))
                   ((string= key "--lower-limit") 
                    (setf options (append options (list :lower-limit (read-from-string val)))))
                   ((string= key "--upper-limit") 
                    (setf options (append options (list :upper-limit (read-from-string val)))))
                   ((string= key "--position-min-spacing") 
                    (setf options (append options (list :position-min-spacing (read-from-string val)))))
                   ((string= key "--segment-min-spacing") 
                    (setf options (append options (list :segment-min-spacing (read-from-string val)))))
                   ((string= key "--width") 
                    (setf options (append options (list :width (parse-integer val)))))
                   ((string= key "--height") 
                    (setf options (append options (list :height (parse-integer val)))))
                   ((string= key "--stroke") 
                    (setf options (append options (list :stroke val))))
                   ((string= key "--stroke-width") 
                    (setf options (append options (list :stroke-width (parse-integer val))))))
    
    ;; Check for required filename
    (unless filename
      (error "Output filename is required"))
    
    ;; Generate the shape with the specified options and return the filename
    (apply #'create-random-shape-svg filename options))))

;;;-----------------------------------------------------------------------------
;;; Example Usage
;;;-----------------------------------------------------------------------------

;; Examples:
;; Basic usage to create a random shape SVG:
;; (create-random-shape-svg "output.svg")
;;
;; Create a shape with custom parameters:
;; (create-random-shape-svg "output.svg" :parallel-count 7 :stroke "blue" :stroke-width 2)
;;
;; Command-line interface usage (for binary executable):
;; (generate-shape-cli (list "output.svg" "--parallel-count" "7" "--stroke" "red"))

;;;-----------------------------------------------------------------------------
;;; End of main.lisp
;;;-----------------------------------------------------------------------------

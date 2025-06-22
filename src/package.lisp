(defpackage :vorm
  (:documentation "Main package for the VORM system.")
  (:use :cl :alexandria) ;; Using Alexandria for common utilities
  (:export 
   ;; Math tolerance constants
   #:*linear-tolerance*
   #:*angular-tolerance*
   #:+two-pi+
   
   ;; Math tolerance functions
   #:linear-equal
   #:normalize-angle
   #:angular-equal
   #:with-custom-tolerance
   
   ;; Segment structure and accessors
   #:segment
   #:segment-lower
   #:segment-upper
   #:make-segment
   
   ;; Segment operations
   #:segments-overlap-p
   #:segments-merge
   
   ;; Line structure and accessors
   #:line
   #:line-segments
   #:make-line
   
   ;; Line operations
   #:lines-merge
   #:line-empty-p
   
   ;; Parallels structure and accessors
   #:parallels
   #:parallels-lines
   #:make-parallels
   
   ;; Parallels operations
   #:parallels-add-lines
   #:parallels-positions
   
   ;; Shape structure and accessors
   #:shape
   #:shape-angle-parallels
   #:make-shape
   
   ;; Shape operations
   #:shape-angles
   #:shape-add-parallels
   
   ;; SVG visualization
   #:render-shape-to-svg
   #:render-shape-to-file
   #:transform-segment-coordinates
   
   ;; Random number generation
   #:generate-random-set
   #:generate-random-line
   #:generate-random-parallels
   #:generate-random-shape
   
   ;; Random shape creation and SVG output
   #:create-random-shape-svg
   #:generate-shape-cli))

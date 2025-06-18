(defpackage :vorm
  (:use :cl)
  (:documentation "Main package for the vorm system")
  (:export ;; Main functions
           #:main
           
           ;; Utility functions
           #:ensure-list
           #:with-gensyms
           #:point-distance
           #:degrees-to-radians
           #:radians-to-degrees
           
           ;; Basic Shape classes and functions
           #:shape
           #:shape-id
           #:shape-metadata
           #:point
           #:point-x
           #:point-y
           #:make-point
           
           ;; Shape generic functions
           #:shape-contains-p
           #:shape-intersects-p
           #:shape-equals-p
           #:shape-area
           #:shape-perimeter
           #:shape-bounds
           
           ;; Line class and functions
           #:line
           #:line-start
           #:line-end
           #:line-length
           #:make-line
           
           ;; Polygon class and functions
           #:polygon
           #:polygon-vertices
           #:make-polygon
           #:make-rectangle
           
           ;; Circle class and functions
           #:circle
           #:circle-center
           #:circle-radius
           #:make-circle
           
           ;; Transformation classes and functions
           #:transformation
           #:transformation-name
           #:apply-transformation
           #:compose-transformations
           
           ;; Translation transformation
           #:translation
           #:translation-dx
           #:translation-dy
           #:make-translation
           
           ;; Rotation transformation
           #:rotation
           #:rotation-angle
           #:rotation-center
           #:make-rotation
           
           ;; Scaling transformation
           #:scaling
           #:scaling-sx
           #:scaling-sy
           #:scaling-center
           #:make-scaling
           
           ;; Reflection transformation
           #:reflection
           #:reflection-line
           #:make-reflection
           
           ;; Composed transformation
           #:composed-transformation
           #:composed-transformation-transformations
           #:identity-transformation
           
           ;; Grammar classes and functions
           #:rule
           #:rule-left-side
           #:rule-right-side
           #:rule-label
           #:rule-probability
           #:make-rule
           #:grammar
           #:grammar-name
           #:grammar-axiom
           #:grammar-rules
           #:make-grammar
           #:add-rule
           #:match-shape
           #:apply-rule
           #:apply-grammar-step
           #:apply-grammar
           #:substitute-bindings))

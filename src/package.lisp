(defpackage :vorm
  (:use :cl)
  (:documentation "Main package for the vorm system, providing a shape grammar implementation.
                   
                   Vorm is a framework for creating and manipulating
                   geometric shapes through rule-based transformations. The system allows for the
                   definition of shape grammars that can generate complex patterns from simple starting
                   shapes through repeated application of transformation rules.
                   
                   Key components:
                   - Basic shape definitions (point, line, circle, polygon, rectangle)
                   - Geometric transformations (translation, rotation, scaling, reflection)
                   - Grammar system (rules, pattern matching, substitution)
                   - Parser for grammar definitions
                   - Interactive execution environment")
  (:export ;; Main functions
           #:main
           
           ;; Utility functions
           #:ensure-list
           #:with-gensyms
           
           ;; Basic Shape classes and functions
           #:shape
           #:shape-id
           #:shape-metadata
           #:point
           #:point-x
           #:point-y
           
           ;; Shape generic functions
           #:shape-equals-p
           
           ;; Line class and functions
           #:line
           #:line-start
           #:line-end
           #:make-line
           
           ;; No polygon or circle classes in minimal geometry branch
           
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
           #:substitute-bindings
           
           ;; Parser functions
           #:parse-shape
           #:parse-line
           #:parse-transformation
           #:parse-translation
           #:parse-rotation
           #:parse-scaling
           #:parse-reflection
           #:parse-rule
           #:parse-grammar
           
           ;; Interpreter functions
           #:interpret-grammar
           #:generate-shapes
           #:evaluate-condition
           #:execute-grammar-script
           #:trace-grammar-execution
           #:start-interactive-grammar
           #:step-interactive-grammar
           #:get-interactive-grammar-state
           #:reset-interactive-grammar))

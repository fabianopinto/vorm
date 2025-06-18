(in-package :vorm)

;;;; Parser for the VORM shape grammar system
;;;; This module handles parsing grammar definitions from s-expressions

(defun parse-point (expr)
  "Parse a point expression: (point x y)"
  (unless (and (listp expr) (>= (length expr) 3))
    (error "Invalid point expression: ~S" expr))
  (make-point (second expr) (third expr)))

(defun parse-line (expr)
  "Parse a line expression: (line (x1 y1) (x2 y2))"
  (unless (and (listp expr) (>= (length expr) 3))
    (error "Invalid line expression: ~S" expr))
  (let ((start (second expr))
        (end (third expr)))
    (make-line (first start) (second start) (first end) (second end))))

(defun parse-polygon (expr)
  "Parse a polygon expression: (polygon (x1 y1) (x2 y2) ...)"
  (unless (and (listp expr) (>= (length expr) 2))
    (error "Invalid polygon expression: ~S" expr))
  (make-polygon (cdr expr)))

(defun parse-circle (expr)
  "Parse a circle expression: (circle (center-x center-y) radius)"
  (unless (and (listp expr) (>= (length expr) 3))
    (error "Invalid circle expression: ~S" expr))
  (let ((center (second expr))
        (radius (third expr)))
    (make-circle (first center) (second center) radius)))

(defun parse-rectangle (expr)
  "Parse a rectangle expression: (rectangle x y width height)"
  (unless (and (listp expr) (>= (length expr) 5))
    (error "Invalid rectangle expression: ~S" expr))
  (make-rectangle (second expr) (third expr) (fourth expr) (fifth expr)))

(defun parse-translation (expr)
  "Parse a translation expression: (translate dx dy)"
  (unless (and (listp expr) (>= (length expr) 3))
    (error "Invalid translation expression: ~S" expr))
  (make-translation (second expr) (third expr)))

(defun parse-rotation (expr)
  "Parse a rotation expression: (rotate angle center-x center-y)"
  (unless (and (listp expr) (>= (length expr) 4))
    (error "Invalid rotation expression: ~S" expr))
  (make-rotation (second expr) (third expr) (fourth expr)))

(defun parse-scaling (expr)
  "Parse a scaling expression: (scale sx sy center-x center-y)"
  (unless (and (listp expr) (>= (length expr) 5))
    (error "Invalid scaling expression: ~S" expr))
  (make-scaling (second expr) (third expr) (fourth expr) (fifth expr)))

(defun parse-reflection (expr)
  "Parse a reflection expression: (reflect line-x1 line-y1 line-x2 line-y2)"
  (unless (and (listp expr) (>= (length expr) 5))
    (error "Invalid reflection expression: ~S" expr))
  (make-reflection (second expr) (third expr) (fourth expr) (fifth expr)))

(defun parse-transformation (expr)
  "Parse a transformation expression."
  (unless (listp expr)
    (error "Invalid transformation expression: ~S" expr))
  (let ((type-sym (first expr)))
    (cond
      ((string-equal type-sym "translate") (parse-translation expr))
      ((string-equal type-sym "rotate") (parse-rotation expr))
      ((string-equal type-sym "scale") (parse-scaling expr))
      ((string-equal type-sym "reflect") (parse-reflection expr))
      ((string-equal type-sym "compose")
       ;; Handle composition of transformations
       (let ((transformations (mapcar #'parse-transformation (cdr expr))))
         (reduce #'compose-transformations transformations)))
      (t (error "Unknown transformation type: ~S" type-sym)))))

(defun parse-shape (expr)
  "Parse a shape expression into a shape object.
   
   Parameters:
     EXPR - A shape expression, which can be one of:
            - A symbol (variable or reference)
            - A shape expression like (point x y), (line (x1 y1) (x2 y2)), etc.
   
   Returns:
     A shape object representing the parsed expression, or the symbol if a variable
   
   Example:
     (parse-shape '(circle (50 50) 20)) ; Creates a circle at (50,50) with radius 20
     (parse-shape '(polygon (10 10) (30 10) (20 30))) ; Creates a triangle
     (parse-shape '?x) ; Returns the symbol ?x (represents a variable)"
  (cond
    ((symbolp expr)
     ;; Handle variable or reference
     expr)
    ((not (listp expr))
     (error "Invalid shape expression: ~S" expr))
    (t
     (let ((type-sym (first expr)))
       (cond
         ((string-equal type-sym "point") (parse-point expr))
         ((string-equal type-sym "line") (parse-line expr))
         ((string-equal type-sym "polygon") (parse-polygon expr))
         ((string-equal type-sym "circle") (parse-circle expr))
         ((string-equal type-sym "rectangle") (parse-rectangle expr))
         (t (error "Unknown shape type: ~S" type-sym)))))))

(defun parse-rule (expr)
  "Parse a rule expression into a rule object.
   
   Parameters:
     EXPR - A rule expression of the form:
            (rule left-side right-side &key label probability condition)
   
   Returns:
     A rule object representing the parsed expression
   
   Example:
     (parse-rule '(rule (circle (50 50) 20)
                       (polygon (30 30) (70 30) (50 70))
                       :label "circle-to-triangle"
                       :probability 0.8
                       :condition (> ?r 10)))
   
   The left-side and right-side are parsed with parse-shape.
   Options include:
   - :label - A descriptive label for the rule
   - :probability - Value between 0.0 and 1.0 (default: 1.0)
   - :condition - An expression that must evaluate to true for rule to apply"
  (unless (and (listp expr) (>= (length expr) 3))
    (error "Invalid rule expression: ~S" expr))
  
  (let ((left-side (parse-shape (second expr)))
        (right-side (parse-shape (third expr)))
        (options (cdddr expr))
        label probability condition)
    
    ;; Process options
    (loop for (key value) on options by #'cddr
          do (case key
               (:label (setf label value))
               (:probability (setf probability value))
               (:condition (setf condition (eval `(lambda (shape bindings)
                                                   (declare (ignorable shape bindings))
                                                   ,value))))
               (otherwise (warn "Unknown rule option: ~S" key))))
    
    (make-rule left-side right-side
               :label label
               :probability (or probability 1.0)
               :condition condition)))

(defun parse-grammar (expr)
  "Parse a grammar expression into a grammar object.
   
   Parameters:
     EXPR - A grammar expression of the form:
            (grammar name axiom &key rules metadata)
   
   Returns:
     A grammar object representing the parsed expression
   
   Example:
     (parse-grammar 
      '(grammar "my-grammar"
                (circle (50 50) 20)
                :rules ((rule (circle (50 50) 20)
                            (polygon (30 30) (70 30) (50 70))
                            :label "circle-to-triangle"))))
   
   The rules key takes a list of rule expressions, each parsed with parse-rule.
   The metadata key can store arbitrary data with the grammar.
   
   See also:
     PARSE-RULE - Parses rule expressions within the grammar
     INTERPRET-GRAMMAR - Directly applies a parsed grammar expression
     GENERATE-SHAPES - Generates shapes from a grammar object"
  (unless (and (listp expr) (>= (length expr) 3))
    (error "Invalid grammar expression: ~S" expr))
  
  (let ((name (second expr))
        (axiom (parse-shape (third expr)))
        (options (cdddr expr))
        rules metadata)
    
    ;; Process options
    (loop for (key value) on options by #'cddr
          do (case key
               (:rules (setf rules (mapcar #'parse-rule value)))
               (:metadata (setf metadata value))
               (otherwise (warn "Unknown grammar option: ~S" key))))
    
    (make-grammar name axiom :rules rules :metadata metadata)))

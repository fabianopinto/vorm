(in-package :vorm)

;;;; Grammar definitions for the VORM shape grammar system

;;; Rule class
(defclass rule ()
  ((id :initarg :id
       :accessor rule-id
       :initform (gensym "RULE-")
       :documentation "Unique identifier for the rule")
   (label :initarg :label
          :accessor rule-label
          :initform nil
          :documentation "Optional label for the rule")
   (left-side :initarg :left-side
              :accessor rule-left-side
              :documentation "Left-hand side (pattern) of the rule")
   (right-side :initarg :right-side
               :accessor rule-right-side
               :documentation "Right-hand side (replacement) of the rule")
   (condition :initarg :condition
              :accessor rule-condition
              :initform (constantly t)
              :documentation "Optional condition function that must be satisfied for rule to apply")
   (probability :initarg :probability
                :accessor rule-probability
                :initform 1.0
                :type (real 0.0 1.0)
                :documentation "Probability of rule application (0.0-1.0)"))
  (:documentation "Represents a shape grammar rule with left and right sides."))

(defun make-rule (left-side right-side &key id label condition (probability 1.0))
  "Create a shape grammar rule with LEFT-SIDE (pattern) and RIGHT-SIDE (replacement).
   
   Parameters:
     LEFT-SIDE - The pattern to match against shapes
     RIGHT-SIDE - The replacement shape or shapes
     ID - Optional unique identifier for the rule
     LABEL - Optional descriptive label for the rule
     CONDITION - Optional function that must evaluate to true for rule to be applied
     PROBABILITY - Value between 0.0 and 1.0 indicating application probability (default: 1.0)
   
   Returns:
     A new rule instance
   
   Example:
     (make-rule (make-circle 50 50 20) 
                (make-polygon '((30 30) (70 30) (50 70)))
                :label "circle-to-triangle"
                :probability 0.8)"
  (make-instance 'rule
                 :id (or id (gensym "RULE-"))
                 :label label
                 :left-side left-side
                 :right-side right-side
                 :condition (or condition (constantly t))
                 :probability (max 0.0 (min probability 1.0))))

;;; Grammar class
(defclass grammar ()
  ((name :initarg :name
         :accessor grammar-name
         :initform "Unnamed Grammar"
         :documentation "Name of the grammar")
   (rules :initarg :rules
          :accessor grammar-rules
          :initform nil
          :type list
          :documentation "List of rules in the grammar")
   (axiom :initarg :axiom
          :accessor grammar-axiom
          :documentation "Initial shape or set of shapes")
   (metadata :initarg :metadata
             :accessor grammar-metadata
             :initform nil
             :documentation "Additional metadata for the grammar"))
  (:documentation "Represents a shape grammar with rules and an axiom."))

(defun make-grammar (name axiom &key rules metadata)
  "Create a shape grammar with NAME and AXIOM (initial shape).
   
   Parameters:
     NAME - A string name for the grammar
     AXIOM - The initial shape or list of shapes
     RULES - Optional list of rules to add to the grammar
     METADATA - Optional additional data associated with the grammar
   
   Returns:
     A new grammar instance
   
   Example:
     (make-grammar "simple-grammar"
                  (make-circle 50 50 20)
                  :rules (list (make-rule ...)))
   
   See also:
     MAKE-RULE - Creates rules to add to the grammar
     APPLY-GRAMMAR - Applies a grammar for multiple iterations
     PARSE-GRAMMAR - Creates a grammar from an s-expression"
  (make-instance 'grammar
                 :name name
                 :axiom axiom
                 :rules rules
                 :metadata metadata))

(defun add-rule (grammar rule)
  "Add a RULE to the GRAMMAR."
  (push rule (grammar-rules grammar))
  grammar)

;;; Helper functions
(defun rotate-list (lst n)
  "Rotate LST by N positions."
  (let* ((len (length lst))
         (n-mod (mod n len)))
    (append (subseq lst n-mod) (subseq lst 0 n-mod))))

;;; Rule application
(defun match-shape (pattern shape &optional bindings)
  "Match a pattern to a shape, returning bindings if successful or NIL if not.
   
   Parameters:
     PATTERN - The left-hand side of a rule or pattern to match
     SHAPE - The shape to match against the pattern
     BINDINGS - Optional alist of existing variable bindings
   
   Returns:
     An alist of variable bindings if match is successful, or NIL if no match
     A value of '(t) indicates a successful match with no variable bindings
   
   Example:
     (match-shape '?x (make-point 10 20)) ; Binds ?x to the point"
  (cond
    ;; If pattern is a variable (symbol starting with ?), bind it to the shape
    ((and (symbolp pattern) (char= (char (symbol-name pattern) 0) #\?))
     (let ((existing (assoc pattern bindings)))
       (if existing
           ;; If variable is already bound, check for consistency
           (and (shape-equals-p (cdr existing) shape) bindings)
           ;; Otherwise, add new binding
           (acons pattern shape bindings))))
    
    ;; Match shape types
    ((and (typep pattern 'point) (typep shape 'point))
     ;; Point matching: compare x and y coordinates with almost-equal
     (if (and (almost-equal (point-x pattern) (point-x shape))
              (almost-equal (point-y pattern) (point-y shape)))
         ;; Explicitly return bindings (or non-nil) on successful match
         (or bindings '(t))
         ;; Return nil if match fails
         nil))
    
    ;; Line matching
    ((and (typep pattern 'line) (typep shape 'line))
     ;; Ensure both objects have valid start and end points
     (if (and (line-start pattern) (line-end pattern) 
              (line-start shape) (line-end shape))
         ;; Check if start points match using almost-equal for coordinates
         (if (and (almost-equal (point-x (line-start pattern)) (point-x (line-start shape)))
                  (almost-equal (point-y (line-start pattern)) (point-y (line-start shape))))
             ;; If start points match, check end points
             (if (and (almost-equal (point-x (line-end pattern)) (point-x (line-end shape)))
                      (almost-equal (point-y (line-end pattern)) (point-y (line-end shape))))
                 ;; If both start and end match, return bindings
                 (or bindings '(t))
                 ;; End points don't match
                 nil)
             ;; Start points don't match
             nil)
         ;; Invalid line objects
         nil))
    
    ;; Fallback: no match found
    (t nil)))

(defun process-vertex-coordinate (coord bindings)
  "Process a vertex coordinate, which could be a number, variable, or expression."
  (cond
    ;; If it's a variable (symbol starting with ?), substitute its binding
    ((and (symbolp coord) (char= (char (symbol-name coord) 0) #\?))
     (let ((binding (assoc coord bindings)))
       (if binding
           (cdr binding)
           coord)))
    
    ;; If it's an arithmetic expression, evaluate it recursively
    ((and (listp coord) (member (first coord) '(+ - * / expt sqrt)))
     (let ((op (first coord))
           (args (mapcar (lambda (arg) (process-vertex-coordinate arg bindings)) (rest coord))))
       (cond
         ((eq op '+) (apply #'+ args))
         ((eq op '-) (apply #'- args))
         ((eq op '*) (apply #'* args))
         ((eq op '/) (apply #'/ args))
         ((eq op 'expt) (apply #'expt args))
         ((eq op 'sqrt) (apply #'sqrt args))
         (t coord))))
    
    ;; Otherwise, return as is
    (t coord)))

(defun evaluate-expression (expr bindings)
  "Evaluate an arithmetic expression with variables replaced by their bindings."
  (cond
    ;; Variable substitution
    ((and (symbolp expr) (char= (char (symbol-name expr) 0) #\?))
     (let ((binding (assoc expr bindings)))
       (if binding
           (cdr binding)
           expr)))
    
    ;; Recursive evaluation of list expressions (arithmetic operations)
    ((listp expr)
     (let ((op (first expr))
           (args (mapcar (lambda (arg) (evaluate-expression arg bindings)) (rest expr))))
       (cond
         ((eq op '+) (apply #'+ args))
         ((eq op '-) (apply #'- args))
         ((eq op '*) (apply #'* args))
         ((eq op '/) (apply #'/ args))
         ((eq op 'expt) (apply #'expt args))
         ((eq op 'sqrt) (apply #'sqrt args))
         (t expr))))
    
    ;; Return other values unchanged (numbers, etc.)
    (t expr)))

(defun substitute-bindings (shape bindings)
  "Replace variables in SHAPE with their bound values from BINDINGS.
   
   Parameters:
     SHAPE - A shape or pattern containing variables
     BINDINGS - An alist mapping variable symbols to values
   
   Returns:
     A new shape with all variables replaced by their bound values
   
   Example:
     (substitute-bindings '(make-point ?x ?y) '((?x . 10) (?y . 20)))
     ; Returns (make-point 10 20)"
  (when shape  ;; Guard against NIL inputs
    (cond
      ;; If shape is a variable, substitute its binding
      ((and (symbolp shape) (char= (char (symbol-name shape) 0) #\?))
       (let ((binding (assoc shape bindings)))
         (if binding
             (cdr binding)
             shape)))
      
      ;; Special handling for polygon vertex pairs in rule definitions
      ((and (listp shape) (= (length shape) 2) 
            (or (numberp (first shape)) (symbolp (first shape)) (listp (first shape)))
            (or (numberp (second shape)) (symbolp (second shape)) (listp (second shape))))
       (list (process-vertex-coordinate (first shape) bindings)
             (process-vertex-coordinate (second shape) bindings)))
      
      ;; For arithmetic expressions in lists that aren't shape definitions
      ((and (listp shape) (not (typep shape 'point)) 
            (member (first shape) '(+ - * / expt sqrt)))
       (evaluate-expression shape bindings))
      
      ;; Recursively substitute bindings in composite shapes
      ((typep shape 'line)
       (make-instance 'line
                      :start (substitute-bindings (line-start shape) bindings)
                      :end (substitute-bindings (line-end shape) bindings)
                      :id (shape-id shape)
                      :metadata (shape-metadata shape)))
      
      ((typep shape 'point)
       (make-instance 'point
                      :x (process-vertex-coordinate (point-x shape) bindings)
                      :y (process-vertex-coordinate (point-y shape) bindings)
                      :id (shape-id shape)
                      :metadata (shape-metadata shape)))
      
      ;; Otherwise, return shape unchanged
      (t shape))))

(defun apply-rule (rule shape &optional (transformation (identity-transformation)))
  "Apply RULE to SHAPE using TRANSFORMATION, returning a list of results.
   
   Parameters:
     RULE - The shape grammar rule to apply
     SHAPE - The shape to transform
     TRANSFORMATION - Optional transformation to apply to the result (default: identity)
   
   Returns:
     A list containing the transformed shape(s) if rule applies, or NIL if not
   
   Example:
     (apply-rule my-rule (make-circle 50 50 20)) ; Applies rule to circle"
  (when (and rule shape)
    ;; Try to match the left side of the rule with the shape
    (let ((bindings (match-shape (rule-left-side rule) shape)))
      ;; If match succeeded and bindings were created 
      ;; Note: A bindings value of '(t) indicates a successful match with no actual bindings
      (when bindings
        ;; Check condition and probability
        (when (and (funcall (rule-condition rule) shape bindings)
                   (<= (random 1.0) (rule-probability rule)))
          ;; First substitute bindings in the right side of the rule
          (let ((substituted (substitute-bindings (rule-right-side rule) bindings)))
            ;; Ensure we have a valid substituted shape
            (when substituted
              ;; Apply the transformation to the substituted shape
              (let ((transformed-shape (apply-transformation transformation substituted)))
                ;; Make sure we have a valid result and return it as a list
                (when transformed-shape
                  (list transformed-shape))))))))))

(defun apply-grammar-step (grammar shapes)
  "Apply one step of GRAMMAR to SHAPES, returning the new set of shapes.
   
   Parameters:
     GRAMMAR - The shape grammar to apply
     SHAPES - List of shapes to transform
   
   Returns:
     A new list of shapes after applying one rule transformation
     Returns original shapes if no rule could be applied
   
   Example:
     (apply-grammar-step my-grammar (list (make-circle 50 50 20)))
   
   This function attempts to apply one rule from the grammar to one shape.
   It prioritizes rules in the order they appear in the grammar."
  (let ((result nil)  ;; Use a different variable name to avoid confusion
        (applied nil))
    ;; Try each rule on each shape
    (dolist (rule (grammar-rules grammar))
      (dolist (shape shapes)
        ;; Skip if we've already applied a rule
        (unless applied
          ;; Try to apply the rule to this shape
          (let ((application (apply-rule rule shape)))
            ;; Check if application succeeded and produced a valid shape
            (when (and application 
                       (car application) ;; Make sure we have a non-nil first element
                       (not (eq (car application) shape))) ;; Make sure it's different from original
              ;; Mark that we've applied a rule
              (setf applied t)
              ;; Create new shape list: replace the current shape with the application result
              (setf result (append application
                                   (remove shape shapes :test #'eq))))))))
    ;; If no rule applied, return original shapes unchanged
    (or result shapes)))

(defun apply-grammar (grammar max-iterations &optional (shapes (ensure-list (grammar-axiom grammar))))
  "Apply GRAMMAR repeatedly for up to MAX-ITERATIONS steps, starting with SHAPES.
   
   Parameters:
     GRAMMAR - The shape grammar to apply
     MAX-ITERATIONS - Maximum number of iterations to perform
     SHAPES - Optional initial shapes (default: grammar's axiom)
   
   Returns:
     The final set of shapes after all iterations
   
   Example:
     (apply-grammar my-grammar 5) ; Apply grammar for 5 iterations
   
   This function stops early if no changes are made in an iteration."
  (let ((current-shapes shapes))
    (dotimes (i max-iterations current-shapes)
      (let ((new-shapes (apply-grammar-step grammar current-shapes)))
        (setf current-shapes new-shapes)
        ;; Stop if no changes were made or we've reached max iterations
        (when (or (= i (1- max-iterations))
                  (equal new-shapes shapes))
          (return current-shapes))))))

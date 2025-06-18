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
   CONDITION is an optional function that must return true for rule to be applied.
   PROBABILITY is a value between 0.0 and 1.0 indicating the probability of applying the rule."
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
   RULES is an optional list of rules to add to the grammar."
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
   PATTERN is the left-hand side of a rule.
   SHAPE is the shape to match against.
   BINDINGS is an optional alist of existing bindings."
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
    
    ;; Circle matching
    ((and (typep pattern 'circle) (typep shape 'circle))
     (if (almost-equal (circle-radius pattern) (circle-radius shape))
         (let ((center-match (match-shape (circle-center pattern) (circle-center shape) bindings)))
           (or center-match '(t)))
         nil))
    
    ;; Polygon matching
    ((and (typep pattern 'polygon) (typep shape 'polygon))
     (let ((pattern-vertices (polygon-vertices pattern))
           (shape-vertices (polygon-vertices shape)))
       (when (= (length pattern-vertices) (length shape-vertices))
         ;; Try to match vertices in sequence, allowing for rotational variants
         (or (some (lambda (i)
                     (let ((rotated-vertices (rotate-list shape-vertices i))
                           (result bindings))
                       (loop for pv in pattern-vertices
                             for sv in rotated-vertices
                             while result
                             do (setf result (match-shape pv sv result)))
                       result))
                   ;; Generate list from 0 to (length shape-vertices - 1)
                   (loop for i from 0 below (length shape-vertices) collect i))
             ;; Try with reversed vertices (reflection)
             (let ((result bindings))
               (loop for pv in pattern-vertices
                     for sv in (reverse shape-vertices)
                     while result
                     do (setf result (match-shape pv sv result)))
               result)))))
    
    ;; Fallback: no match found
    (t nil)))

(defun substitute-bindings (shape bindings)
  "Replace variables in SHAPE with their bound values from BINDINGS."
  (when shape  ;; Guard against NIL inputs
    (cond
      ;; If shape is a variable, substitute its binding
      ((and (symbolp shape) (char= (char (symbol-name shape) 0) #\?))
       (let ((binding (assoc shape bindings)))
         (if binding
             (cdr binding)
             shape)))
      
      ;; Recursively substitute bindings in composite shapes
      ((typep shape 'line)
       (make-instance 'line
                      :start (substitute-bindings (line-start shape) bindings)
                      :end (substitute-bindings (line-end shape) bindings)
                      :id (shape-id shape)
                      :metadata (shape-metadata shape)))
      
      ((typep shape 'polygon)
       (make-instance 'polygon
                      :vertices (mapcar (lambda (v) (substitute-bindings v bindings))
                                        (polygon-vertices shape))
                      :id (shape-id shape)
                      :metadata (shape-metadata shape)))
      
      ((typep shape 'circle)
       (make-instance 'circle
                      :center (substitute-bindings (circle-center shape) bindings)
                      :radius (circle-radius shape)
                      :id (shape-id shape)
                      :metadata (shape-metadata shape)))
      
      ;; Otherwise, return shape unchanged
      (t shape))))

(defun apply-rule (rule shape &optional (transformation (identity-transformation)))
  "Apply RULE to SHAPE using TRANSFORMATION, returning a list of results.
   Returns NIL if the rule cannot be applied to the shape."
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
   Attempts to apply one rule from the grammar to one shape."
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
   Returns the final set of shapes after all iterations."
  (let ((current-shapes shapes))
    (dotimes (i max-iterations current-shapes)
      (let ((new-shapes (apply-grammar-step grammar current-shapes)))
        (setf current-shapes new-shapes)
        ;; Stop if no changes were made or we've reached max iterations
        (when (or (= i (1- max-iterations))
                  (equal new-shapes shapes))
          (return current-shapes))))))

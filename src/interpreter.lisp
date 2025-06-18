(in-package :vorm)

;;;; Interpreter for the VORM shape grammar system
;;;; This module handles the interpretation and execution of shape grammars
;;;; 
;;;; The interpreter takes shape grammars and applies their rules to generate new shapes.
;;;; It provides functions for one-time grammar application, step-by-step execution,
;;;; execution tracing, and interactive grammar exploration.
;;;;
;;;; Expression Examples:
;;;; 
;;;; 1. Basic grammar with simple transformations:
;;;;    (grammar "simple-example"
;;;;             (circle (50 50) 20)              ; Starting with a circle
;;;;             :rules ((rule (circle (?x ?y) ?r) ; Match circle with any position and radius
;;;;                          (rectangle (?x ?y) (+ ?x (* 2 ?r)) (+ ?y (* 2 ?r))) ; Create rectangle
;;;;                          :label "circle-to-rectangle")))
;;;;
;;;; 2. Handling variables in expressions:
;;;;    Shape variables (prefixed with '?') can be used in expressions and are bound
;;;;    during pattern matching. For example, ?x and ?y in (circle (?x ?y) ?r) will be
;;;;    bound to the actual coordinates when matching a specific circle.
;;;;    
;;;;    IMPORTANT: Circle definitions use the format (circle (x y) r) where the center
;;;;    is represented as a list of coordinates, NOT as separate arguments.

(defun interpret-grammar (grammar-expr max-iterations)
  "Interpret a grammar expression and generate shapes by applying it for MAX-ITERATIONS steps.
   
   Parameters:
     GRAMMAR-EXPR - A grammar expression that will be parsed into a grammar object
     MAX-ITERATIONS - Maximum number of iteration steps to perform
   
   Returns:
     The final list of shapes after applying the grammar rules
   
   See also:
     PARSE-GRAMMAR - Parses the grammar expression without applying it
     GENERATE-SHAPES - Lower-level function to generate shapes from a grammar object
     TRACE-GRAMMAR-EXECUTION - Executes the grammar with detailed tracing information"
  (let ((grammar (parse-grammar grammar-expr)))
    (apply-grammar grammar max-iterations)))

(defun generate-shapes (grammar max-iterations &key (include-intermediates nil))
  "Generate shapes from GRAMMAR by applying it for MAX-ITERATIONS steps.
   
   Parameters:
     GRAMMAR - A grammar object to apply
     MAX-ITERATIONS - Maximum number of iteration steps to perform
     INCLUDE-INTERMEDIATES - When true, returns all intermediate steps as a list
   
   Returns:
     If INCLUDE-INTERMEDIATES is true, returns a list of shape lists,
     with each element representing the shapes after each iteration.
     Otherwise, returns only the final list of shapes.
   
   See also:
     APPLY-GRAMMAR - Lower-level function that applies a grammar for MAX-ITERATIONS steps
     INTERPRET-GRAMMAR - Parses and applies a grammar expression
     TRACE-GRAMMAR-EXECUTION - Similar functionality but with detailed execution trace
     START-INTERACTIVE-GRAMMAR - For step-by-step interactive grammar application"
  (let* ((initial-shapes (ensure-list (grammar-axiom grammar)))
         (shape-list initial-shapes)
         (results (when include-intermediates
                    (list initial-shapes))))
    (dotimes (i max-iterations)
      (let ((new-shapes (apply-grammar-step grammar shape-list)))
        (setf shape-list new-shapes)
        (when include-intermediates
          (push new-shapes results))
        ;; Stop if no changes were made
        (when (equal new-shapes shape-list)
          (return))))
    
    (if include-intermediates
        (reverse results)
        shape-list)))

(defun evaluate-condition (condition-expr shape bindings)
  "Evaluate a condition expression in the context of SHAPE and BINDINGS.
   
   Parameters:
     CONDITION-EXPR - An expression to evaluate
     SHAPE - The shape to evaluate the condition against
     BINDINGS - The variable bindings from the rule matching
   
   Returns:
     The result of evaluating the condition
   
   Example:
     ;; With a circle at (50,50) radius 20, and bindings from pattern matching
     ;; evaluate-condition '(> ?r 10) shape '((?x . 50) (?y . 50) (?r . 20))
     ;; => T (since 20 > 10)
     
     ;; Can also check shape properties and perform complex tests
     ;; evaluate-condition '(and (> ?r 10) (< (point-distance '(0 0) (?x ?y)) 100)) shape bindings
     ;; => T if circle radius > 10 and center is within 100 units of origin"
  (let ((condition-fn (eval `(lambda (shape bindings)
                              (declare (ignorable shape bindings))
                              ,condition-expr))))
    (funcall condition-fn shape bindings)))

(defun execute-grammar-script (script-file &key (max-iterations 100))
  "Execute a grammar script from a file.
   
   Parameters:
     SCRIPT-FILE - Path to the file containing a grammar expression
     MAX-ITERATIONS - Maximum number of iteration steps to perform (default: 100)
   
   Returns:
     The final list of shapes after applying the grammar rules
   
   The script should contain a valid grammar expression that can be read and parsed."
  (with-open-file (in script-file :direction :input)
    (let ((expr (read in)))
      (interpret-grammar expr max-iterations))))

;;; Grammar evaluation with step-by-step tracing
(defun trace-grammar-execution (grammar max-iterations)
  "Execute GRAMMAR for MAX-ITERATIONS steps, tracing each step.
   
   Parameters:
     GRAMMAR - A grammar object to apply
     MAX-ITERATIONS - Maximum number of iteration steps to perform
   
   Returns:
     Two values: the final list of shapes and a list of execution traces
     Each trace entry contains information about shapes and rules applied at each step."
  (let ((shape-list (ensure-list (grammar-axiom grammar)))
        (trace-list nil))
    
    ;; Record initial state
    (push (list :step 0 :shapes shape-list :rule nil) trace-list)
    
    ;; Execute for max-iterations or until no rules apply
    (dotimes (i max-iterations)
      ;; Try to apply rules until one succeeds
      (let ((applied-rule nil)
            (new-shapes nil))
        
        ;; Try each rule
        (dolist (rule (grammar-rules grammar))
          (unless applied-rule
            (dolist (shape shape-list)
              (unless applied-rule
                (let ((application (apply-rule rule shape)))
                  (when application
                    (setf applied-rule rule)
                    (setf new-shapes (append application
                                           (remove shape shape-list :test #'eq)))))))))
        
        ;; If no rule applied, we're done
        (unless applied-rule
          (return))
        
        ;; Update current state
        (setf shape-list new-shapes)
        
        ;; Record this step
        (push (list :step (1+ i) 
                   :shapes shape-list 
                   :rule (rule-label applied-rule))
              trace-list)))
    
    (values shape-list (reverse trace-list))))

;;; State tracking for interactive execution
(defvar *grammar-state* nil
  "Current grammar execution state for interactive mode.
   This is a property list containing:
   - :grammar - The grammar object being applied
   - :shapes - The current shapes after the latest step
   - :history - List of shape sets from each step
   - :step - Current step number")

(defun start-interactive-grammar (grammar)
  "Start interactive execution of GRAMMAR.
   
   Parameters:
     GRAMMAR - A grammar object to apply interactively
   
   Returns:
     The initial set of shapes from the grammar axiom
   
   This function initializes the global *grammar-state* for step-by-step
   execution controlled by the user."
  (let ((initial-shapes (ensure-list (grammar-axiom grammar))))
    (setf *grammar-state* (list :grammar grammar
                               :shapes initial-shapes
                               :history (list initial-shapes)
                               :step 0))
    initial-shapes))

(defun step-interactive-grammar ()
  "Execute one step of the current interactive grammar.
   
   Returns:
     The new set of shapes after applying a rule, or NIL if no rules applied
   
   Raises an error if no grammar has been started with START-INTERACTIVE-GRAMMAR.
   Updates the global *grammar-state* with the new shapes and step information.
   
   Example:
     ;; Start interactive grammar execution
     (start-interactive-grammar my-grammar)
     ;; Execute one step and get the updated shapes
     (step-interactive-grammar) ; => list of shapes after one rule application"
  (unless *grammar-state*
    (error "No active grammar. Call START-INTERACTIVE-GRAMMAR first."))
  
  (let* ((grammar (getf *grammar-state* :grammar))
         (shape-list (getf *grammar-state* :shapes))
         (new-shapes (apply-grammar-step grammar shape-list)))
    
    (if (equal new-shapes shape-list)
        ;; No changes, we're done
        nil
        ;; Update state
        (progn
          (setf (getf *grammar-state* :shapes) new-shapes)
          (push new-shapes (getf *grammar-state* :history))
          (incf (getf *grammar-state* :step))
          new-shapes))))

(defun get-interactive-grammar-state ()
  "Get the current state of the interactive grammar.
   
   Returns:
     A property list with the current grammar execution state including:
     - :step - Current step number
     - :shapes - Current shapes after the latest step
     - :history - List of shape sets from each step
   
   Raises an error if no grammar has been started with START-INTERACTIVE-GRAMMAR."
  (unless *grammar-state*
    (error "No active grammar. Call START-INTERACTIVE-GRAMMAR first."))
  
  (list :step (getf *grammar-state* :step)
        :shapes (getf *grammar-state* :shapes)
        :history (getf *grammar-state* :history)))

(defun reset-interactive-grammar ()
  "Reset the interactive grammar to its initial state.
   
   Returns:
     The initial set of shapes from the grammar axiom
   
   Raises an error if no grammar has been started with START-INTERACTIVE-GRAMMAR."
  (unless *grammar-state*
    (error "No active grammar. Call START-INTERACTIVE-GRAMMAR first."))
  
  (let ((grammar (getf *grammar-state* :grammar)))
    (start-interactive-grammar grammar)))

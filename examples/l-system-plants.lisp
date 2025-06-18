(in-package :vorm.examples)

;;;; L-System Plant Growth Examples
;;;; 
;;;; This example demonstrates how to create plant-like structures using L-system based
;;;; shape grammars. L-systems are particularly well-suited for modeling plant growth
;;;; and other biological forms through iterative rule application.

(defvar *turtle-state* nil
  "Current state of the L-system turtle for drawing, including position, direction, and stack.")

(defun init-turtle (start-x start-y angle)
  "Initialize the turtle state at the given position and angle.
   
   Parameters:
     START-X - Starting X coordinate
     START-Y - Starting Y coordinate
     ANGLE - Starting angle in degrees (0 = right, 90 = up)
   
   Sets up the global *turtle-state* with initial position, direction and an empty stack."
  (setf *turtle-state* 
        (list :position (list start-x start-y)
              :angle angle
              :stack nil
              :line-length 10
              :angle-delta 25)))

(defun turtle-position ()
  "Get the current position of the turtle."
  (getf *turtle-state* :position))

(defun turtle-angle ()
  "Get the current angle of the turtle in degrees."
  (getf *turtle-state* :angle))

(defun turtle-move-forward ()
  "Move the turtle forward by its line length and return the old and new positions.
   
   Returns:
     A list containing the starting position and ending position of the move."
  (let* ((position (turtle-position))
         (angle (turtle-angle))
         (line-length (getf *turtle-state* :line-length))
         (angle-radians (* angle (/ pi 180)))
         (x (first position))
         (y (second position))
         (new-x (+ x (* line-length (cos angle-radians))))
         (new-y (+ y (* line-length (sin angle-radians))))
         (new-position (list new-x new-y)))
    (setf (getf *turtle-state* :position) new-position)
    (list position new-position)))

(defun turtle-rotate (delta)
  "Rotate the turtle by the specified delta angle.
   
   Parameters:
     DELTA - Angle change in degrees (positive = counterclockwise)"
  (let ((current-angle (turtle-angle)))
    (setf (getf *turtle-state* :angle) (+ current-angle delta))))

(defun turtle-push-state ()
  "Save the current turtle state on the stack."
  (push (list (copy-list (turtle-position))
              (turtle-angle)
              (getf *turtle-state* :line-length))
        (getf *turtle-state* :stack)))

(defun turtle-pop-state ()
  "Restore the turtle state from the stack."
  (if (getf *turtle-state* :stack)
      (let ((saved-state (pop (getf *turtle-state* :stack))))
        (setf (getf *turtle-state* :position) (first saved-state))
        (setf (getf *turtle-state* :angle) (second saved-state))
        (setf (getf *turtle-state* :line-length) (third saved-state)))
      (error "Turtle stack is empty.")))

(defun turtle-scale-length (factor)
  "Scale the turtle's line length by the given factor.
   
   Parameters:
     FACTOR - Multiplier for line length (e.g., 0.9 to reduce by 10%)"
  (let ((current-length (getf *turtle-state* :line-length)))
    (setf (getf *turtle-state* :line-length) (* current-length factor))))

(defun interpret-l-system (axiom rules iterations)
  "Generate an L-system string by applying rules for a number of iterations.
   
   Parameters:
     AXIOM - Initial string
     RULES - Alist of character replacements
     ITERATIONS - Number of iterations to apply the rules
   
   Returns:
     The final L-system string after all iterations"
  (let ((current axiom))
    (dotimes (i iterations current)
      (let ((next ""))
        (loop for c across current do
          (let ((replacement (cdr (assoc c rules))))
            (setf next (concatenate 'string next (or replacement (string c))))))
        (setf current next)))))

(defun l-system-to-shapes (l-string angle-delta)
  "Convert an L-system string to a list of shapes using turtle graphics.
   
   Parameters:
     L-STRING - The L-system string to interpret
     ANGLE-DELTA - Base angle change for turns (in degrees)
   
   Returns:
     A list of line shapes representing the L-system
   
   Interprets the following L-system symbols:
     F - Move forward and draw a line
     f - Move forward without drawing
     + - Turn left by angle-delta
     - - Turn right by angle-delta
     [ - Push current state onto stack
     ] - Pop state from stack
     < - Scale line length down by 10%
     > - Scale line length up by 10%"
  (init-turtle 300 450 -90) ; Start at middle bottom, facing up
  (setf (getf *turtle-state* :angle-delta) angle-delta)
  
  (let ((shapes nil))
    (loop for c across l-string do
      (case c
        (#\F ; Move forward and draw a line
         (let ((points (turtle-move-forward)))
           (push (make-instance 'line 
                               :start-point (make-instance 'point :x (first (first points)) :y (second (first points)))
                               :end-point (make-instance 'point :x (first (second points)) :y (second (second points))))
                 shapes)))
        (#\f ; Move forward without drawing
         (turtle-move-forward))
        (#\+ ; Turn left
         (turtle-rotate (getf *turtle-state* :angle-delta)))
        (#\- ; Turn right
         (turtle-rotate (- (getf *turtle-state* :angle-delta))))
        (#\[ ; Push state
         (turtle-push-state))
        (#\] ; Pop state
         (turtle-pop-state))
        (#\< ; Scale down
         (turtle-scale-length 0.9))
        (#\> ; Scale up
         (turtle-scale-length 1.1))))
    shapes))

;; Plant L-system examples
(defun create-plant-grammar ()
  "Create a simple plant grammar using L-system rules.
   
   Returns:
     A parsed shape grammar that generates a simple plant structure
   
   This function uses an L-system to create a basic plant with branching structure,
   then converts the L-system string to shapes that VORM can process."
  (let* ((axiom "F")
         (rules '((#\F . "FF-[-F+F+F]+[+F-F-F]")))
         (l-string (interpret-l-system axiom rules 3))
         (shapes (l-system-to-shapes l-string 22.5)))
    (make-instance 'grammar 
                  :name "plant-l-system"
                  :axiom shapes
                  :rules nil)))

(defun create-fern-grammar ()
  "Create a fern-like structure using L-system rules.
   
   Returns:
     A parsed shape grammar that generates a fern-like structure
   
   This function uses an L-system to create a fractal fern pattern,
   then converts the L-system string to shapes that VORM can process."
  (let* ((axiom "X")
         (rules '((#\X . "F-[[X]+X]+F[+FX]-X")
                  (#\F . "FF")))
         (l-string (interpret-l-system axiom rules 5))
         (shapes (l-system-to-shapes l-string 25)))
    (make-instance 'grammar 
                  :name "fern-l-system"
                  :axiom shapes
                  :rules nil)))

(defun create-tree-grammar ()
  "Create a tree-like structure using L-system rules.
   
   Returns:
     A parsed shape grammar that generates a tree-like structure
   
   This function uses an L-system to create a fractal tree pattern,
   then converts the L-system string to shapes that VORM can process."
  (let* ((axiom "F")
         (rules '((#\F . "F[+F]F[-F][F]")))
         (l-string (interpret-l-system axiom rules 4))
         (shapes (l-system-to-shapes l-string 27.5)))
    (make-instance 'grammar 
                  :name "tree-l-system"
                  :axiom shapes
                  :rules nil)))

(defun load-plant-structure ()
  "Generate a plant-like structure using L-system rules.
   
   Returns:
     A list of shapes representing the plant structure
   
   Example:
     (load-plant-structure) ; Generate a plant-like structure"
  (grammar-axiom (create-plant-grammar)))

(defun load-fern-structure ()
  "Generate a fern-like structure using L-system rules.
   
   Returns:
     A list of shapes representing the fern structure
   
   Example:
     (load-fern-structure) ; Generate a fern-like structure"
  (grammar-axiom (create-fern-grammar)))

(defun load-tree-structure ()
  "Generate a tree-like structure using L-system rules.
   
   Returns:
     A list of shapes representing the tree structure
   
   Example:
     (load-tree-structure) ; Generate a tree-like structure"
  (grammar-axiom (create-tree-grammar)))

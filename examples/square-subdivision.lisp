(in-package :vorm.examples)

;;;; Square Subdivision Shape Grammar Example
;;;; 
;;;; This example demonstrates how to create a recursive square subdivision pattern
;;;; using shape grammars. This type of pattern is often seen in architectural designs,
;;;; Mondrian-style art, and various computational design applications.

(defun create-mondrian-grammar ()
  "Create a grammar that generates a Mondrian-style subdivision pattern.
   
   Returns:
     A grammar object that can be used with interpret-grammar or generate-shapes
   
   This grammar starts with a single rectangle and recursively subdivides it
   into smaller rectangles, alternating between horizontal and vertical splits,
   creating an abstract grid-like composition."
  (parse-grammar
   '(grammar "mondrian-style"
            ;; Start with a single rectangle
            (rectangle (100 100) (500 400))
            :rules
            ;; Horizontal split rule - divides rectangle horizontally
            ((rule (rectangle (?x1 ?y1) (?x2 ?y2))
                   ((rectangle (?x1 ?y1) (?x2 (+ ?y1 (/ (- ?y2 ?y1) 2))))
                    (rectangle (?x1 (+ ?y1 (/ (- ?y2 ?y1) 2))) (?x2 ?y2)))
                   :condition (and (> (- ?y2 ?y1) 50) (< (random 1.0) 0.5))
                   :label "horizontal-split")
             ;; Vertical split rule - divides rectangle vertically  
             (rule (rectangle (?x1 ?y1) (?x2 ?y2))
                   ((rectangle (?x1 ?y1) ((+ ?x1 (/ (- ?x2 ?x1) 2)) ?y2))
                    (rectangle ((+ ?x1 (/ (- ?x2 ?x1) 2)) ?y1) (?x2 ?y2)))
                   :condition (and (> (- ?x2 ?x1) 50) (< (random 1.0) 0.5))
                   :label "vertical-split")))))

(defun create-recursive-grid-grammar ()
  "Create a grammar that generates a regular grid through recursive subdivision.
   
   Returns:
     A grammar object that can be used with interpret-grammar or generate-shapes
   
   This grammar starts with a single square and recursively subdivides it
   into four equal squares, creating a regular grid pattern."
  (parse-grammar
   '(grammar "recursive-grid"
            ;; Start with a single square
            (rectangle (100 100) (400 400))
            :rules
            ;; Quad split rule - divides square into four equal squares
            ((rule (rectangle (?x1 ?y1) (?x2 ?y2))
                   ((rectangle (?x1 ?y1) 
                             (+ ?x1 (/ (- ?x2 ?x1) 2)) 
                             (+ ?y1 (/ (- ?y2 ?y1) 2)))
                    (rectangle ((+ ?x1 (/ (- ?x2 ?x1) 2)) ?y1)
                             ?x2 
                             (+ ?y1 (/ (- ?y2 ?y1) 2)))
                    (rectangle (?x1 (+ ?y1 (/ (- ?y2 ?y1) 2)))
                             (+ ?x1 (/ (- ?x2 ?x1) 2))
                             ?y2)
                    (rectangle ((+ ?x1 (/ (- ?x2 ?x1) 2)) (+ ?y1 (/ (- ?y2 ?y1) 2)))
                             ?x2 ?y2))
                   :condition (and (> (- ?x2 ?x1) 30) (> (- ?y2 ?y1) 30))
                   :label "quad-split")))))

(defun create-dynamic-architecture-grammar ()
  "Create a grammar that generates an architectural-style subdivision pattern.
   
   Returns:
     A grammar object that can be used with interpret-grammar or generate-shapes
   
   This grammar starts with a rectangle and subdivides it using a mix of
   horizontal, vertical, and proportional splits, creating an interesting
   architectural floor plan or facade pattern."
  (parse-grammar
   '(grammar "architectural-subdivision"
            ;; Start with a large rectangle (building outline)
            (rectangle (50 50) (550 450))
            :rules
            ;; Split into two rectangles horizontally with golden ratio
            ((rule (rectangle (?x1 ?y1) (?x2 ?y2))
                   ((rectangle (?x1 ?y1) 
                             (+ ?x1 (* (- ?x2 ?x1) 0.618)) 
                             ?y2)
                    (rectangle ((+ ?x1 (* (- ?x2 ?x1) 0.618)) ?y1)
                             ?x2 
                             ?y2))
                   :condition (and (> (- ?x2 ?x1) 100) 
                                  (> (- ?x2 ?x1) (- ?y2 ?y1))
                                  (< (random 1.0) 0.4))
                   :label "golden-horizontal-split")
             ;; Split into two rectangles vertically with golden ratio
             (rule (rectangle (?x1 ?y1) (?x2 ?y2))
                   ((rectangle (?x1 ?y1) 
                             ?x2
                             (+ ?y1 (* (- ?y2 ?y1) 0.618)))
                    (rectangle (?x1 (+ ?y1 (* (- ?y2 ?y1) 0.618)))
                             ?x2 
                             ?y2))
                   :condition (and (> (- ?y2 ?y1) 100)
                                  (>= (- ?y2 ?y1) (- ?x2 ?x1))
                                  (< (random 1.0) 0.4))
                   :label "golden-vertical-split")
             ;; Split into three rectangles vertically (room divisions)
             (rule (rectangle (?x1 ?y1) (?x2 ?y2))
                   ((rectangle (?x1 ?y1) 
                             ?x2
                             (+ ?y1 (/ (- ?y2 ?y1) 3)))
                    (rectangle (?x1 (+ ?y1 (/ (- ?y2 ?y1) 3)))
                             ?x2 
                             (+ ?y1 (* 2 (/ (- ?y2 ?y1) 3))))
                    (rectangle (?x1 (+ ?y1 (* 2 (/ (- ?y2 ?y1) 3))))
                             ?x2 
                             ?y2))
                   :condition (and (> (- ?y2 ?y1) 150)
                                  (< (random 1.0) 0.3))
                   :label "triple-vertical-split")))))

(defun load-mondrian-pattern (iterations)
  "Generate a Mondrian-style subdivision pattern by applying the grammar for ITERATIONS steps.
   
   Parameters:
     ITERATIONS - Number of iterations to apply the grammar (4-8 recommended)
   
   Returns:
     A list of shapes representing the Mondrian-style pattern
   
   Example:
     (load-mondrian-pattern 6) ; Generate a Mondrian pattern with 6 iterations"
  (generate-shapes (create-mondrian-grammar) iterations))

(defun load-recursive-grid (iterations)
  "Generate a recursive grid pattern by applying the grammar for ITERATIONS steps.
   
   Parameters:
     ITERATIONS - Number of iterations to apply the grammar (3-5 recommended)
   
   Returns:
     A list of shapes representing the recursive grid pattern
   
   Example:
     (load-recursive-grid 4) ; Generate a recursive grid with 4 iterations"
  (generate-shapes (create-recursive-grid-grammar) iterations))

(defun load-architectural-pattern (iterations)
  "Generate an architectural subdivision pattern by applying the grammar for ITERATIONS steps.
   
   Parameters:
     ITERATIONS - Number of iterations to apply the grammar (4-7 recommended)
   
   Returns:
     A list of shapes representing the architectural subdivision pattern
   
   Example:
     (load-architectural-pattern 5) ; Generate an architectural pattern with 5 iterations"
  (generate-shapes (create-dynamic-architecture-grammar) iterations))

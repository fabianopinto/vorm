(in-package :vorm.examples)

;;;; Koch Curve Fractal Example
;;;; 
;;;; This example demonstrates how to create the Koch Curve fractal using shape grammars.
;;;; The Koch curve is a classic fractal that replaces each line segment with four
;;;; smaller segments, creating a snowflake-like pattern through iteration.

(defun create-koch-curve-grammar ()
  "Create a grammar that generates the Koch curve fractal.
   
   Returns:
     A grammar object that can be used with interpret-grammar or generate-shapes
   
   This grammar starts with a single horizontal line and transforms each line
   segment into four smaller connected line segments to form the characteristic
   Koch pattern."
  (parse-grammar
   '(grammar "koch-curve"
            ;; Start with a single horizontal line
            (line (100 250) (500 250))
            :rules
            ((rule (line (?x1 ?y1) (?x2 ?y2))
                   ;; Replace with 4 connected lines forming the Koch pattern
                   ((line (?x1 ?y1) (+ ?x1 (/ (- ?x2 ?x1) 3)) (+ ?y1 (/ (- ?y2 ?y1) 3)))
                    (line (+ ?x1 (/ (- ?x2 ?x1) 3)) (+ ?y1 (/ (- ?y2 ?y1) 3))
                          (+ ?x1 (/ (* 2 (- ?x2 ?x1)) 3)) (+ ?y1 (/ (* 2 (- ?y2 ?y1)) 3)))
                    (line (+ ?x1 (/ (* 2 (- ?x2 ?x1)) 3)) (+ ?y1 (/ (* 2 (- ?y2 ?y1)) 3))
                          (+ ?x2 (- ?x1 (/ (- ?x2 ?x1) 3))) (+ ?y2 (- ?y1 (/ (- ?y2 ?y1) 3))))
                    (line (+ ?x2 (- ?x1 (/ (- ?x2 ?x1) 3))) (+ ?y2 (- ?y1 (/ (- ?y2 ?y1) 3)))
                          (?x2 ?y2)))
                   :label "koch-generator")))))

(defun create-koch-snowflake-grammar ()
  "Create a grammar that generates the Koch snowflake fractal.
   
   Returns:
     A grammar object that can be used with interpret-grammar or generate-shapes
   
   This grammar starts with a triangle and transforms each line segment into four
   smaller connected line segments, eventually creating a snowflake-like pattern."
  (parse-grammar
   '(grammar "koch-snowflake"
            ;; Start with an equilateral triangle
            ((line (200 350) (400 100))
             (line (400 100) (600 350))
             (line (600 350) (200 350)))
            :rules
            ((rule (line (?x1 ?y1) (?x2 ?y2))
                   ;; Replace with 4 connected lines forming the Koch pattern
                   ((line (?x1 ?y1) (+ ?x1 (/ (- ?x2 ?x1) 3)) (+ ?y1 (/ (- ?y2 ?y1) 3)))
                    (line (+ ?x1 (/ (- ?x2 ?x1) 3)) (+ ?y1 (/ (- ?y2 ?y1) 3))
                          (+ ?x1 (/ (- ?x2 ?x1) 2) (* (/ (- ?y2 ?y1) 3) -0.866)) 
                          (+ ?y1 (/ (- ?y2 ?y1) 2) (* (/ (- ?x2 ?x1) 3) 0.866)))
                    (line (+ ?x1 (/ (- ?x2 ?x1) 2) (* (/ (- ?y2 ?y1) 3) -0.866))
                          (+ ?y1 (/ (- ?y2 ?y1) 2) (* (/ (- ?x2 ?x1) 3) 0.866))
                          (+ ?x1 (* 2 (/ (- ?x2 ?x1) 3))) (+ ?y1 (* 2 (/ (- ?y2 ?y1) 3))))
                    (line (+ ?x1 (* 2 (/ (- ?x2 ?x1) 3))) (+ ?y1 (* 2 (/ (- ?y2 ?y1) 3)))
                          (?x2 ?y2)))
                   :label "koch-generator")))))

(defun load-koch-curve (iterations)
  "Generate the Koch curve by applying the grammar for ITERATIONS steps.
   
   Parameters:
     ITERATIONS - Number of iterations to apply the grammar (4-7 recommended)
   
   Returns:
     A list of shapes representing the Koch curve fractal
   
   Example:
     (load-koch-curve 5) ; Generate a Koch curve with 5 iterations"
  (generate-shapes (create-koch-curve-grammar) iterations))

(defun load-koch-snowflake (iterations)
  "Generate the Koch snowflake by applying the grammar for ITERATIONS steps.
   
   Parameters:
     ITERATIONS - Number of iterations to apply the grammar (4-7 recommended)
   
   Returns:
     A list of shapes representing the Koch snowflake fractal
   
   Example:
     (load-koch-snowflake 4) ; Generate a Koch snowflake with 4 iterations"
  (generate-shapes (create-koch-snowflake-grammar) iterations))

(in-package :vorm.examples)

;;;; Sierpinski Triangle Fractal Example
;;;; 
;;;; This example demonstrates how to create the Sierpinski Triangle fractal using shape grammars.
;;;; The Sierpinski Triangle is a classic fractal that begins with a single triangle,
;;;; and recursively replaces it with three smaller triangles at the corners of the original.

(defun create-sierpinski-triangle-grammar ()
  "Create a grammar that generates the Sierpinski triangle fractal.
   
   Returns:
     A grammar object that can be used with interpret-grammar or generate-shapes
   
   This grammar starts with a single triangle and transforms it into three
   triangles positioned at the corners of the original triangle, leaving
   the center empty."
  (parse-grammar
   '(grammar "sierpinski-triangle"
            ;; Start with a single triangle
            (polygon ((300 100) (500 400) (100 400)))
            :rules
            ((rule (polygon ((?x1 ?y1) (?x2 ?y2) (?x3 ?y3)))
                   ;; Replace with 3 triangles at the corners
                   ((polygon ((?x1 ?y1) 
                             (+ ?x1 (/ (- ?x2 ?x1) 2)) (+ ?y1 (/ (- ?y2 ?y1) 2))
                             (+ ?x1 (/ (- ?x3 ?x1) 2)) (+ ?y1 (/ (- ?y3 ?y1) 2))))
                    (polygon ((+ ?x1 (/ (- ?x2 ?x1) 2)) (+ ?y1 (/ (- ?y2 ?y1) 2))
                             ?x2 ?y2 
                             (+ ?x2 (/ (- ?x3 ?x2) 2)) (+ ?y2 (/ (- ?y3 ?y2) 2))))
                    (polygon ((+ ?x1 (/ (- ?x3 ?x1) 2)) (+ ?y1 (/ (- ?y3 ?y1) 2))
                             (+ ?x2 (/ (- ?x3 ?x2) 2)) (+ ?y2 (/ (- ?y3 ?y2) 2))
                             ?x3 ?y3)))
                   :label "sierpinski-generator")))))

(defun create-sierpinski-gasket-grammar ()
  "Create a grammar that generates the Sierpinski gasket fractal.
   
   Returns:
     A grammar object that can be used with interpret-grammar or generate-shapes
   
   This variant starts with three triangles and recursively replaces the
   interior triangle with three smaller triangles. This approach gives a
   slightly different visual result from the standard Sierpinski triangle."
  (parse-grammar
   '(grammar "sierpinski-gasket"
            ;; Start with a triangle divided into four sub-triangles
            ((polygon ((200 400) (400 400) (300 228)))  ; Bottom triangle
             (polygon ((200 400) (300 228) (100 228)))  ; Left triangle
             (polygon ((400 400) (500 228) (300 228)))  ; Right triangle
             (polygon ((300 228) (500 228) (100 228)))) ; Top triangle
            :rules
            ;; Rule to replace the inner triangle (the last one)
            ((rule (polygon ((?x1 ?y1) (?x2 ?y2) (?x3 ?y3)))
                   ;; Calculate midpoints of original edges
                   ((polygon ((?x1 ?y1) 
                             (/ (+ ?x1 ?x2) 2) (/ (+ ?y1 ?y2) 2)
                             (/ (+ ?x1 ?x3) 2) (/ (+ ?y1 ?y3) 2)))
                    (polygon ((/ (+ ?x1 ?x2) 2) (/ (+ ?y1 ?y2) 2) 
                             ?x2 ?y2 
                             (/ (+ ?x2 ?x3) 2) (/ (+ ?y2 ?y3) 2)))
                    (polygon ((/ (+ ?x1 ?x3) 2) (/ (+ ?y1 ?y3) 2)
                             (/ (+ ?x2 ?x3) 2) (/ (+ ?y2 ?y3) 2)
                             ?x3 ?y3)))
                   :condition (and (> (- ?y1 ?y3) 5) (> (- ?x2 ?x1) 5))
                   :label "sierpinski-subdivision")))))

(defun load-sierpinski-triangle (iterations)
  "Generate the Sierpinski triangle by applying the grammar for ITERATIONS steps.
   
   Parameters:
     ITERATIONS - Number of iterations to apply the grammar (5-8 recommended)
   
   Returns:
     A list of shapes representing the Sierpinski triangle fractal
   
   Example:
     (load-sierpinski-triangle 6) ; Generate a Sierpinski triangle with 6 iterations"
  (generate-shapes (create-sierpinski-triangle-grammar) iterations))

(defun load-sierpinski-gasket (iterations)
  "Generate the Sierpinski gasket by applying the grammar for ITERATIONS steps.
   
   Parameters:
     ITERATIONS - Number of iterations to apply the grammar (4-6 recommended)
   
   Returns:
     A list of shapes representing the Sierpinski gasket fractal
   
   Example:
     (load-sierpinski-gasket 5) ; Generate a Sierpinski gasket with 5 iterations"
  (generate-shapes (create-sierpinski-gasket-grammar) iterations))

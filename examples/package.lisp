(defpackage :vorm.examples
  (:use :cl :vorm)
  (:documentation "Examples of shape grammars for the VORM system.
                   
                   This package provides practical demonstrations of shape grammar concepts
                   implemented in VORM, including fractal patterns (Koch curve, Sierpinski triangle),
                   subdivision techniques (Mondrian-style, architectural patterns), and L-system
                   based structures (plants, ferns, trees).
                   
                   Each example includes functions to create the grammar and to load/generate
                   the pattern with a specified number of iterations.")
  (:export :load-koch-curve
           :load-koch-snowflake
           :create-koch-curve-grammar
           :create-koch-snowflake-grammar
           :load-sierpinski-triangle
           :load-sierpinski-gasket
           :create-sierpinski-triangle-grammar
           :create-sierpinski-gasket-grammar
           :load-mondrian-pattern
           :load-recursive-grid
           :load-architectural-pattern
           :create-mondrian-grammar
           :create-recursive-grid-grammar
           :create-dynamic-architecture-grammar
           :load-plant-structure
           :load-fern-structure
           :load-tree-structure
           :create-plant-grammar
           :create-fern-grammar
           :create-tree-grammar))

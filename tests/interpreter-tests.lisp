(in-package :vorm.tests)

(def-suite interpreter-suite
  :description "Test suite for the grammar interpreter system"
  :in vorm-tests)

(in-suite interpreter-suite)

(test interpret-grammar
  "Test basic grammar interpretation"
  (let* ((result (interpret-grammar 
                  '(grammar "line-rotation"
                           (line (0 0) (10 0))
                           :rules ((rule (line (0 0) (10 0))
                                        (line (0 0) (0 10))
                                        :label "horizontal-to-vertical")))
                  1)))
    (is (= (length result) 1))
    (let ((shape (first result)))
      (is (typep shape 'line))
      (is (= (point-x (line-start shape)) 0))
      (is (= (point-y (line-start shape)) 0))
      (is (= (point-x (line-end shape)) 0))
      (is (= (point-y (line-end shape)) 10)))))

(test generate-shapes
  "Test generating shapes through grammar application"
  (let* ((grammar (parse-grammar 
                   '(grammar "circle-to-triangle"
                            (circle (50 50) 20)
                            :rules ((rule (circle (50 50) 20)
                                          (polygon (30 30) (70 30) (50 70))
                                          :label "circle-to-triangle")))))
         (result (generate-shapes grammar 1))
         (intermediate-results (generate-shapes grammar 1 :include-intermediates t)))
    ;; Check final result
    (is (= (length result) 1))
    (is (typep (first result) 'polygon))
    (is (= (length (polygon-vertices (first result))) 3)) ; Triangle
    
    ;; Check intermediate results
    (is (= (length intermediate-results) 2)) ; Initial + 1 step
    (is (= (length (first intermediate-results)) 1)) ; Initial state has 1 shape
    (is (= (length (second intermediate-results)) 1)) ; Final state has 1 shape
    
    ;; Check that the first state is a circle
    (is (typep (first (first intermediate-results)) 'circle))
    ;; Check that the last state is a triangle (3 vertices)
    (is (= (length (polygon-vertices (first (second intermediate-results)))) 3))))

(test trace-grammar-execution
  "Test tracing grammar execution steps"
  (let* ((grammar (parse-grammar 
                   '(grammar "rotations"
                            (line (0 0) (10 0))
                            :rules ((rule (line (0 0) (10 0))
                                         (line (0 0) (0 10))
                                         :label "horizontal-to-vertical")
                                   (rule (line (0 0) (0 10))
                                         (line (0 0) (-10 0))
                                         :label "vertical-to-horizontal-neg"))))))
    (multiple-value-bind (shapes trace) (trace-grammar-execution grammar 2)
      ;; Check final shapes
      (is (= (length shapes) 1))
      (let ((final-shape (first shapes)))
        (is (typep final-shape 'line))
        (is (approximately-equal (point-x (line-end final-shape)) -10 0.0001))
        (is (approximately-equal (point-y (line-end final-shape)) 0 0.0001)))
      
      ;; Check trace information
      (is (= (length trace) 3)) ; Initial + 2 steps
      (is (= (getf (first trace) :step) 0))
      (is (null (getf (first trace) :rule)))
      (is (= (getf (second trace) :step) 1))
      (is (string= (getf (second trace) :rule) "horizontal-to-vertical"))
      (is (= (getf (third trace) :step) 2))
      (is (string= (getf (third trace) :rule) "vertical-to-horizontal-neg")))))

(test interactive-grammar
  "Test interactive grammar execution"
  (let* ((grammar (parse-grammar 
                   '(grammar "interactive-test"
                            (line (0 0) (10 0))
                            :rules ((rule (line (0 0) (10 0))
                                         (line (0 0) (0 10))
                                         :label "horizontal-to-vertical")))))
         (initial-shapes (start-interactive-grammar grammar))
         (state1 (get-interactive-grammar-state)))
    
    ;; Check initial state
    (is (= (length initial-shapes) 1))
    (is (typep (first initial-shapes) 'line))
    (is (= (getf state1 :step) 0))
    (is (= (length (getf state1 :shapes)) 1))
    (is (= (length (getf state1 :history)) 1))
    
    ;; Take one step
    (let ((new-shapes (step-interactive-grammar))
          (state2 (get-interactive-grammar-state)))
      (is (= (length new-shapes) 1))
      (is (typep (first new-shapes) 'line))
      (is (= (point-x (line-end (first new-shapes))) 0))
      (is (= (point-y (line-end (first new-shapes))) 10))
      (is (= (getf state2 :step) 1))
      (is (= (length (getf state2 :history)) 2)))
    
    ;; Reset and check state
    (reset-interactive-grammar)
    (let ((state3 (get-interactive-grammar-state)))
      (is (= (getf state3 :step) 0))
      (is (= (length (getf state3 :shapes)) 1))
      (is (= (point-x (line-end (first (getf state3 :shapes)))) 10))
      (is (= (point-y (line-end (first (getf state3 :shapes)))) 0)))))

(in-package :vorm.tests)

(def-suite grammar-suite
  :description "Test suite for the grammar rules system"
  :in vorm-tests)

(in-suite grammar-suite)

(test rule-creation
  "Test creating a rule"
  (let* ((left (make-line 0 0 10 0))
         (right (make-line 0 0 0 10))
         (rule (make-rule left right :label "rotation-rule")))
    (is (eq (rule-left-side rule) left))
    (is (eq (rule-right-side rule) right))
    (is (string= (rule-label rule) "rotation-rule"))
    (is (= (rule-probability rule) 1.0))))

(test rule-probability
  "Test rule probability"
  (let* ((left (make-line 0 0 10 0))
         (right (make-line 0 0 0 10))
         (rule (make-rule left right :probability 0.5)))
    (is (= (rule-probability rule) 0.5))))

(test grammar-creation
  "Test creating a grammar"
  (let* ((axiom (make-line 0 0 10 0))
         (rule (make-rule (make-line 0 0 10 0) (make-line 0 0 0 10)))
         (grammar (make-grammar "test-grammar" axiom :rules (list rule))))
    (is (string= (grammar-name grammar) "test-grammar"))
    (is (eq (grammar-axiom grammar) axiom))
    (is (= (length (grammar-rules grammar)) 1))))

(test add-rule
  "Test adding a rule to a grammar"
  (let* ((axiom (make-line 0 0 10 0))
         (grammar (make-grammar "test-grammar" axiom))
         (rule1 (make-rule (make-line 0 0 10 0) (make-line 0 0 0 10)))
         (rule2 (make-rule (make-line 0 0 0 10) (make-line 0 0 -10 0))))
    (add-rule grammar rule1)
    (is (= (length (grammar-rules grammar)) 1))
    (add-rule grammar rule2)
    (is (= (length (grammar-rules grammar)) 2))))

(test match-shape-point
  "Test matching points"
  (let* ((pattern (make-point 10 20))
         (shape1 (make-point 10 20))
         (shape2 (make-point 10 30))
         (result1 (match-shape pattern shape1))
         (result2 (match-shape pattern shape2)))
    (is (not (null result1)) "match-shape should return non-nil for matching points")
    (is (null result2) "match-shape should return nil for non-matching points")))

(test match-shape-line
  "Test matching lines"
  (let* ((pattern (make-line 0 0 10 0))
         (shape1 (make-line 0 0 10 0))
         (shape2 (make-line 0 0 10 10))
         (result1 (match-shape pattern shape1))
         (result2 (match-shape pattern shape2)))
    (is (not (null result1)) "match-shape should return non-nil for matching lines")
    (is (null result2) "match-shape should return nil for non-matching lines")))

(test match-shape-variable
  "Test matching with variables"
  (let* ((pattern '?x)
         (shape (make-point 10 20))
         (bindings (match-shape pattern shape)))
    (is (not (null bindings)))
    (is (eq (cdr (assoc '?x bindings)) shape))))

(test match-shape-variable-consistency
  "Test variable consistency in matches"
  (let* ((p1 (make-point 10 20))
         (p2 (make-point 20 30))
         (pattern1 '?x)
         (pattern2 '?x)
         (bindings (match-shape pattern1 p1))
         (bindings2 (match-shape pattern2 p1 bindings))
         (bindings3 (match-shape pattern2 p2 bindings)))
    (is (not (null bindings2)))
    (is (null bindings3))))

(test apply-rule-basic
  "Test basic rule application"
  (let* ((left (make-line 0 0 10 0))
         (right (make-line 0 0 0 10))
         (rule (make-rule left right))
         (shape1 (make-line 0 0 10 0))
         (shape2 (make-line 0 0 10 10))
         (result1 (apply-rule rule shape1))
         (result2 (apply-rule rule shape2)))
    (is (not (null result1)))
    (is (= (length result1) 1))
    (let ((new-shape (first result1)))
      (is (typep new-shape 'line))
      (is (= (point-x (line-start new-shape)) 0))
      (is (= (point-y (line-start new-shape)) 0))
      (is (= (point-x (line-end new-shape)) 0))
      (is (= (point-y (line-end new-shape)) 10)))
    (is (null result2))))

(test apply-rule-with-transformation
  "Test rule application with transformation"
  (let* ((left (make-line 0 0 10 0))
         (right (make-line 0 0 0 10))
         (rule (make-rule left right))
         (shape (make-line 0 0 10 0))
         (trans (make-translation 5 5))
         (result (apply-rule rule shape trans)))
    (is (not (null result)))
    (let ((new-shape (first result)))
      (is (= (point-x (line-start new-shape)) 5))
      (is (= (point-y (line-start new-shape)) 5))
      (is (= (point-x (line-end new-shape)) 5))
      (is (= (point-y (line-end new-shape)) 15)))))

(test apply-rule-with-variables
  "Test rule application with variables"
  (let* ((left '?x)
         (right '?x)  ; identity rule
         (rule (make-rule left right))
         (shape (make-point 10 20))
         (result (apply-rule rule shape)))
    (is (not (null result)))
    (let ((new-shape (first result)))
      (is (typep new-shape 'point))
      (is (= (point-x new-shape) 10))
      (is (= (point-y new-shape) 20)))))

(test apply-grammar-step
  "Test applying one step of a grammar"
  (let* ((line (make-line 0 0 10 0))
         (rule (make-rule line (make-line 0 0 0 10)))
         (grammar (make-grammar "test" line :rules (list rule)))
         (shapes (list line))
         (new-shapes (apply-grammar-step grammar shapes)))
    (is (= (length new-shapes) 1))
    (let ((new-shape (first new-shapes)))
      (is (typep new-shape 'line))
      (is (= (point-x (line-end new-shape)) 0))
      (is (= (point-y (line-end new-shape)) 10)))))

(test apply-grammar
  "Test applying a complete grammar"
  (let* ((axiom (make-line 0 0 10 0))
         ;; A rule that rotates a horizontal line to vertical
         (rule1 (make-rule (make-line 0 0 10 0) 
                           (make-line 0 0 0 10) 
                           :label "horizontal-to-vertical"))
         ;; A rule that rotates a vertical line to horizontal (negative)
         (rule2 (make-rule (make-line 0 0 0 10) 
                           (make-line 0 0 -10 0) 
                           :label "vertical-to-horizontal"))
         ;; A rule that rotates a horizontal negative line to vertical negative
         (rule3 (make-rule (make-line 0 0 -10 0) 
                           (make-line 0 0 0 -10) 
                           :label "horizontal-neg-to-vertical-neg"))
         ;; A rule that rotates a vertical negative line back to horizontal positive
         (rule4 (make-rule (make-line 0 0 0 -10) 
                           (make-line 0 0 10 0) 
                           :label "vertical-neg-to-horizontal"))
         ;; Grammar that applies the rules in sequence
         (grammar (make-grammar "rotation-grammar" axiom 
                               :rules (list rule1 rule2 rule3 rule4)))
         ;; Run for 4 iterations to complete a cycle
         (result (apply-grammar grammar 4)))
    ;; After 4 iterations, we should be back to the original line
    (is (= (length result) 1))
    (let ((final-shape (first result)))
      (is (typep final-shape 'line))
      (is (approximately-equal (point-x (line-start final-shape)) 0 0.0001))
      (is (approximately-equal (point-y (line-start final-shape)) 0 0.0001))
      (is (approximately-equal (point-x (line-end final-shape)) 10 0.0001))
      (is (approximately-equal (point-y (line-end final-shape)) 0 0.0001)))))

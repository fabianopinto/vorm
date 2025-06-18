(in-package :vorm.tests)

(def-suite parser-suite
  :description "Test suite for the grammar parser system"
  :in vorm-tests)

(in-suite parser-suite)

(test parse-point
  "Test parsing a point expression"
  (let ((point (parse-shape '(point 10 20))))
    (is (typep point 'point))
    (is (= (point-x point) 10))
    (is (= (point-y point) 20))))

(test parse-line
  "Test parsing a line expression"
  (let ((line (parse-shape '(line (0 0) (10 10)))))
    (is (typep line 'line))
    (is (= (point-x (line-start line)) 0))
    (is (= (point-y (line-start line)) 0))
    (is (= (point-x (line-end line)) 10))
    (is (= (point-y (line-end line)) 10))))

(test parse-polygon
  "Test parsing a polygon expression"
  (let ((polygon (parse-shape '(polygon (0 0) (10 0) (10 10) (0 10)))))
    (is (typep polygon 'polygon))
    (is (= (length (polygon-vertices polygon)) 4))
    (let ((vertices (polygon-vertices polygon)))
      (is (= (point-x (first vertices)) 0))
      (is (= (point-y (first vertices)) 0))
      (is (= (point-x (second vertices)) 10))
      (is (= (point-y (second vertices)) 0))
      (is (= (point-x (third vertices)) 10))
      (is (= (point-y (third vertices)) 10))
      (is (= (point-x (fourth vertices)) 0))
      (is (= (point-y (fourth vertices)) 10)))))

(test parse-rectangle
  "Test parsing a rectangle expression"
  (let ((rect (parse-shape '(rectangle 0 0 10 20))))
    (is (typep rect 'polygon))
    (is (= (length (polygon-vertices rect)) 4))
    (let ((vertices (polygon-vertices rect)))
      (is (= (point-x (first vertices)) 0))
      (is (= (point-y (first vertices)) 0))
      (is (= (point-x (second vertices)) 10))
      (is (= (point-y (second vertices)) 0))
      (is (= (point-x (third vertices)) 10))
      (is (= (point-y (third vertices)) 20))
      (is (= (point-x (fourth vertices)) 0))
      (is (= (point-y (fourth vertices)) 20)))))

(test parse-circle
  "Test parsing a circle expression"
  (let ((circle (parse-shape '(circle (10 20) 5))))
    (is (typep circle 'circle))
    (is (= (point-x (circle-center circle)) 10))
    (is (= (point-y (circle-center circle)) 20))
    (is (= (circle-radius circle) 5))))

(test parse-translation
  "Test parsing a translation transformation"
  (let ((trans (parse-transformation '(translate 10 20))))
    (is (typep trans 'translation))
    (is (= (translation-dx trans) 10))
    (is (= (translation-dy trans) 20))))

(test parse-rotation
  "Test parsing a rotation transformation"
  (let ((rot (parse-transformation '(rotate 90 0 0))))
    (is (typep rot 'rotation))
    (is (= (rotation-angle rot) 90))
    (is (= (point-x (rotation-center rot)) 0))
    (is (= (point-y (rotation-center rot)) 0))))

(test parse-scaling
  "Test parsing a scaling transformation"
  (let ((scale (parse-transformation '(scale 2 3 0 0))))
    (is (typep scale 'scaling))
    (is (= (scaling-sx scale) 2))
    (is (= (scaling-sy scale) 3))
    (is (= (point-x (scaling-center scale)) 0))
    (is (= (point-y (scaling-center scale)) 0))))

(test parse-reflection
  "Test parsing a reflection transformation"
  (let ((refl (parse-transformation '(reflect 0 0 10 0))))
    (is (typep refl 'reflection))
    (is (= (point-x (line-start (reflection-line refl))) 0))
    (is (= (point-y (line-start (reflection-line refl))) 0))
    (is (= (point-x (line-end (reflection-line refl))) 10))
    (is (= (point-y (line-end (reflection-line refl))) 0))))

(test parse-composed-transformation
  "Test parsing a composed transformation"
  (let ((comp (parse-transformation '(compose (translate 10 0) (rotate 90 0 0)))))
    (is (typep comp 'composed-transformation))
    (let ((transforms (composed-transformation-transformations comp)))
      (is (= (length transforms) 2))
      (is (typep (first transforms) 'translation))
      (is (typep (second transforms) 'rotation)))))

(test parse-rule
  "Test parsing a rule"
  (let ((rule (parse-rule '(rule (line (0 0) (10 0)) 
                                      (line (0 0) (0 10))
                                      :label "horizontal-to-vertical"
                                      :probability 0.8))))
    (is (typep rule 'rule))
    (is (typep (rule-left-side rule) 'line))
    (is (typep (rule-right-side rule) 'line))
    (is (string= (rule-label rule) "horizontal-to-vertical"))
    (is (= (rule-probability rule) 0.8))))

(test parse-rule-with-variables
  "Test parsing a rule with variables"
  (let ((rule (parse-rule '(rule ?x ?x :label "identity-rule"))))
    (is (typep rule 'rule))
    (is (symbolp (rule-left-side rule)))
    (is (symbolp (rule-right-side rule)))
    (is (string= (rule-label rule) "identity-rule"))
    (is (eq (rule-left-side rule) (rule-right-side rule)))))

(test parse-grammar
  "Test parsing a complete grammar"
  (let ((grammar (parse-grammar '(grammar "square-to-triangle"
                                               (rectangle 0 0 10 10)
                                               :rules ((rule (rectangle 0 0 10 10)
                                                             (polygon (0 0) (10 0) (5 10))
                                                             :label "square-to-triangle"))))))
    (is (typep grammar 'grammar))
    (is (string= (grammar-name grammar) "square-to-triangle"))
    (is (typep (grammar-axiom grammar) 'polygon))
    (is (= (length (grammar-rules grammar)) 1))
    (is (string= (rule-label (first (grammar-rules grammar))) "square-to-triangle"))))

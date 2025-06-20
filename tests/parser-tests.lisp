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

;; Polygon parsing test removed in minimal geometry branch

;; Rectangle parsing test removed in minimal geometry branch

;; Circle parsing test removed in minimal geometry branch

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
  (let ((grammar (parse-grammar '(grammar "horizontal-to-vertical"
                                               (line (0 0) (10 0))
                                               :rules ((rule (line (0 0) (10 0))
                                                             (line (0 0) (0 10))
                                                             :label "horizontal-to-vertical"))))))
    (is (typep grammar 'grammar))
    (is (string= (grammar-name grammar) "horizontal-to-vertical"))
    (is (typep (grammar-axiom grammar) 'line))
    (is (= (length (grammar-rules grammar)) 1))
    (is (string= (rule-label (first (grammar-rules grammar))) "horizontal-to-vertical"))))

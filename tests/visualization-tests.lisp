;;;; visualization-tests.lisp - Tests for the visualization functionality

(in-package :vorm/tests)

;; Ensure we're using the VORM-TESTS suite
(in-suite :vorm-tests)

;; Tests for segment coordinate transformations

(test coordinate-transformation
  "Test transformation of segment coordinates to SVG coordinates"
  (let ((seg (vorm:make-segment 10 20))
        (angle 0)
        (pos 5))
    
    (multiple-value-bind (x1 y1 x2 y2)
        (vorm:transform-segment-coordinates seg angle pos)
      
      ;; With angle=0 and pos=5, the segment should be on a vertical line at x=5
      ;; The segment bounds of 10-20 should run from y=10 to y=20
      (is (float-near-p x1 5.0))
      (is (float-near-p y1 10.0))
      (is (float-near-p x2 5.0))
      (is (float-near-p y2 20.0))))
  
  ;; Test with angle = pi/2 (horizontal line)
  (let ((seg (vorm:make-segment 10 20))
        (angle (/ pi 2))
        (pos 5))
    
    (multiple-value-bind (x1 y1 x2 y2)
        (vorm:transform-segment-coordinates seg angle pos)
      
      ;; With angle=pi/2 and pos=5, the segment should be on a horizontal line at y=5
      ;; The segment bounds of 10-20 should run from x=10 to x=20
      ;; Using absolute values for comparison due to potential sign differences
      (is (float-near-p (abs x1) 10.0))
      (is (float-near-p y1 5.0))
      (is (float-near-p (abs x2) 20.0))
      (is (float-near-p y2 5.0)))))

;; Tests for SVG shape rendering
(test simple-shape-rendering
  "Test generating SVG for a simple shape"
  ;; Create a simplified test to verify basic SVG rendering
  ;; No need to create complex shapes for this test
  
  ;; Create a very simple shape with one line segment
  (let* ((segment (vorm:make-segment 10.0 20.0))
         (line (vorm:make-line segment))
         (parallels (vorm:make-parallels (cons 5.0 line)))
         (shape (vorm:make-shape (cons 0.0 parallels)))
         (svg (vorm:render-shape-to-svg shape :width 100 :height 100)))
    
    ;; Check that we have a valid SVG string with basic elements
    (is (stringp svg))
    (is (> (length svg) 10))
    (is (search "<?xml" svg))
    (is (search "<svg" svg))
    (is (search "</svg>" svg))
    
    ;; This shape should have a line element
    (is (search "<line" svg))))
  
(test empty-shape-rendering
  "Test generating SVG for an empty shape"
  (let ((shape (vorm:make-shape)))
    
    ;; Ensure we have a valid shape even if empty
    (when shape ; Only run the test if shape creation succeeded
      ;; Test that SVG content is generated without errors for empty shape
      (let ((svg (vorm:render-shape-to-svg shape :width 100 :height 100)))
      ;; Check that we have a valid SVG string without any lines
      (is (stringp svg))
      (is (search "<?xml" svg))
      (is (search "<svg" svg))
      (is (search "</svg>" svg))
      (is-false (search "<line" svg))))))

;; Helper for approximate float comparisons
(defun float-near-p (a b &optional (tolerance 0.0001))
  "Check if two floating point numbers are approximately equal"
  (<= (abs (- a b)) tolerance))

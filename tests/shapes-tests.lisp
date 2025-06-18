(in-package :vorm.tests)

(def-suite shapes-suite
  :description "Test suite for the shape representations"
  :in vorm-tests)

(in-suite shapes-suite)

(test point-creation
  "Test creating a point and checking its coordinates"
  (let ((p (make-point 10 20)))
    (is (= (point-x p) 10))
    (is (= (point-y p) 20))))

(test point-equality
  "Test point equality"
  (let ((p1 (make-point 10 20))
        (p2 (make-point 10 20))
        (p3 (make-point 10.5 20)))
    (is (shape-equals-p p1 p2))
    (is-false (shape-equals-p p1 p3))))

(test line-creation
  "Test creating a line and checking its properties"
  (let ((l (make-line 10 20 30 40)))
    (is (= (point-x (line-start l)) 10))
    (is (= (point-y (line-start l)) 20))
    (is (= (point-x (line-end l)) 30))
    (is (= (point-y (line-end l)) 40))))

(test line-length
  "Test line length calculation"
  (let ((l1 (make-line 0 0 3 4))
        (l2 (make-line 10 10 10 20)))
    (is (= (line-length l1) 5.0))
    (is (= (line-length l2) 10.0))))

(test polygon-creation
  "Test creating a polygon"
  (let ((poly (make-polygon '((0 0) (10 0) (10 10) (0 10)))))
    (is (= (length (polygon-vertices poly)) 4))
    (is (= (point-x (first (polygon-vertices poly))) 0))
    (is (= (point-y (first (polygon-vertices poly))) 0))))

(test rectangle-creation
  "Test creating a rectangle"
  (let ((rect (make-rectangle 0 0 10 20)))
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

(test circle-creation
  "Test creating a circle"
  (let ((c (make-circle 10 20 5)))
    (is (= (point-x (circle-center c)) 10))
    (is (= (point-y (circle-center c)) 20))
    (is (= (circle-radius c) 5))))

(test circle-area
  "Test circle area calculation"
  (let ((c (make-circle 0 0 2)))
    (is (approximately-equal (shape-area c) (* pi 4) 0.001))))

(test circle-contains-point
  "Test if circle contains a point"
  (let ((c (make-circle 0 0 5))
        (p1 (make-point 3 4))
        (p2 (make-point 6 0)))
    (is (shape-contains-p c p1))
    (is-false (shape-contains-p c p2))))

(test shape-bounds
  "Test shape bounds calculation"
  (let ((p (make-point 10 20))
        (l (make-line 0 0 10 10))
        (c (make-circle 5 5 5)))
    (multiple-value-bind (min-x min-y max-x max-y) (shape-bounds p)
      (is (= min-x 10))
      (is (= min-y 20))
      (is (= max-x 10))
      (is (= max-y 20)))
    (multiple-value-bind (min-x min-y max-x max-y) (shape-bounds l)
      (is (= min-x 0))
      (is (= min-y 0))
      (is (= max-x 10))
      (is (= max-y 10)))
    (multiple-value-bind (min-x min-y max-x max-y) (shape-bounds c)
      (is (= min-x 0))
      (is (= min-y 0))
      (is (= max-x 10))
      (is (= max-y 10)))))

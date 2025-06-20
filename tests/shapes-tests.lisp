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

;; Polygon tests removed in minimal geometry branch

;; Rectangle tests removed in minimal geometry branch

;; Circle tests removed in minimal geometry branch

(test shape-bounds
  "Test shape bounds calculation"
  (let ((p (make-point 10 20))
        (l (make-line 0 0 10 10)))
    (multiple-value-bind (min-x min-y max-x max-y) (shape-bounds p)
      (is (= min-x 10))
      (is (= min-y 20))
      (is (= max-x 10))
      (is (= max-y 20)))
    (multiple-value-bind (min-x min-y max-x max-y) (shape-bounds l)
      (is (= min-x 0))
      (is (= min-y 0))
      (is (= max-x 10))
      (is (= max-y 10)))))


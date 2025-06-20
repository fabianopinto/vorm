(in-package :vorm.tests)

(def-suite transformations-suite
  :description "Test suite for the shape transformations"
  :in vorm-tests)

(in-suite transformations-suite)

(test translation
  "Test translation transformation"
  (let* ((p (make-point 10 20))
         (trans (make-translation 5 -3))
         (result (apply-transformation trans p)))
    (is (= (point-x result) 15))
    (is (= (point-y result) 17))))

(test translation-line
  "Test translation of a line"
  (let* ((l (make-line 0 0 10 10))
         (trans (make-translation 5 5))
         (result (apply-transformation trans l)))
    (is (= (point-x (line-start result)) 5))
    (is (= (point-y (line-start result)) 5))
    (is (= (point-x (line-end result)) 15))
    (is (= (point-y (line-end result)) 15))))

;; Translation-polygon test removed in minimal geometry branch

;; Translation-circle test removed in minimal geometry branch

(test rotation-point
  "Test rotation of a point"
  (let* ((p (make-point 10 0))
         (rot (make-rotation 90 0 0))  ; 90 degrees around origin
         (result (apply-transformation rot p))
         (epsilon 0.0001))
    (is (approximately-equal (point-x result) 0 epsilon))
    (is (approximately-equal (point-y result) 10 epsilon))))

(test rotation-line
  "Test rotation of a line"
  (let* ((l (make-line 0 0 10 0))
         (rot (make-rotation 90 0 0))  ; 90 degrees around origin
         (result (apply-transformation rot l))
         (epsilon 0.0001))
    ;; First point should stay at origin
    (is (approximately-equal (point-x (line-start result)) 0 epsilon))
    (is (approximately-equal (point-y (line-start result)) 0 epsilon))
    ;; Second point should rotate to (0, 10)
    (is (approximately-equal (point-x (line-end result)) 0 epsilon))
    (is (approximately-equal (point-y (line-end result)) 10 epsilon))))

;; Rotation-polygon test removed in minimal geometry branch

(test scaling-point
  "Test scaling a point"
  (let* ((p (make-point 10 20))
         (scl (make-scaling 2 0.5 0 0))  ; 2x horizontally, 0.5x vertically from origin
         (result (apply-transformation scl p)))
    (is (= (point-x result) 20))
    (is (= (point-y result) 10))))

;; Scaling-circle test removed in minimal geometry branch

(test reflection
  "Test reflection transformation"
  (let* ((p (make-point 10 20))
         (refl (make-reflection 0 0 0 10))  ; Reflect across y-axis
         (result (apply-transformation refl p))
         (epsilon 0.0001))
    (is (approximately-equal (point-x result) -10 epsilon))
    (is (approximately-equal (point-y result) 20 epsilon))))

(test composed-transformation
  "Test composition of transformations"
  (let* ((p (make-point 10 0))
         (trans (make-translation 0 10))
         (rot (make-rotation 90 0 0))
         ;; First translate, then rotate
         (composed (compose-transformations trans rot))
         (result (apply-transformation composed p))
         (epsilon 0.0001))
    ;; Should result in point at (-10, 10)
    (is (approximately-equal (point-x result) -10 epsilon))
    (is (approximately-equal (point-y result) 10 epsilon))))

(test identity-transformation
  "Test identity transformation"
  (let* ((p (make-point 10 20))
         (id (identity-transformation))
         (result (apply-transformation id p)))
    (is (= (point-x result) 10))
    (is (= (point-y result) 20))))

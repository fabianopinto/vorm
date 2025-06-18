(in-package :vorm)

;;;; Transformations for the VORM shape grammar system

;;; Generic transformation class and methods
(defclass transformation ()
  ((name :initarg :name
         :accessor transformation-name
         :initform nil
         :documentation "Optional name for the transformation"))
  (:documentation "Base class for all transformations."))

(defgeneric apply-transformation (transformation shape)
  (:documentation "Apply TRANSFORMATION to SHAPE, returning a new shape."))

(defgeneric compose-transformations (transformation1 transformation2)
  (:documentation "Compose two transformations, returning a new transformation."))

;;; Translation transformation
(defclass translation (transformation)
  ((dx :initarg :dx
       :accessor translation-dx
       :initform 0
       :type number
       :documentation "Distance to translate along x-axis")
   (dy :initarg :dy
       :accessor translation-dy
       :initform 0
       :type number
       :documentation "Distance to translate along y-axis"))
  (:documentation "Translation transformation by (dx, dy)."))

(defun make-translation (dx dy &key name)
  "Create a translation transformation by DX along x-axis and DY along y-axis."
  (make-instance 'translation :dx dx :dy dy :name name))

(defmethod apply-transformation ((trans translation) (shape point))
  "Apply translation to a point."
  (make-point (+ (point-x shape) (translation-dx trans))
              (+ (point-y shape) (translation-dy trans))
              :id (shape-id shape)
              :metadata (shape-metadata shape)))

(defmethod apply-transformation ((trans translation) (shape line))
  "Apply translation to a line."
  (make-instance 'line
                 :start (apply-transformation trans (line-start shape))
                 :end (apply-transformation trans (line-end shape))
                 :id (shape-id shape)
                 :metadata (shape-metadata shape)))

(defmethod apply-transformation ((trans translation) (shape polygon))
  "Apply translation to a polygon."
  (make-instance 'polygon
                 :vertices (mapcar (lambda (v) (apply-transformation trans v))
                                   (polygon-vertices shape))
                 :id (shape-id shape)
                 :metadata (shape-metadata shape)))

(defmethod apply-transformation ((trans translation) (shape circle))
  "Apply translation to a circle."
  (make-instance 'circle
                 :center (apply-transformation trans (circle-center shape))
                 :radius (circle-radius shape)
                 :id (shape-id shape)
                 :metadata (shape-metadata shape)))

;;; Rotation transformation
(defclass rotation (transformation)
  ((angle :initarg :angle
          :accessor rotation-angle
          :type number
          :documentation "Rotation angle in degrees")
   (center :initarg :center
           :accessor rotation-center
           :type point
           :documentation "Center of rotation"))
  (:documentation "Rotation transformation around a center point by an angle."))

(defun make-rotation (angle center-x center-y &key name)
  "Create a rotation transformation of ANGLE degrees around center (CENTER-X, CENTER-Y)."
  (make-instance 'rotation
                 :angle angle
                 :center (make-point center-x center-y)
                 :name name))

(defmethod apply-transformation ((trans rotation) (shape point))
  "Apply rotation to a point."
  (let* ((angle-rad (degrees-to-radians (rotation-angle trans)))
         (cx (point-x (rotation-center trans)))
         (cy (point-y (rotation-center trans)))
         (dx (- (point-x shape) cx))
         (dy (- (point-y shape) cy))
         (cos-a (cos angle-rad))
         (sin-a (sin angle-rad))
         (new-x (+ cx (- (* dx cos-a) (* dy sin-a))))
         (new-y (+ cy (+ (* dx sin-a) (* dy cos-a)))))
    (make-point new-x new-y :id (shape-id shape) :metadata (shape-metadata shape))))

(defmethod apply-transformation ((trans rotation) (shape line))
  "Apply rotation to a line."
  (make-instance 'line
                 :start (apply-transformation trans (line-start shape))
                 :end (apply-transformation trans (line-end shape))
                 :id (shape-id shape)
                 :metadata (shape-metadata shape)))

(defmethod apply-transformation ((trans rotation) (shape polygon))
  "Apply rotation to a polygon."
  (make-instance 'polygon
                 :vertices (mapcar (lambda (v) (apply-transformation trans v))
                                   (polygon-vertices shape))
                 :id (shape-id shape)
                 :metadata (shape-metadata shape)))

(defmethod apply-transformation ((trans rotation) (shape circle))
  "Apply rotation to a circle."
  (make-instance 'circle
                 :center (apply-transformation trans (circle-center shape))
                 :radius (circle-radius shape)
                 :id (shape-id shape)
                 :metadata (shape-metadata shape)))

;;; Scaling transformation
(defclass scaling (transformation)
  ((sx :initarg :sx
       :accessor scaling-sx
       :initform 1.0
       :type number
       :documentation "Scale factor along x-axis")
   (sy :initarg :sy
       :accessor scaling-sy
       :initform 1.0
       :type number
       :documentation "Scale factor along y-axis")
   (center :initarg :center
           :accessor scaling-center
           :type point
           :documentation "Center of scaling"))
  (:documentation "Scaling transformation by factors sx and sy around a center point."))

(defun make-scaling (sx sy center-x center-y &key name)
  "Create a scaling transformation by factors SX and SY around center (CENTER-X, CENTER-Y)."
  (make-instance 'scaling
                 :sx sx
                 :sy sy
                 :center (make-point center-x center-y)
                 :name name))

(defmethod apply-transformation ((trans scaling) (shape point))
  "Apply scaling to a point."
  (let* ((cx (point-x (scaling-center trans)))
         (cy (point-y (scaling-center trans)))
         (dx (- (point-x shape) cx))
         (dy (- (point-y shape) cy))
         (new-x (+ cx (* dx (scaling-sx trans))))
         (new-y (+ cy (* dy (scaling-sy trans)))))
    (make-point new-x new-y :id (shape-id shape) :metadata (shape-metadata shape))))

(defmethod apply-transformation ((trans scaling) (shape line))
  "Apply scaling to a line."
  (make-instance 'line
                 :start (apply-transformation trans (line-start shape))
                 :end (apply-transformation trans (line-end shape))
                 :id (shape-id shape)
                 :metadata (shape-metadata shape)))

(defmethod apply-transformation ((trans scaling) (shape polygon))
  "Apply scaling to a polygon."
  (make-instance 'polygon
                 :vertices (mapcar (lambda (v) (apply-transformation trans v))
                                   (polygon-vertices shape))
                 :id (shape-id shape)
                 :metadata (shape-metadata shape)))

(defmethod apply-transformation ((trans scaling) (shape circle))
  "Apply scaling to a circle. Note: Non-uniform scaling converts circle to ellipse, 
   but we approximate it as a circle with the average scaling factor."
  (let* ((sx (scaling-sx trans))
         (sy (scaling-sy trans))
         (avg-scale (/ (+ sx sy) 2.0)))
    (make-instance 'circle
                   :center (apply-transformation trans (circle-center shape))
                   :radius (* (circle-radius shape) avg-scale)
                   :id (shape-id shape)
                   :metadata (shape-metadata shape))))

;;; Reflection transformation
(defclass reflection (transformation)
  ((line :initarg :line
         :accessor reflection-line
         :type line
         :documentation "Line to reflect across"))
  (:documentation "Reflection transformation across a line."))

(defun make-reflection (line-start-x line-start-y line-end-x line-end-y &key name)
  "Create a reflection transformation across the line from 
   (LINE-START-X, LINE-START-Y) to (LINE-END-X, LINE-END-Y)."
  (make-instance 'reflection
                 :line (make-line line-start-x line-start-y line-end-x line-end-y)
                 :name name))

(defmethod apply-transformation ((trans reflection) (shape point))
  "Apply reflection to a point."
  ;; Implementation of point reflection across a line
  (let* ((line (reflection-line trans))
         (x (point-x shape))
         (y (point-y shape))
         (x1 (point-x (line-start line)))
         (y1 (point-y (line-start line)))
         (x2 (point-x (line-end line)))
         (y2 (point-y (line-end line)))
         ;; Vector along the reflection line
         (dx (- x2 x1))
         (dy (- y2 y1))
         ;; Length squared of the line segment
         (len-squared (+ (* dx dx) (* dy dy)))
         ;; Avoid division by zero
         (len-squared (if (= len-squared 0.0) 1.0 len-squared))
         ;; Project point onto the line
         (t-param (/ (+ (* (- x x1) dx) (* (- y y1) dy)) len-squared))
         ;; Find the projection point
         (proj-x (+ x1 (* t-param dx)))
         (proj-y (+ y1 (* t-param dy)))
         ;; Reflect through projection point
         (new-x (+ proj-x (- proj-x x)))
         (new-y (+ proj-y (- proj-y y))))
    (make-point new-x new-y :id (shape-id shape) :metadata (shape-metadata shape))))

(defmethod apply-transformation ((trans reflection) (shape line))
  "Apply reflection to a line."
  (make-instance 'line
                 :start (apply-transformation trans (line-start shape))
                 :end (apply-transformation trans (line-end shape))
                 :id (shape-id shape)
                 :metadata (shape-metadata shape)))

(defmethod apply-transformation ((trans reflection) (shape polygon))
  "Apply reflection to a polygon."
  (make-instance 'polygon
                 :vertices (mapcar (lambda (v) (apply-transformation trans v))
                                   (polygon-vertices shape))
                 :id (shape-id shape)
                 :metadata (shape-metadata shape)))

(defmethod apply-transformation ((trans reflection) (shape circle))
  "Apply reflection to a circle."
  (make-instance 'circle
                 :center (apply-transformation trans (circle-center shape))
                 :radius (circle-radius shape)
                 :id (shape-id shape)
                 :metadata (shape-metadata shape)))

;;; Composed transformation
(defclass composed-transformation (transformation)
  ((transformations :initarg :transformations
                   :accessor composed-transformation-transformations
                   :type list
                   :documentation "List of transformations to apply in sequence"))
  (:documentation "A composition of multiple transformations applied in sequence."))

(defun identity-transformation (&key name)
  "Create an identity transformation that doesn't change shapes."
  (make-instance 'composed-transformation
                 :transformations nil
                 :name (or name "Identity")))

(defmethod compose-transformations ((t1 transformation) (t2 transformation))
  "Compose two transformations."
  (make-instance 'composed-transformation
                 :transformations (list t1 t2)))

(defmethod compose-transformations ((t1 composed-transformation) (t2 transformation))
  "Compose a composed transformation with another transformation."
  (make-instance 'composed-transformation
                 :transformations (append (composed-transformation-transformations t1)
                                         (list t2))))

(defmethod compose-transformations ((t1 transformation) (t2 composed-transformation))
  "Compose a transformation with a composed transformation."
  (make-instance 'composed-transformation
                 :transformations (cons t1 (composed-transformation-transformations t2))))

(defmethod compose-transformations ((t1 composed-transformation) (t2 composed-transformation))
  "Compose two composed transformations."
  (make-instance 'composed-transformation
                 :transformations (append (composed-transformation-transformations t1)
                                         (composed-transformation-transformations t2))))

(defmethod apply-transformation ((trans composed-transformation) shape)
  "Apply composed transformation to a shape."
  (if (null (composed-transformation-transformations trans))
      ;; Identity transformation
      shape
      ;; Apply transformations in sequence
      (let ((result shape))
        (dolist (trans-item (composed-transformation-transformations trans))
          (setf result (apply-transformation trans-item result)))
        result)))

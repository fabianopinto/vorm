(in-package :vorm)

;;;; Shape definitions for the VORM shape grammar system

;;; Generic shape class and methods
(defclass shape ()
  ((id :initarg :id
       :accessor shape-id
       :initform (gensym "SHAPE-")
       :documentation "Unique identifier for the shape")
   (metadata :initarg :metadata
             :accessor shape-metadata
             :initform nil
             :documentation "Additional metadata for the shape"))
  (:documentation "Base class for all shapes in the system."))

(defgeneric shape-contains-p (shape point)
  (:documentation "Returns true if SHAPE contains POINT."))

(defgeneric shape-intersects-p (shape1 shape2)
  (:documentation "Returns true if SHAPE1 intersects with SHAPE2."))

(defgeneric shape-equals-p (shape1 shape2)
  (:documentation "Returns true if SHAPE1 equals SHAPE2 (same type and same parameters)."))

(defgeneric shape-area (shape)
  (:documentation "Returns the area of SHAPE."))

(defgeneric shape-perimeter (shape)
  (:documentation "Returns the perimeter of SHAPE."))

(defgeneric shape-bounds (shape)
  (:documentation "Returns the bounding box of SHAPE as (min-x min-y max-x max-y)."))

;;; Point shape
(defclass point (shape)
  ((x :initarg :x
      :accessor point-x
      :type number
      :documentation "X coordinate of the point")
   (y :initarg :y
      :accessor point-y
      :type number
      :documentation "Y coordinate of the point"))
  (:documentation "Represents a 2D point."))

(defun make-point (x y &key id metadata)
  "Create a point at coordinates (X, Y)."
  (make-instance 'point :x x :y y :id id :metadata metadata))

(defmethod shape-contains-p ((shape point) (pt point))
  (and (almost-equal (point-x shape) (point-x pt))
       (almost-equal (point-y shape) (point-y pt))))

(defmethod shape-equals-p ((shape1 point) (shape2 point))
  (shape-contains-p shape1 shape2))

(defmethod shape-area ((shape point))
  0)

(defmethod shape-perimeter ((shape point))
  0)

(defmethod shape-bounds ((shape point))
  (let ((x (point-x shape))
        (y (point-y shape)))
    (values x y x y)))

;;; Line shape
(defclass line (shape)
  ((start :initarg :start
          :accessor line-start
          :type point
          :documentation "Start point of the line")
   (end :initarg :end
        :accessor line-end
        :type point
        :documentation "End point of the line"))
  (:documentation "Represents a line segment between two points."))

(defun make-line (start-x start-y end-x end-y &key id metadata)
  "Create a line from (START-X, START-Y) to (END-X, END-Y)."
  (make-instance 'line
                 :start (make-point start-x start-y)
                 :end (make-point end-x end-y)
                 :id id
                 :metadata metadata))

(defun line-length (line)
  "Calculate the length of LINE."
  (point-distance (point-x (line-start line))
                  (point-y (line-start line))
                  (point-x (line-end line))
                  (point-y (line-end line))))

(defmethod shape-perimeter ((shape line))
  (line-length shape))

(defmethod shape-area ((shape line))
  0)

(defmethod shape-bounds ((shape line))
  (let ((x1 (point-x (line-start shape)))
        (y1 (point-y (line-start shape)))
        (x2 (point-x (line-end shape)))
        (y2 (point-y (line-end shape))))
    (values (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2))))

;;; Polygon shape
(defclass polygon (shape)
  ((vertices :initarg :vertices
             :accessor polygon-vertices
             :type list
             :documentation "List of points forming the polygon"))
  (:documentation "Represents a polygon defined by a list of vertices."))

(defun make-polygon (points &key id metadata)
  "Create a polygon from a list of points. POINTS is a list of (X Y) pairs."
  (make-instance 'polygon
                 :vertices (mapcar (lambda (p) 
                                     (if (typep p 'point)
                                         p
                                         (make-point (first p) (second p))))
                                   points)
                 :id id
                 :metadata metadata))

(defun make-rectangle (x y width height &key id metadata)
  "Create a rectangle with top-left corner at (X, Y) and given WIDTH and HEIGHT."
  (make-polygon (list (list x y)
                      (list (+ x width) y)
                      (list (+ x width) (+ y height))
                      (list x (+ y height)))
               :id id :metadata metadata))

;;; Circle shape
(defclass circle (shape)
  ((center :initarg :center
           :accessor circle-center
           :type point
           :documentation "Center point of the circle")
   (radius :initarg :radius
           :accessor circle-radius
           :type number
           :documentation "Radius of the circle"))
  (:documentation "Represents a circle with a center and radius."))

(defun make-circle (center-x center-y radius &key id metadata)
  "Create a circle with center at (CENTER-X, CENTER-Y) and given RADIUS."
  (make-instance 'circle
                 :center (make-point center-x center-y)
                 :radius radius
                 :id id
                 :metadata metadata))

(defmethod shape-area ((shape circle))
  (* pi (expt (circle-radius shape) 2)))

(defmethod shape-perimeter ((shape circle))
  (* 2 pi (circle-radius shape)))

(defmethod shape-bounds ((shape circle))
  (let ((x (point-x (circle-center shape)))
        (y (point-y (circle-center shape)))
        (r (circle-radius shape)))
    (values (- x r) (- y r) (+ x r) (+ y r))))

(defmethod shape-contains-p ((shape circle) (pt point))
  (<= (point-distance (point-x (circle-center shape))
                      (point-y (circle-center shape))
                      (point-x pt)
                      (point-y pt))
      (circle-radius shape)))

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
  "Create a point at coordinates (X, Y).
   
   Parameters:
     X - The x-coordinate (horizontal position)
     Y - The y-coordinate (vertical position)
     ID - Optional unique identifier for the point
     METADATA - Optional additional data associated with the point
   
   Returns:
     A new point instance at the specified coordinates
   
   Example:
     (make-point 10 20) ; Creates a point at (10, 20)
   
   See also:
     POINT-X - Accessor for the x-coordinate
     POINT-Y - Accessor for the y-coordinate
     PARSE-POINT - Creates a point from an s-expression"
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
  "Create a line from (START-X, START-Y) to (END-X, END-Y).
   
   Parameters:
     START-X - X coordinate of the line's start point
     START-Y - Y coordinate of the line's start point
     END-X - X coordinate of the line's end point
     END-Y - Y coordinate of the line's end point
     ID - Optional unique identifier for the line
     METADATA - Optional additional data associated with the line
   
   Returns:
     A new line instance between the specified points
   
   Example:
     (make-line 10 20 30 40) ; Creates a line from (10, 20) to (30, 40)"
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
  "Create a polygon from a list of points.
   
   Parameters:
     POINTS - A list of (X Y) pairs or point objects defining the vertices
     ID - Optional unique identifier for the polygon
     METADATA - Optional additional data associated with the polygon
   
   Returns:
     A new polygon instance with the specified vertices
   
   Example:
     (make-polygon '((10 20) (30 40) (50 20))) ; Creates a triangle"
  (make-instance 'polygon
                 :vertices (mapcar (lambda (p) 
                                     (if (typep p 'point)
                                         p
                                         (make-point (first p) (second p))))
                                   points)
                 :id id
                 :metadata metadata))

(defun make-rectangle (x y width height &key id metadata)
  "Create a rectangle with top-left corner at (X, Y) and given WIDTH and HEIGHT.
   
   Parameters:
     X - X coordinate of the top-left corner
     Y - Y coordinate of the top-left corner
     WIDTH - Width of the rectangle (horizontal size)
     HEIGHT - Height of the rectangle (vertical size)
     ID - Optional unique identifier for the rectangle
     METADATA - Optional additional data associated with the rectangle
   
   Returns:
     A new polygon instance in a rectangular shape
   
   Example:
     (make-rectangle 10 20 50 30) ; Creates a 50x30 rectangle at (10, 20)
   
   See also:
     MAKE-POLYGON - For creating general polygons
     PARSE-RECTANGLE - Creates a rectangle from an s-expression
     POLYGON-VERTICES - Accessor for the rectangle's vertices
     SHAPE-BOUNDS - Gets the bounding box of a shape"
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
  "Create a circle with center at (CENTER-X, CENTER-Y) and given RADIUS.
   
   Parameters:
     CENTER-X - X coordinate of the circle's center
     CENTER-Y - Y coordinate of the circle's center
     RADIUS - Radius of the circle
     ID - Optional unique identifier for the circle
     METADATA - Optional additional data associated with the circle
   
   Returns:
     A new circle instance with the specified center and radius
   
   Example:
     (make-circle 50 50 20) ; Creates a circle at (50, 50) with radius 20
   
   See also:
     CIRCLE-CENTER - Accessor for the center point
     CIRCLE-RADIUS - Accessor for the radius
     PARSE-CIRCLE - Creates a circle from an s-expression
     SHAPE-CONTAINS-P - Tests if a point is inside a shape"
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

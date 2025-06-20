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

;; In minimal geometry branch, we only keep the essential functions
;; for points and lines. The complex geometry functions have been removed.

(defgeneric shape-equals-p (shape1 shape2)
  (:documentation "Returns true if SHAPE1 equals SHAPE2 (same type and same parameters)."))

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

;; make-point function removed in minimal geometry branch

(defmethod shape-equals-p ((shape1 point) (shape2 point))
  (and (almost-equal (point-x shape1) (point-x shape2))
       (almost-equal (point-y shape1) (point-y shape2))))

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

;; line-length function removed in minimal geometry branch

(defmethod shape-equals-p ((shape1 line) (shape2 line))
  (or (and (shape-equals-p (line-start shape1) (line-start shape2))
           (shape-equals-p (line-end shape1) (line-end shape2)))
      (and (shape-equals-p (line-start shape1) (line-end shape2))
           (shape-equals-p (line-end shape1) (line-start shape2)))))

;;; Polygon shape
;; Polygon class removed in minimal geometry branch

;; Rectangle function removed in minimal geometry branch

;;; No other shapes in minimal geometry branch
;; Circle class removed in minimal geometry branch

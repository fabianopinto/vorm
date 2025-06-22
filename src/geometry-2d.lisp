;;;; geometry-2d.lisp - Two-dimensional geometric primitives for the VORM system

(in-package :vorm)

;;;-----------------------------------------------------------------------------
;;; Parallels Structure
;;;-----------------------------------------------------------------------------
;;; A structure representing a set of parallel lines distributed along an orthogonal axis.
;;; Each line is positioned at a specific location in the second dimension.

(defstruct (parallels (:constructor make-parallels-internal))
  "A set of parallel lines distributed along real positions of an orthogonal axis.
   Each line is associated with a unique position value representing its location on the
   orthogonal axis. No two lines can be positioned closer than *LINEAR-TOLERANCE* to each other."
  (lines (make-hash-table :test #'equal) :type hash-table))

(defun find-nearest-position (position positions)
  "Find the position in the list that is closest to the given position and within *LINEAR-TOLERANCE*.
   
   Parameters:
   - position: The reference position to compare against
   - positions: A list of positions to search through
   
   Returns:
   - The position with the minimum distance to the reference position, or nil if no position is within tolerance"
  (when positions
    (let ((min-distance nil)
          (min-position nil))
      (dolist (pos positions)
        (let ((distance (abs (- position pos))))
          (when (or (null min-distance) (< distance min-distance))
            (setf min-distance distance)
            (setf min-position pos))))
      (if (<= min-distance *linear-tolerance*)
          min-position
          nil))))

(defun make-parallels (&rest position-line-pairs)
  "Create a new parallels structure with optional position-line pairs.
   
   Parameters:
   - position-line-pairs: Any number of cons cells where each car is a position
                        and each cdr is a line
   
   Returns:
   - A new parallels structure
   
   Notes:
   - When adding lines at positions closer than *LINEAR-TOLERANCE* to existing positions,
     the lines will be merged instead of signaling errors
   - Empty lines (lines without any segments) are automatically discarded
   - If merging lines results in an empty line, that position is removed
   
   Usage examples:
   (make-parallels)                                    ;; Create empty parallels
   (make-parallels (cons 1.0 line1) (cons 2.0 line2)) ;; Create with direct position-line pairs"
  ;; Create an empty parallels structure
  (let ((p (make-parallels-internal)))
    (setf (parallels-lines p) (make-hash-table :test #'equal))
    
    ;; Add initial lines if provided, filtering out any empty lines
    (when position-line-pairs
      ;; Filter out empty lines
      (let ((filtered-pairs (remove-if (lambda (pair) (line-empty-p (cdr pair)))
                                      position-line-pairs)))
        ;; Only process if we have non-empty lines
        (when filtered-pairs
          (apply #'parallels-add-lines p filtered-pairs))))
    
    p))

(defun parallels-add-lines (parallels &rest position-line-pairs)
  "Add multiple lines at specified positions in the parallels structure.
   
   Parameters:
   - parallels: The parallels structure to modify
   - position-line-pairs: Any number of cons cells where each car is a position
                         and each cdr is a line
   
   Returns:
   - The modified parallels structure
   
   Behavior:
   - When a new line's position is within *LINEAR-TOLERANCE* of an existing position,
     the new line is merged with the existing line at that position
   - Lines at positions further apart than *LINEAR-TOLERANCE* are added separately
   - Empty lines (lines without any segments) are automatically discarded
   - If merging lines results in an empty line, that position is removed from the structure
   
   Usage examples:
   (parallels-add-lines p (cons 1.0 line1))                   ;; Add a single line
   (parallels-add-lines p (cons 1.0 line1) (cons 2.0 line2)) ;; Add multiple lines"
  (declare (type parallels parallels))
  
  (when position-line-pairs
    ;; Filter out any empty lines first
    (let ((filtered-pairs (remove-if (lambda (pair) (line-empty-p (cdr pair)))
                                    position-line-pairs)))
      
      ;; If we have non-empty lines to process
      (when filtered-pairs
        ;; Get existing positions
        (let ((positions (parallels-positions parallels)))
          ;; First pass: validate all positions
          (dolist (pos-line-pair filtered-pairs)
            (destructuring-bind (pos . line) pos-line-pair
              (declare (type real pos))
              ;; Check against existing positions
              (let ((existing-pos (find-nearest-position pos positions)))
                (when existing-pos
                  (let* ((existing-line (gethash existing-pos (parallels-lines parallels)))
                         (merged-line (lines-merge line existing-line)))
                    ;; Only store the result if not empty
                    (if (line-empty-p merged-line)
                        (remhash existing-pos (parallels-lines parallels))
                        (setf (gethash existing-pos (parallels-lines parallels)) merged-line)))))))
          
          ;; Second pass: add lines that don't conflict with existing positions
          (dolist (pos-line-pair filtered-pairs)
            (destructuring-bind (pos . line) pos-line-pair
              (unless (find-nearest-position pos positions)
                (setf (gethash pos (parallels-lines parallels)) line)))))))
  
  parallels))

(defun parallels-positions (parallels)
  "Get a list of all positions in the parallels structure that have lines.
   The positions are returned in ascending order.
   
   Parameters:
   - parallels: The parallels structure to query
   
   Returns:
   - A sorted list of positions"
  (declare (type parallels parallels))
  (let ((positions nil))
    (maphash (lambda (pos line)
               (declare (ignore line))
               (push pos positions))
             (parallels-lines parallels))
    (sort positions #'<)))

;;;-----------------------------------------------------------------------------
;;; Shape Structure
;;;-----------------------------------------------------------------------------
;;; A structure representing a shape composed of parallel lines at different angles.

(defstruct (shape (:constructor make-shape-internal))
  "A representation of a shape as a collection of angle-parallels pairs.
   Each angle corresponds to a set of parallel lines (a parallels structure) at that orientation.
   
   The shape is represented as sorted by angle in ascending order."
  (angle-parallels (make-hash-table :test #'equal) :type hash-table))

(defun make-shape (&rest angle-parallels-pairs)
  "Create a new shape with optional angle-parallels pairs.
   
   Parameters:
   - angle-parallels-pairs: Any number of cons cells where each car is an angle
                           (in radians) and each cdr is a parallels structure
   
   Returns:
   - A new shape structure
   
   Notes:
   - Angles are normalized to the range [0, 2π)
   - When adding parallels at angles closer than *ANGULAR-TOLERANCE* to existing angles,
     the parallels will be merged
   - Empty parallels (with no lines) are automatically discarded
   
   Usage examples:
   (make-shape)                                          ;; Create empty shape
   (make-shape (cons 0.0 parallels1) (cons pi parallels2)) ;; Create with direct angle-parallels pairs"
  ;; Create an empty shape structure
  (let ((shape (make-shape-internal)))
    (setf (shape-angle-parallels shape) (make-hash-table :test #'equal))
    
    ;; Add initial angle-parallels pairs if provided, filtering out empty parallels
    (when angle-parallels-pairs
      ;; Filter out pairs with empty parallels
      (let ((filtered-pairs (remove-if 
                              (lambda (pair)
                                (let ((parallels (cdr pair)))
                                  (zerop (hash-table-count (parallels-lines parallels)))))
                              angle-parallels-pairs)))
        
        ;; Add each pair if we have non-empty parallels
        (when filtered-pairs
          (dolist (pair filtered-pairs)
            (destructuring-bind (angle . parallels) pair
              (shape-add-parallels shape angle parallels))))))
    
    ;; Return the shape object
    shape))

(defun shape-angles (shape)
  "Get a list of all angles in the shape that have parallels.
   The angles are returned in ascending order after normalization.
   
   Parameters:
   - shape: The shape structure to query
   
   Returns:
   - A sorted list of angles"
  (declare (type shape shape))
  (let ((angles nil))
    (maphash (lambda (angle parallels)
               (declare (ignore parallels))
               (push angle angles))
             (shape-angle-parallels shape))
    (sort angles #'<)))

(defun find-nearest-angle (angle angles)
  "Find the angle in the list that is closest to the given angle and within *ANGULAR-TOLERANCE*.
   
   Parameters:
   - angle: The reference angle to compare against (in radians)
   - angles: A list of angles to search through
   
   Returns:
   - The angle with the minimum angular distance to the reference angle,
     or nil if no angle is within tolerance"
  (when angles
    (let ((min-distance nil)
          (min-angle nil))
      (dolist (ang angles)
        (let* ((normalized-angle (normalize-angle angle))
               (normalized-ang (normalize-angle ang))
               ;; Calculate the shortest angular distance considering the circle
               (distance (min (abs (- normalized-angle normalized-ang))
                             (abs (- (+ normalized-angle +two-pi+) normalized-ang))
                             (abs (- normalized-angle (+ normalized-ang +two-pi+))))))
          (when (or (null min-distance) (< distance min-distance))
            (setf min-distance distance)
            (setf min-angle ang))))
      (if (<= min-distance *angular-tolerance*)
          min-angle
          nil))))

(defun shape-add-parallels (shape angle parallels)
  "Add a parallels structure at the specified angle to the shape.
   If parallels already exist at an angle within *ANGULAR-TOLERANCE* of the specified angle,
   the parallels are merged by adding all lines from the new parallels to the existing one.
   
   Parameters:
   - shape: The shape structure to modify
   - angle: The angle (in radians) at which to add the parallels
   - parallels: The parallels structure to add
   
   Returns:
   - The modified shape structure
   
   Notes:
   - The angle is normalized to [0, 2π)
   - Empty parallels (with no lines) are not added
   - If a parallels structure already exists at an angle within *ANGULAR-TOLERANCE*,
     the lines from the new parallels are added to the existing parallels"
  (declare (type shape shape))
  (declare (type real angle))
  (declare (type parallels parallels))
  
  ;; Don't add empty parallels
  (unless (zerop (hash-table-count (parallels-lines parallels)))
    (let* ((normalized-angle (normalize-angle angle))
           (angles (shape-angles shape))
           (existing-angle (find-nearest-angle normalized-angle angles)))
      
      (if existing-angle
          ;; If we have parallels at this angle already (within tolerance), merge them
          (let ((existing-parallels (gethash existing-angle (shape-angle-parallels shape))))
            ;; For each line in the new parallels, add it to the existing parallels
            (maphash (lambda (pos line)
                       (apply #'parallels-add-lines existing-parallels (cons pos line)))
                     (parallels-lines parallels)))
          
          ;; Otherwise add the new parallels at the normalized angle
          (setf (gethash normalized-angle (shape-angle-parallels shape)) parallels))))
  
  shape)

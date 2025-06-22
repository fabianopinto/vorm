(in-package :vorm)

;;;-----------------------------------------------------------------------------
;;; Segment Structure
;;;-----------------------------------------------------------------------------
;;; A segment represents an interval on the real line, with a lower and upper limit.

(defstruct (segment 
            (:constructor make-segment-internal (lower upper)))
  "A segment in one dimension, represented by lower and upper limits as real numbers.
   Lower limit must be strictly less than the upper limit (not equal within tolerance)."
  (lower 0.0 :type real)
  (upper 0.0 :type real))

(defun make-segment (lower upper)
  "Create a segment with validation to prevent degenerate or invalid segments.
   Signals an error if: 
   - The segment would be degenerate (equal limits within tolerance)
   - The lower bound is greater than the upper bound"
  (declare (type real lower upper))
  (when (linear-equal lower upper)
    (error "Cannot create degenerate segment: lower (~a) and upper (~a) are equal within tolerance."
           lower upper))
  (when (> lower upper)
    (error "Invalid segment: lower bound (~a) is greater than upper bound (~a)" 
           lower upper))
  (make-segment-internal lower upper))

;;;-----------------------------------------------------------------------------
;;; Segment Operations
;;;-----------------------------------------------------------------------------

(defun segments-overlap-p (segment1 segment2)
  "Check if two segments overlap. Also returns T if they touch at endpoints within tolerance."
  (declare (type segment segment1 segment2))
  ;; First check the more common case: strict overlap
  (or (and (< (segment-lower segment1) (segment-upper segment2))
           (> (segment-upper segment1) (segment-lower segment2)))
      ;; Then check the touching endpoints with tolerance
      (linear-equal (segment-lower segment1) (segment-upper segment2))
      (linear-equal (segment-upper segment1) (segment-lower segment2))))

(defun segments-merge (segment1 segment2)
  "Merge two segments if they overlap.
   
   Parameters:
   - segment1: First segment to merge
   - segment2: Second segment to merge
   
   Returns:
   - A new merged segment, or NIL if the segments don't overlap
     or if merging would result in a degenerate segment
   
   Usage examples:
   (segments-merge segment1 segment2)     ;; Merge two segments"
  (declare (type segment segment1 segment2))
  
  ;; Check if the segments overlap
  (when (segments-overlap-p segment1 segment2)
    (let ((new-lower (min (segment-lower segment1) (segment-lower segment2)))
          (new-upper (max (segment-upper segment1) (segment-upper segment2))))
      ;; Check to ensure we don't create a degenerate segment
      (if (linear-equal new-lower new-upper)
          nil
          (make-segment-internal new-lower new-upper)))))

;;;-----------------------------------------------------------------------------
;;; Line Structure
;;;-----------------------------------------------------------------------------
;;; A line is represented as a collection of non-overlapping, ordered segments.

(defstruct (line (:constructor make-line-internal)) 
  "A line in one dimension, represented by a sorted list of non-overlapping segments."
  (segments nil :type list))

(defun make-line (&rest segments)
  "Create a line from a list of segments. Overlapping segments are unified automatically.
   
   Parameters:
   - segments: Optional list of segments to include in the line
   
   Returns:
   - A new line with all segments merged
   
   Usage examples:
   (make-line)                           ;; Create an empty line
   (make-line '(segment1 segment2 ...))  ;; Create a line from a list of segments"
    
    ;; Create a new result line
    (let ((result-line (make-line-internal)))
      
      ;; If we have segments to process
      (when segments
        ;; Sort all segments by lower bound for efficient processing
        (let ((sorted-segments (sort (copy-list segments) #'< :key #'segment-lower)))
          
          ;; Merge overlapping segments in a single pass
          (let ((result nil)
                (current nil))
            (dolist (segment sorted-segments)
              (cond
                ;; First segment
                ((null current) 
                 (setf current segment))
                
                ;; Current segment overlaps with previous
                ((segments-overlap-p current segment)
                 (setf current (segments-merge current segment)))
                
                ;; No overlap, store current and move to next
                (t 
                 (push current result)
                 (setf current segment))))
            
            ;; Don't forget the last segment
            (when current
              (push current result))
            
            ;; Store the result (already in correct order, reversed from push operations)
            (setf (line-segments result-line) (nreverse result)))))
      
      ;; Return the new line with all merged segments
    result-line))

;;;-----------------------------------------------------------------------------
;;; Line Operations
;;;-----------------------------------------------------------------------------

(defun lines-merge (line1 line2)
  "Merge exactly two lines into a single line.
   Returns a new line with all segments from both lines merged and unified.
   
   Arguments:
   - line1: First line to merge
   - line2: Second line to merge
   
   Returns:
   - A new line with all segments merged
   
   Usage example:
   (lines-merge line1 line2)"
  (declare (type line line1 line2))
  
  ;; Create a new result line
  (let ((result-line (make-line-internal)))
    
    ;; Get all segments from both lines
    (let ((all-segments (append (copy-list (line-segments line1))
                                (copy-list (line-segments line2)))))
      
      ;; If we have segments to process
      (when all-segments
        ;; Sort all segments by lower bound for efficient processing
        (setf all-segments (sort all-segments #'< :key #'segment-lower))
        
        ;; Merge overlapping segments in a single pass
        (let ((result nil)
              (current nil))
          (dolist (segment all-segments)
            (cond
              ;; First segment
              ((null current) 
               (setf current segment))
              
              ;; Current segment overlaps with previous
              ((segments-overlap-p current segment)
               (setf current (segments-merge current segment)))
              
              ;; No overlap, store current and move to next
              (t 
               (push current result)
               (setf current segment))))
          
          ;; Don't forget the last segment
          (when current
            (push current result))
          
          ;; Store the result (already in correct order, reversed from push operations)
          (setf (line-segments result-line) (nreverse result)))))
    
    ;; Return the new line with all merged segments
    result-line))

(defun line-empty-p (line)
  "Check if a line is empty (has no segments).
   
   Parameters:
   - line: The line to check
   
   Returns:
   - T if the line has no segments, NIL otherwise
   
   Usage example:
   (line-empty-p line) ;; => T if line has no segments"
  (declare (type line line))
  (null (line-segments line)))

;;;-----------------------------------------------------------------------------
;;; End of geometry-1d.lisp
;;;-----------------------------------------------------------------------------

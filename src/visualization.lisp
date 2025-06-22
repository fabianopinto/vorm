;;;; visualization.lisp - Visualization utilities for the VORM system

(in-package :vorm)

;;;-----------------------------------------------------------------------------
;;; SVG Rendering Functions
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;; Internal Helper Functions
;;;-----------------------------------------------------------------------------

(defun transform-segment-coordinates (segment angle pos &key (center-x 0) (center-y 0) (scale 1))
  "Transform segment coordinates to SVG coordinates based on angle and position.
   
   Parameters:
   - segment: The segment to transform
   - angle: The angle in radians
   - pos: The position value for the line containing this segment
   - center-x, center-y: The center point of the SVG (default: 0, 0)
   - scale: Scaling factor (default: 1)
   
   Returns:
   - Four values: x1, y1, x2, y2 representing the SVG line coordinates"
  (let* ((cos-angle (cos angle))
         (sin-angle (sin angle))
         (seg-lower (segment-lower segment))
         (seg-upper (segment-upper segment))
         
         ;; Calculate the start and end points of the segment
         ;; The segment is positioned along a line perpendicular to angle
         ;; pos represents the distance from origin along the angle direction
         (perp-angle (+ angle (/ pi 2)))
         (perp-cos (cos perp-angle))
         (perp-sin (sin perp-angle))
         
         ;; Start point: base position + offset along perpendicular direction for lower bound
         (x1 (+ center-x (* scale (+ (* pos cos-angle) (* seg-lower perp-cos)))))
         (y1 (+ center-y (* scale (+ (* pos sin-angle) (* seg-lower perp-sin)))))
         
         ;; End point: base position + offset along perpendicular direction for upper bound
         (x2 (+ center-x (* scale (+ (* pos cos-angle) (* seg-upper perp-cos)))))
         (y2 (+ center-y (* scale (+ (* pos sin-angle) (* seg-upper perp-sin))))))
    
    (values x1 y1 x2 y2)))

(defun calculate-view-bounds (shape &key (margin 10))
  "Calculate the minimum view bounds needed to contain the entire shape.
   
   Parameters:
   - shape: The shape to analyze
   - margin: Extra margin to add around the shape (default: 10)
   
   Returns:
   - Four values: min-x, min-y, max-x, max-y representing the bounds"
  (let ((min-x nil)
        (min-y nil)
        (max-x nil)
        (max-y nil))
    
    ;; Iterate over all angles in the shape
    (maphash 
     (lambda (angle parallels)
       (declare (type real angle))
       
       ;; Iterate over all positions in these parallels
       (maphash
        (lambda (pos line)
          (declare (type real pos))
          
          ;; Iterate over all segments in this line
          (dolist (segment (line-segments line))
            (multiple-value-bind (x1 y1 x2 y2)
                (transform-segment-coordinates segment angle pos)
              
              ;; Update bounds
              (setf min-x (if min-x (min min-x x1 x2) (min x1 x2)))
              (setf min-y (if min-y (min min-y y1 y2) (min y1 y2)))
              (setf max-x (if max-x (max max-x x1 x2) (max x1 x2)))
              (setf max-y (if max-y (max max-y y1 y2) (max y1 y2))))))
        (parallels-lines parallels)))
     (shape-angle-parallels shape))
    
    ;; Add margin
    (when (and min-x min-y max-x max-y)
      (decf min-x margin)
      (decf min-y margin)
      (incf max-x margin)
      (incf max-y margin))
    
    (values (or min-x 0) (or min-y 0) (or max-x 100) (or max-y 100))))

(defun render-shape-to-svg (shape &key 
                                  (width 500) 
                                  (height 500) 
                                  (auto-viewbox t) 
                                  (center-x nil) 
                                  (center-y nil)
                                  (scale nil)
                                  (stroke "black") 
                                  (stroke-width 1) 
                                  (opacity 1.0))
  "Render a shape to SVG format.
   
   Parameters:
   - shape: The shape to render
   - width: The width of the SVG (default: 500)
   - height: The height of the SVG (default: 500)
   - auto-viewbox: Whether to automatically calculate the viewBox (default: t)
   - center-x, center-y: The center point of the SVG (if nil and auto-viewbox is t, calculated automatically)
   - scale: Scaling factor (if nil and auto-viewbox is t, calculated automatically)
   - stroke: The line color (default: black)
   - stroke-width: The line width (default: 1)
   - opacity: The line opacity (default: 1.0)
   
   Returns:
   - A string containing the complete SVG representation of the shape"
  (with-output-to-string (svg)
    (multiple-value-bind (min-x min-y max-x max-y)
        (when auto-viewbox (calculate-view-bounds shape))
      
      (let* ((viewbox (when auto-viewbox (format nil "~A ~A ~A ~A" 
                                                min-x min-y 
                                                (- max-x min-x) (- max-y min-y))))
             (cx (or center-x (if auto-viewbox (/ (+ min-x max-x) 2) (/ width 2))))
             (cy (or center-y (if auto-viewbox (/ (+ min-y max-y) 2) (/ height 2))))
             (s (or scale 
                    (if auto-viewbox
                        (let ((width-scale (/ width (- max-x min-x)))
                              (height-scale (/ height (- max-y min-y))))
                          (min width-scale height-scale))
                        1))))
        
        ;; Write SVG header with proper viewBox
        (let ((vb (if viewbox 
                      viewbox 
                      (format nil "0 0 ~A ~A" width height))))
          (write-string (format nil "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"~A\" height=\"~A\" viewBox=\"~A\">~%" 
                           width height vb)
                       svg))
        
        ;; Iterate over all angles in the shape
        (maphash 
         (lambda (angle parallels)
           (declare (type real angle))
           
           ;; Iterate over all positions in these parallels
           (maphash
            (lambda (pos line)
              (declare (type real pos))
              
              ;; Iterate over all segments in this line
              (dolist (segment (line-segments line))
                (multiple-value-bind (x1 y1 x2 y2)
                    (transform-segment-coordinates segment angle pos 
                                                   :center-x cx :center-y cy :scale s)
                  
                  ;; Generate and write SVG line element
                  (write-string (format nil "  <line x1=\"~A\" y1=\"~A\" x2=\"~A\" y2=\"~A\" stroke=\"~A\" stroke-width=\"~A\" opacity=\"~A\" />~%" 
                                     x1 y1 x2 y2 stroke stroke-width opacity)
                              svg))))
            (parallels-lines parallels)))
         (shape-angle-parallels shape))
        
        ;; Write SVG footer
        (write-string "</svg>" svg)))))

;;;-----------------------------------------------------------------------------
;;; File Output Function
;;;-----------------------------------------------------------------------------

(defun render-shape-to-file (shape filename &rest options &key &allow-other-keys)
  "Render a shape to an SVG file.
   
   Parameters:
   - shape: The shape to render
   - filename: The name of the file to save to
   - options: Additional options passed to render-shape-to-svg
   
   Returns:
   - The filename on success"
  (let ((svg-content (apply #'render-shape-to-svg shape options)))
    (with-open-file (file filename :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string svg-content file)))
  filename)

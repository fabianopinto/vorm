(in-package :vorm)

;;;-----------------------------------------------------------------------------
;;; Random Set Generation
;;;-----------------------------------------------------------------------------

(defun generate-random-set (&key 
                          (lower-limit 0.0)
                          (upper-limit 1000.0)
                          (quantity 10)
                          (min-spacing 20.0))
  "Generate a set of randomly distributed real numbers.
   
   Arguments:
   - LOWER-LIMIT: The lower boundary (exclusive) for the random numbers
   - UPPER-LIMIT: The upper boundary (exclusive) for the random numbers
   - QUANTITY: Number of random values to generate
   - MIN-SPACING: Minimum spacing between any two numbers in the set
   
   Returns:
   A sorted list of random numbers between lower-limit and upper-limit (exclusive),
   with at least min-spacing distance between any two numbers.
   
   Note: If the range (upper-limit - lower-limit) is too small to accommodate 
   quantity numbers with min-spacing between them, the function will signal an error.
   
   Algorithm:
   1. Calculate total minimum space: (1 + quantity) * min-spacing
   2. Calculate total randomization interval: upper-limit - lower-limit - total-min-space
   3. Generate the quantity random numbers in the total randomization interval
   4. Sort the numbers
   5. Add to each number in the set, counting i from 0: (1 + i) * min-spacing"
  
  ;; Validate input parameters
  (when (>= lower-limit upper-limit)
    (error "Lower limit must be less than upper limit"))
  
  (when (<= quantity 0)
    (error "Quantity must be positive"))
  
  (when (<= min-spacing 0)
    (error "Minimum spacing must be positive"))
  
  ;; Calculate the total minimum space needed
  (let* ((total-min-space (* (1+ quantity) min-spacing))
         (total-range (- upper-limit lower-limit)))
    
    ;; Check if the range can accommodate the requested quantity with minimum spacing
    (when (< total-range total-min-space)
      (error "Range (~A) is too small to accommodate ~A numbers with minimum spacing of ~A"
             total-range quantity min-spacing))
    
    ;; Calculate the total randomization interval
    (let* ((randomization-interval (- total-range total-min-space))
           ;; Generate quantity random numbers in the randomization interval
           (random-numbers (loop repeat quantity
                                collect (* randomization-interval (random 1.0))))
           ;; Sort the numbers
           (sorted-numbers (sort random-numbers #'<))
           ;; Add the minimum spacing to each number
           (result (loop for number in sorted-numbers
                         for i from 0
                         collect (+ number lower-limit (* (1+ i) min-spacing)))))
      
      ;; Return the result
      result)))

(defun generate-random-line (&key
                          (lower-limit 0.0)
                          (upper-limit 1000.0)
                          (segment-count 5)
                          (min-spacing 20.0))
  "Generate a random line composed of random segments.
   
   Arguments:
   - LOWER-LIMIT: The lower boundary for the line coordinates
   - UPPER-LIMIT: The upper boundary for the line coordinates
   - SEGMENT-COUNT: Number of segments to generate
   - MIN-SPACING: Minimum value for both spacing between segments and segment size
   
   Returns:
   A line structure containing randomly generated segments within the specified range.
   Segments are non-overlapping and have at least min-spacing distance between them.
   Each segment has a length of at least min-spacing.
   
   Procedure:
   1. Generate a set of random numbers representing segment boundaries
   2. Ensure minimum spacing between segments and minimum segment size"
  
  ;; Validate input parameters
  (when (>= lower-limit upper-limit)
    (error "Lower limit must be less than upper limit"))
  
  (when (<= segment-count 0)
    (error "Segment count must be positive"))
  
  (when (<= min-spacing 0)
    (error "Minimum spacing must be positive"))
  
  ;; Check if range can accommodate segments
  (let ((available-range (- upper-limit lower-limit))
        ;; For each segment, we need:  segment-size + gap-after-segment
        ;; The total need is: (segment-count * min-spacing * 2) - min-spacing
        ;; (Subtracting min-spacing because we don't need gap after last segment)
        (required-range (* (- (* segment-count 2) 1) min-spacing)))
    
    (when (< available-range required-range)
      (error "Range (~A) is too small to accommodate ~A segments with min spacing/size of ~A"
             available-range segment-count min-spacing))
    
    ;; Generate 2*segment-count - 1 boundaries (start and end for each segment)
    (let* ((boundary-count (* 2 segment-count))
           ;; Generate random points for segment boundaries, with minimum spacing
           (boundaries (generate-random-set :lower-limit lower-limit
                                         :upper-limit upper-limit
                                         :quantity boundary-count
                                         :min-spacing min-spacing))
           (segments nil))
      
      ;; Convert boundaries into segments
      (loop for i from 0 below segment-count
            for start = (nth (* i 2) boundaries)
            for end = (nth (1+ (* i 2)) boundaries)
            when (and start end)
            do (push (make-segment start end) segments))
      
      ;; Create and return a line from the segments
      (apply #'make-line segments))))

(defun generate-random-parallels (&key
                                (line-count 10)
                                (lower-limit 0.0)
                                (upper-limit 1000.0)
                                (position-min-spacing 20.0)
                                (segment-count 5)
                                (segment-min-spacing 20.0))
  "Generate a random parallels structure with randomly positioned lines.
   
   Arguments:
   - LINE-COUNT: Number of lines to generate in the parallels structure
   - LOWER-LIMIT: Lower boundary for both line positions and coordinates
   - UPPER-LIMIT: Upper boundary for both line positions and coordinates
   - POSITION-MIN-SPACING: Minimum spacing between line positions
   - SEGMENT-COUNT: Number of segments in each line
   - SEGMENT-MIN-SPACING: Minimum value for segment size and spacing
   
   Returns:
   A parallels structure containing randomly generated lines at random positions.
   Each line is composed of random segments with the specified parameters."
  
  ;; Validate input parameters
  (when (<= line-count 0)
    (error "Line count must be positive"))
  
  (when (>= lower-limit upper-limit)
    (error "Lower limit must be less than upper limit"))
  
  (when (<= position-min-spacing 0)
    (error "Position minimum spacing must be positive"))
  
  (when (<= segment-min-spacing 0)
    (error "Segment minimum spacing must be positive"))
  
  ;; Create a new parallels structure
  (let ((parallels (make-parallels-internal)))
    
    ;; Generate random positions for the lines with minimum spacing
    (let ((positions (generate-random-set :lower-limit lower-limit
                                         :upper-limit upper-limit
                                         :quantity line-count
                                         :min-spacing position-min-spacing)))
      
      ;; For each position, create and add a random line
      (dolist (position positions)
        (let ((line (generate-random-line :lower-limit lower-limit
                                         :upper-limit upper-limit
                                         :segment-count segment-count
                                         :min-spacing segment-min-spacing)))
          
          ;; Add the line to the parallels structure at the current position
          (parallels-add-lines parallels (cons position line)))))
    
    ;; Return the complete parallels structure
    parallels))

(defun generate-random-shape (&key
                            (parallel-count 5)
                            (line-count 10)
                            (lower-limit 0.0)
                            (upper-limit 1000.0)
                            (position-min-spacing 20.0)
                            (segment-count 5)
                            (segment-min-spacing 20.0))
  "Generate a random shape composed of parallel lines at different angles.
   
   Arguments:
   - PARALLEL-COUNT: Number of parallels to generate, each at a different angle
   - LINE-COUNT: Number of lines to generate in each parallel
   - LOWER-LIMIT: Lower boundary for both line positions and coordinates
   - UPPER-LIMIT: Upper boundary for both line positions and coordinates
   - POSITION-MIN-SPACING: Minimum spacing between line positions
   - SEGMENT-COUNT: Number of segments in each line
   - SEGMENT-MIN-SPACING: Minimum value for segment size and spacing
   
   Note:
   - Angles are randomly generated between 0 and π (0 to 180 degrees)
   - Minimum angle spacing of π/8 (22.5 degrees) is maintained between any two parallels
   
   Returns:
   A shape structure containing randomly generated parallels at different angles,
   with each parallel containing randomly distributed lines composed of segments."
  
  ;; Validate input parameters
  (when (<= parallel-count 0)
    (error "Parallel count must be positive"))
  
  ;; Fixed angle constraints
  (let* ((min-angle 0.0)
         (max-angle pi)
         (min-angle-spacing (/ pi 8))  ;; π/8 radians = 22.5 degrees
         (available-angle-range (- max-angle min-angle))
         (required-angle-range (* (1- parallel-count) min-angle-spacing)))
    
    ;; Check if angle range can accommodate the parallels with minimum spacing
    (when (< available-angle-range required-angle-range)
      (error "Angle range (~A) is too small to accommodate ~A parallels with min spacing of ~A"
             available-angle-range parallel-count min-angle-spacing))
    
    ;; Create a new shape
    (let ((shape (make-shape-internal)))
      
      ;; Generate a set of random angles with minimum spacing
      (let ((angles (generate-random-set :lower-limit min-angle
                                         :upper-limit max-angle
                                         :quantity parallel-count
                                         :min-spacing min-angle-spacing)))
        
        ;; For each angle, create a parallel structure
        (dolist (angle angles)
          (let ((parallels (generate-random-parallels :line-count line-count
                                                      :lower-limit lower-limit
                                                      :upper-limit upper-limit
                                                      :position-min-spacing position-min-spacing
                                                      :segment-count segment-count
                                                      :segment-min-spacing segment-min-spacing)))
            
            ;; Add the parallels structure to the shape at the current angle
            (shape-add-parallels shape angle parallels))))
      
      ;; Return the complete shape
      shape)))

;;;-----------------------------------------------------------------------------
;;; End of math-random.lisp
;;;-----------------------------------------------------------------------------

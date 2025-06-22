;;;; math-random-tests.lisp - Tests for random number generation functions in the VORM system

(in-package :vorm/tests)

;; Ensure we're using the VORM-TESTS suite
(in-suite :vorm-tests)

;; Tests for generate-random-set basic functionality
(test generate-random-set-basic
  "Test the basic functionality of generate-random-set"
  ;; Test with default parameters
  (let ((random-set (generate-random-set)))
    ;; Check correct number of elements
    (is (= (length random-set) 10)
        "Default quantity should be 10")
    
    ;; Check all elements are within range (exclusive)
    (dolist (num random-set)
      (is (> num 0.0) "Elements should be greater than lower limit")
      (is (< num 1000.0) "Elements should be less than upper limit"))
    
    ;; Check list is sorted
    (is (equal random-set (sort (copy-list random-set) #'<))
        "Result should be sorted in ascending order")))

;; Test minimum spacing requirement
(test generate-random-set-spacing
  "Test that generate-random-set maintains minimum spacing"
  (let* ((min-spacing 20.0)
         (random-set (generate-random-set :min-spacing min-spacing)))
    
    ;; Verify minimum spacing between adjacent elements
    (loop for i from 0 below (1- (length random-set))
          for current = (nth i random-set)
          for next = (nth (1+ i) random-set)
          do (is (>= (- next current) min-spacing)
                 "Adjacent elements should maintain minimum spacing"))))

;; Test custom parameters
(test generate-random-set-custom
  "Test generate-random-set with custom parameters"
  (let* ((lower 10.0)
         (upper 200.0) ;; Increased upper limit to allow more space
         (quantity 15)
         (min-spacing 4.0) ;; Reduced minimum spacing
         (random-set (generate-random-set :lower-limit lower
                                          :upper-limit upper
                                          :quantity quantity
                                          :min-spacing min-spacing)))
    
    ;; Check correct number of elements
    (is (= (length random-set) quantity)
        "Should generate requested quantity of numbers")
    
    ;; Check range
    (dolist (num random-set)
      (is (> num lower) "Elements should be greater than specified lower limit")
      (is (< num upper) "Elements should be less than specified upper limit"))
    
    ;; Check minimum spacing
    (loop for i from 0 below (1- (length random-set))
          for current = (nth i random-set)
          for next = (nth (1+ i) random-set)
          do (is (>= (- next current) min-spacing)
                 "Adjacent elements should maintain custom minimum spacing"))))

;; Test error conditions
(test generate-random-set-errors
  "Test error conditions in generate-random-set"
  
  ;; Inverted limits
  (signals error
    (generate-random-set :lower-limit 100.0 :upper-limit 50.0)
    "Should signal error when lower limit exceeds upper limit")
  
  ;; Negative quantity
  (signals error
    (generate-random-set :quantity -5)
    "Should signal error when quantity is negative")
  
  ;; Zero quantity
  (signals error
    (generate-random-set :quantity 0)
    "Should signal error when quantity is zero")
  
  ;; Non-positive spacing
  (signals error
    (generate-random-set :min-spacing 0.0)
    "Should signal error when min-spacing is zero")
  
  ;; Range too small for requested quantity and spacing
  (signals error
    (generate-random-set :lower-limit 0.0
                         :upper-limit 10.0
                         :quantity 100
                         :min-spacing 1.0)
    "Should signal error when range can't accommodate quantity with min-spacing"))

;; Test statistical properties (distribution)
(test generate-random-set-distribution
  "Test the distribution properties of generate-random-set"
  (let* ((lower 0.0)
         (upper 100.0)
         (quantity 50)
         (random-set (generate-random-set :lower-limit lower
                                          :upper-limit upper
                                          :quantity quantity
                                          :min-spacing 0.01)))
    
    ;; Check all values are unique
    (is (= (length random-set) (length (remove-duplicates random-set :test #'=)))
        "All generated numbers should be unique")
    
    ;; Simple coverage check - split range into sections and ensure we have values in most sections
    ;; This is a simple way to check for reasonable distribution
    (let* ((sections 5)
           (section-size (/ (- upper lower) sections))
           (section-counts (make-array sections :initial-element 0)))
      
      ;; Count items in each section
      (dolist (num random-set)
        (let ((section-index (min (floor (/ (- num lower) section-size)) (1- sections))))
          (incf (aref section-counts section-index))))
      
      ;; We should have at least one item in most sections
      ;; (allowing for some randomness - at least 3/5 sections should have elements)
      (is (>= (count-if (lambda (count) (> count 0)) section-counts) 3)
          "Random distribution should cover most of the range"))))

;; Test reproducibility with random state
(test generate-random-set-reproducibility
  "Test that generate-random-set produces consistent results with same random state"
  (let ((saved-random-state (make-random-state nil)))
    ;; Generate two sets with same random state
    (let* ((random-state-1 (make-random-state saved-random-state))
           (random-state-2 (make-random-state saved-random-state))
           (set-1 (let ((*random-state* random-state-1)) 
                    (generate-random-set :quantity 20)))
           (set-2 (let ((*random-state* random-state-2))
                    (generate-random-set :quantity 20))))
      
      ;; Both sets should be identical
      (is (equal set-1 set-2)
          "Same random state should produce identical sets"))))

;;;-----------------------------------------------------------------------------
;;; generate-random-line Tests
;;;-----------------------------------------------------------------------------

(test generate-random-line-basic
  "Test the basic functionality of generate-random-line"
  ;; Test with default parameters
  (let* ((line (generate-random-line))
         (segments (line-segments line)))
    
    ;; Check that we got some segments
    (is (> (length segments) 0)
        "Line should have at least one segment")
    
    ;; Check each segment's bounds
    (dolist (segment segments)
      (let ((lower (segment-lower segment))
            (upper (segment-upper segment)))
        ;; Check segments are within global range
        (is (>= lower 0.0) "Segment lower bound should be >= lower-limit")
        (is (<= upper 1000.0) "Segment upper bound should be <= upper-limit")
        ;; Check segment size
        (is (> (- upper lower) 0) "Segment should have positive size")))))

(test generate-random-line-custom
  "Test generate-random-line with custom parameters"
  (let* ((lower 10.0)
         (upper 200.0) ;; Increased upper limit to allow more space
         (segment-count 5) ;; Reduced segment count
         (min-spacing 4.0) ;; Reduced minimum spacing
         (line (generate-random-line :lower-limit lower
                                    :upper-limit upper
                                    :segment-count segment-count
                                    :min-spacing min-spacing))
         (segments (line-segments line)))
    
    ;; Check segment count
    (is (<= (length segments) segment-count)
        "Line should have at most the requested number of segments")
    
    ;; Check segment bounds and spacing
    (dolist (segment segments)
      (let ((seg-lower (segment-lower segment))
            (seg-upper (segment-upper segment)))
        ;; Check segment bounds
        (is (>= seg-lower lower) "Segment lower bound should be >= specified lower-limit")
        (is (<= seg-upper upper) "Segment upper bound should be <= specified upper-limit")
        ;; Check segment size
        (is (>= (- seg-upper seg-lower) min-spacing)
            "Segment should have at least min-spacing length")))
    
    ;; Check spacing between segments
    (when (> (length segments) 1)
      (let ((sorted-segments (sort (copy-list segments) #'< :key #'segment-lower)))
        (loop for i from 0 below (1- (length sorted-segments))
              for current = (nth i sorted-segments)
              for next = (nth (1+ i) sorted-segments)
              do (is (>= (- (segment-lower next) (segment-upper current)) min-spacing)
                     "Spacing between segments should be at least min-spacing"))))))

(test generate-random-line-errors
  "Test error conditions in generate-random-line"
  
  ;; Inverted limits
  (signals error
    (generate-random-line :lower-limit 100.0 :upper-limit 50.0)
    "Should signal error when lower limit exceeds upper limit")
  
  ;; Negative segment count
  (signals error
    (generate-random-line :segment-count -5)
    "Should signal error when segment count is negative")
  
  ;; Zero segment count
  (signals error
    (generate-random-line :segment-count 0)
    "Should signal error when segment count is zero")
  
  ;; Non-positive spacing
  (signals error
    (generate-random-line :min-spacing 0.0)
    "Should signal error when min-spacing is zero")
  
  ;; Range too small for requested segments and spacing
  (signals error
    (generate-random-line :lower-limit 0.0
                         :upper-limit 10.0
                         :segment-count 100
                         :min-spacing 1.0)
    "Should signal error when range can't accommodate segments with min-spacing"))

;;;-----------------------------------------------------------------------------
;;; generate-random-parallels Tests
;;;-----------------------------------------------------------------------------

(test generate-random-parallels-basic
  "Test the basic functionality of generate-random-parallels"
  ;; Test with default parameters
  (let* ((parallels (generate-random-parallels))
         (lines-hash (parallels-lines parallels))
         (positions (parallels-positions parallels)))
    
    ;; Check that we got some positions with lines
    (is (> (length positions) 0)
        "Parallels should have at least one line")
    
    ;; Check each position-line pair
    (maphash (lambda (pos line)
               ;; Check position and line validity
               (is (>= pos 0.0) "Position should be >= lower-limit")
               (is (<= pos 1000.0) "Position should be <= upper-limit")
               (is (not (line-empty-p line)) "Line should not be empty"))
             lines-hash)))

(test generate-random-parallels-custom
  "Test generate-random-parallels with custom parameters"
  (let* ((lower 10.0)
         (upper 100.0)
         (line-count 5)
         (position-min-spacing 5.0)
         (segment-count 3)
         (segment-min-spacing 2.0)
         (parallels (generate-random-parallels :lower-limit lower
                                            :upper-limit upper
                                            :line-count line-count
                                            :position-min-spacing position-min-spacing
                                            :segment-count segment-count
                                            :segment-min-spacing segment-min-spacing))
         (positions (parallels-positions parallels)))
    
    ;; Check line count
    (is (<= (length positions) line-count)
        "Parallels should have at most the requested number of lines")
    
    ;; Check positions are within range
    (dolist (pos positions)
      (is (>= pos lower) "Position should be >= specified lower-limit")
      (is (<= pos upper) "Position should be <= specified upper-limit"))
    
    ;; Check minimum spacing between positions
    (when (> (length positions) 1)
      (let ((sorted-positions (sort (copy-list positions) #'<)))
        (loop for i from 0 below (1- (length sorted-positions))
              for current = (nth i sorted-positions)
              for next = (nth (1+ i) sorted-positions)
              do (is (>= (- next current) position-min-spacing)
                     "Spacing between positions should be at least position-min-spacing"))))
    
    ;; Check each line has proper segments
    (maphash (lambda (pos line)
               (let ((segments (line-segments line)))
                 ;; Check segment count
                 (is (<= (length segments) segment-count)
                     "Line should have at most the requested number of segments")
                 ;; Check segments
                 (dolist (segment segments)
                   (let ((seg-lower (segment-lower segment))
                         (seg-upper (segment-upper segment)))
                     (is (>= seg-lower lower) "Segment lower bound should be >= specified lower-limit")
                     (is (<= seg-upper upper) "Segment upper bound should be <= specified upper-limit")
                     (is (>= (- seg-upper seg-lower) segment-min-spacing)
                         "Segment should have at least segment-min-spacing length")))))
             (parallels-lines parallels))))

(test generate-random-parallels-errors
  "Test error conditions in generate-random-parallels"
  
  ;; Inverted limits
  (signals error
    (generate-random-parallels :lower-limit 100.0 :upper-limit 50.0)
    "Should signal error when lower limit exceeds upper limit")
  
  ;; Negative line count
  (signals error
    (generate-random-parallels :line-count -5)
    "Should signal error when line count is negative")
  
  ;; Zero line count
  (signals error
    (generate-random-parallels :line-count 0)
    "Should signal error when line count is zero")
  
  ;; Non-positive spacing
  (signals error
    (generate-random-parallels :position-min-spacing 0.0)
    "Should signal error when position-min-spacing is zero")
  
  (signals error
    (generate-random-parallels :segment-min-spacing 0.0)
    "Should signal error when segment-min-spacing is zero"))

;;;-----------------------------------------------------------------------------
;;; generate-random-shape Tests
;;;-----------------------------------------------------------------------------

(test generate-random-shape-basic
  "Test the basic functionality of generate-random-shape"
  ;; Test with default parameters
  (let* ((shape (generate-random-shape))
         (angles (shape-angles shape)))
    
    ;; Check that we got some angles with parallels
    (is (> (length angles) 0)
        "Shape should have at least one parallel")
    
    ;; Check angles are within proper range
    (dolist (angle angles)
      (is (>= angle 0.0) "Angle should be >= 0")
      (is (<= angle 3.14159) "Angle should be <= pi"))))

(test generate-random-shape-custom
  "Test generate-random-shape with custom parameters"
  (let* ((parallel-count 3)
         (line-count 4)
         (lower 10.0)
         (upper 100.0)
         (position-min-spacing 4.0)
         (segment-count 2)
         (segment-min-spacing 2.0)
         (shape (generate-random-shape :parallel-count parallel-count
                                     :line-count line-count
                                     :lower-limit lower
                                     :upper-limit upper
                                     :position-min-spacing position-min-spacing
                                     :segment-count segment-count
                                     :segment-min-spacing segment-min-spacing))
         (angles (shape-angles shape)))
    
    ;; Check parallel count
    (is (<= (length angles) parallel-count)
        "Shape should have at most the requested number of parallels")
    
    ;; Check each angle has proper parallels structure
    (maphash (lambda (angle parallels)
               ;; Check angle range
               (is (>= angle 0.0) "Angle should be >= 0")
               (is (<= angle pi) "Angle should be <= pi")
               
               ;; Check parallels structure
               (let ((positions (parallels-positions parallels)))
                 ;; Check line count
                 (is (<= (length positions) line-count)
                     "Parallels should have at most the requested number of lines")
                 
                 ;; Check positions are within range
                 (dolist (pos positions)
                   (is (>= pos lower) "Position should be >= specified lower-limit")
                   (is (<= pos upper) "Position should be <= specified upper-limit"))))
             (shape-angle-parallels shape))))

(test generate-random-shape-errors
  "Test error conditions in generate-random-shape"
  
  ;; Negative parallel count
  (signals error
    (generate-random-shape :parallel-count -5)
    "Should signal error when parallel count is negative")
  
  ;; Zero parallel count
  (signals error
    (generate-random-shape :parallel-count 0)
    "Should signal error when parallel count is zero")
  
  ;; Range too small for angles
  (signals error
    (generate-random-shape :parallel-count 100)
    "Should signal error when angle range is too small for requested parallel count"))


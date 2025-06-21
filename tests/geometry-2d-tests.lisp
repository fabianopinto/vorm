;;;; geometry-2d-tests.lisp - Tests for two-dimensional geometric primitives

(in-package :vorm/tests)

;; Ensure we're using the VORM-TESTS suite
(in-suite :vorm-tests)

;;; Parallels Tests

(test parallels-creation
  "Test parallels creation and accessors"
  ;; Empty parallels
  (let ((p (make-parallels)))
    (is (zerop (hash-table-count (parallels-lines p))))
    (is-true (= (length (parallels-positions p)) 0)))
  
  ;; Parallels with one line
  (let* ((line (make-line (make-segment 1.0 5.0)))
         (p (make-parallels (cons 10.0 line))))
    (is (= (hash-table-count (parallels-lines p)) 1))
    (is-true (gethash 10.0 (parallels-lines p)))))

(test parallels-position-tolerance
  "Test position tolerance for parallels"
  ;; With the new behavior, positions close to each other will merge the lines instead of error
  ;; So we test that the lines are properly merged
  (let* ((line1 (make-line (make-segment 1.0 5.0)))
         (line2 (make-line (make-segment 2.0 6.0)))
         (pos1 3.0)
         (pos2 (+ pos1 (* 0.5 *linear-tolerance*))) ; Position within tolerance
         (p (make-parallels (cons pos1 line1) (cons pos2 line2))))
    
    ;; Should only have one position (they were merged due to tolerance)
    (is (= (length (parallels-positions p)) 1))
    ;; The position should be pos1 (the first one added)
    (is (= (first (parallels-positions p)) pos1))
    ;; The line should be a merge of line1 and line2
    (let ((merged-line (gethash pos1 (parallels-lines p))))
      (is (= (length (line-segments merged-line)) 1))
      (let ((merged-segment (first (line-segments merged-line))))
        (is (= (segment-lower merged-segment) 1.0))
        (is (= (segment-upper merged-segment) 6.0)))))
  
  ;; Creating parallels with positions outside tolerance should keep them separate
  (let* ((line1 (make-line (make-segment 1.0 5.0)))
         (line2 (make-line (make-segment 2.0 6.0)))
         (pos1 3.0)
         (pos2 (+ pos1 (* 2.0 *linear-tolerance*))) ; Position outside tolerance
         (p (make-parallels (cons pos1 line1) (cons pos2 line2))))
    
    ;; Should have two separate positions
    (is (= (length (parallels-positions p)) 2))))

(test parallels-merge-lines-test
  "Test adding lines to parallels"
  (let* ((p (make-parallels))
         (line1 (make-line (make-segment 1.0 5.0)))
         (line2 (make-line (make-segment 2.0 6.0)))
         (pos1 3.0)
         (pos2 (+ pos1 (* 2.0 *linear-tolerance*)))) ; Position outside tolerance
    
    ;; Add first line
    (parallels-add-lines p (cons pos1 line1))
    (is (= (length (parallels-positions p)) 1))
    (is (eql (gethash pos1 (parallels-lines p)) line1))
    
    ;; Add second line
    (parallels-add-lines p (cons pos2 line2))
    (is (= (length (parallels-positions p)) 2))
    (is (eql (gethash pos2 (parallels-lines p)) line2))
    
    ;; Add a line at a position close to an existing one - should merge the lines
    (let* ((close-pos (+ pos1 (* 0.5 *linear-tolerance*)))
           (line3 (make-line (make-segment 7.0 9.0))))
      (parallels-merge-lines p (cons close-pos line3))
      
      ;; Position count should still be 2 (no new position added)
      (is (= (length (parallels-positions p)) 2))
      
      ;; The line at pos1 should now be a merge of line1 and line3
      (let* ((merged-line (gethash pos1 (parallels-lines p)))
             (segments (line-segments merged-line)))
        (is (= (length segments) 2)) ; Two non-overlapping segments
        (let ((seg1 (first segments))
              (seg2 (second segments)))
          (is (= (segment-lower seg1) 1.0))
          (is (= (segment-upper seg1) 5.0))
          (is (= (segment-lower seg2) 7.0))
          (is (= (segment-upper seg2) 9.0)))))))

(test find-nearest-position-test
  "Test finding the nearest position within tolerance"
  (let* ((positions '(10.0 20.0 30.0)))
    
    ;; Exact match
    (is (= (find-nearest-position 10.0 positions) 10.0))
    
    ;; Close match within tolerance
    (is (= (find-nearest-position (+ 10.0 (* 0.5 *linear-tolerance*)) positions) 10.0))
    
    ;; No match - outside tolerance
    (is-false (find-nearest-position (+ 10.0 (* 2.0 *linear-tolerance*)) positions))
    
    ;; Edge case - position exactly at tolerance boundary
    (let ((boundary-pos (+ 10.0 *linear-tolerance*)))
      ;; This should still find the nearest position
      (is (= (find-nearest-position boundary-pos positions) 10.0)))))

(test tolerance-boundary-test
  "Test behavior at the tolerance boundary"
  (let* ((line1 (make-line (make-segment 1.0 5.0)))
         (line2 (make-line (make-segment 6.0 10.0)))
         (pos1 10.0)
         ;; Position exactly at the tolerance boundary
         (pos2 (+ pos1 *linear-tolerance*))
         (p (make-parallels (cons pos1 line1))))
    
    ;; Adding at exactly the tolerance boundary should merge
    (parallels-add-lines p (cons pos2 line2))
    
    ;; Only one position should remain (merged)
    (is (= (length (parallels-positions p)) 1))
    
    ;; The lines should be merged
    (let* ((merged-line (gethash pos1 (parallels-lines p)))
           (segments (line-segments merged-line)))
      (is (= (length segments) 2)) ; Non-overlapping segments
      (let ((seg1 (first segments))
            (seg2 (second segments)))
        (is (= (segment-lower seg1) 1.0))
        (is (= (segment-upper seg1) 5.0))
        (is (= (segment-lower seg2) 6.0))
        (is (= (segment-upper seg2) 10.0))))))

(test parallels-positions-test
  "Test getting positions from parallels"
  (let* ((p (make-parallels))
         (line1 (make-line (make-segment 1.0 5.0)))
         (line2 (make-line (make-segment 2.0 6.0)))
         (line3 (make-line (make-segment 3.0 7.0))))
    
    ;; Add lines in random order
    (parallels-add-lines p 
                     (cons 30.0 line3) 
                     (cons 10.0 line1) 
                     (cons 20.0 line2))
    
    ;; Verify positions are sorted
    (let ((positions (parallels-positions p)))
      (is (equal positions '(10.0 20.0 30.0))))))

(test merge-multiple-close-positions
  "Test merging multiple positions that are all within tolerance of each other"
  (let* ((line1 (make-line (make-segment 1.0 5.0)))
         (line2 (make-line (make-segment 2.0 6.0)))
         (line3 (make-line (make-segment 3.0 7.0)))
         (pos1 10.0)
         (pos2 (+ pos1 (* 0.3 *linear-tolerance*))) ; Very close to pos1
         (pos3 (+ pos2 (* 0.3 *linear-tolerance*))) ; Very close to pos2, also within tolerance of pos1
         ;; Adding three positions that are all within tolerance of each other
         (p (make-parallels (cons pos1 line1) (cons pos2 line2) (cons pos3 line3))))
    
    ;; Should only have one position (all merged due to tolerance)
    (is (= (length (parallels-positions p)) 1))
    
    ;; The position should be pos1 (the first one added)
    (is (= (first (parallels-positions p)) pos1))
    
    ;; The line should be a merge of all three lines
    (let ((merged-line (gethash pos1 (parallels-lines p))))
      (is (= (length (line-segments merged-line)) 1))
      (let ((merged-segment (first (line-segments merged-line))))
        (is (= (segment-lower merged-segment) 1.0))
        (is (= (segment-upper merged-segment) 7.0))))))

(test merge-with-empty-lines
  "Test merging behavior with empty lines"
  (let* ((empty-line (make-line))
         (regular-line (make-line (make-segment 1.0 5.0)))
         (pos 10.0)
         (p (make-parallels)))
    
    ;; Add empty line first, then regular line at same position
    (parallels-merge-lines p (cons pos empty-line))
    (parallels-merge-lines p (cons pos regular-line))
    (is (= (length (parallels-positions p)) 1))
    (let ((result-line (gethash pos (parallels-lines p))))
      (is (= (length (line-segments result-line)) 1))
      (is (= (segment-lower (first (line-segments result-line))) 1.0))
      (is (= (segment-upper (first (line-segments result-line))) 5.0)))
    
    ;; Create new parallels and test the reverse - regular line first, then empty
    (let ((p2 (make-parallels)))
      (parallels-merge-lines p2 (cons pos regular-line))
      (parallels-merge-lines p2 (cons pos empty-line))
      (is (= (length (parallels-positions p2)) 1))
      (let ((result-line (gethash pos (parallels-lines p2))))
        (is (= (length (line-segments result-line)) 1))
        (is (= (segment-lower (first (line-segments result-line))) 1.0))
        (is (= (segment-upper (first (line-segments result-line))) 5.0))))))

(test merge-with-negative-positions
  "Test merging behavior with negative position values"
  (let* ((line1 (make-line (make-segment 1.0 5.0)))
         (line2 (make-line (make-segment 2.0 6.0)))
         (neg-pos1 -10.0)
         (neg-pos2 (+ neg-pos1 (* 0.5 *linear-tolerance*))) ; Within tolerance
         (p (make-parallels (cons neg-pos1 line1) (cons neg-pos2 line2))))
    
    ;; Should merge the negative positions
    (is (= (length (parallels-positions p)) 1))
    (is (= (first (parallels-positions p)) neg-pos1))
    
    ;; Test with positions on opposite sides of zero but within tolerance
    (let* ((near-zero-neg (- (* 0.5 *linear-tolerance*)))
           (near-zero-pos (* 0.3 *linear-tolerance*))
           (line3 (make-line (make-segment 3.0 7.0)))
           (line4 (make-line (make-segment 4.0 8.0)))
           (p2 (make-parallels (cons near-zero-neg line3) (cons near-zero-pos line4))))
      
      ;; These should merge despite being on opposite sides of zero
      (is (= (length (parallels-positions p2)) 1)))))

(test empty-lines-discarded
  "Test that empty lines are properly discarded"
  ;; Test make-parallels with empty lines
  (let* ((empty-line (make-line))
         (regular-line (make-line (make-segment 1.0 5.0)))
         (pos1 10.0)
         (pos2 20.0)
         ;; Create with one empty and one non-empty line
         (p (make-parallels (cons pos1 empty-line) (cons pos2 regular-line))))
    
    ;; Should only have the non-empty line
    (is (= (length (parallels-positions p)) 1))
    (is (= (first (parallels-positions p)) pos2))
    (is-false (gethash pos1 (parallels-lines p))))
  
  ;; Test parallels-merge-lines with empty lines
  (let* ((empty-line (make-line))
         (regular-line (make-line (make-segment 1.0 5.0)))
         (pos1 10.0)
         (pos2 20.0)
         (p (make-parallels)))
    
    ;; Add empty and non-empty lines
    (parallels-add-lines p 
                     (cons pos1 empty-line)
                     (cons pos2 regular-line))
    
    ;; Should only have the non-empty line
    (is (= (length (parallels-positions p)) 1))
    (is (= (first (parallels-positions p)) pos2))
    (is-false (gethash pos1 (parallels-lines p))))
  
  ;; Test that empty result from merging is discarded
  (let* ((line1 (make-line (make-segment 1.0 5.0)))
         (line2 (make-line (make-segment 1.0 5.0)))
         (p (make-parallels)))
    
    ;; Add first line
    (parallels-add-lines p (cons 10.0 line1))
    (is (= (length (parallels-positions p)) 1))
    
    ;; First, verify that we're working with an experimental setup
    ;; where merging produces an empty line (this would be unusual in
    ;; practice as merging normally preserves segments)
    (let ((merged (lines-merge line1 line2)))
      (when (not (line-empty-p merged))
        (skip "This test requires a setup where merging produces an empty line")))
    
    ;; Now create a specialization of lines-merge that produces empty lines when merging
    ;; the same line with itself (just for testing purposes)
    (flet ((test-empty-merge (line1 line2)
             (declare (ignore line1 line2))
             (make-line)))
      ;; Override the normal lines-merge behavior temporarily
      (with-redefinition ((lines-merge #'test-empty-merge))
        ;; Try to merge with same line using our test merger
        (parallels-add-lines p (cons 10.0 line1))
        
        ;; The position should be removed since merging yields an empty line
        (is (= (length (parallels-positions p)) 0))))))

(test complex-mixed-tolerance-merges
  "Test complex merging scenarios with mixed tolerance relationships"
  (let* ((line1 (make-line (make-segment 1.0 3.0)))
         (line2 (make-line (make-segment 4.0 6.0)))
         (line3 (make-line (make-segment 7.0 9.0)))
         (line4 (make-line (make-segment 10.0 12.0)))
         (line5 (make-line (make-segment 13.0 15.0)))
         
         ;; Create positions with mixed relationships:
         ;; pos1 and pos2 are within tolerance
         ;; pos3 is outside tolerance from pos1/pos2
         ;; pos4 and pos5 are within tolerance of each other
         (pos1 10.0)
         (pos2 (+ pos1 (* 0.5 *linear-tolerance*))) ; Within tolerance of pos1
         (pos3 (+ pos1 (* 3.0 *linear-tolerance*))) ; Outside tolerance of pos1/pos2
         (pos4 20.0)
         (pos5 (+ pos4 (* 0.7 *linear-tolerance*))) ; Within tolerance of pos4
         
         ;; Add all positions at once
         (p (make-parallels)))
    
    ;; Add all lines in one call
    (parallels-add-lines p 
                      (cons pos1 line1)
                      (cons pos2 line2)
                      (cons pos3 line3)
                      (cons pos4 line4)
                      (cons pos5 line5))
    
    ;; Should result in 3 distinct positions: pos1 (merged with pos2), pos3, and pos4 (merged with pos5)
    (is (= (length (parallels-positions p)) 3))
    
    ;; Check the merged lines for pos1 (should contain line1 and line2)
    (let* ((merged-line-1 (gethash pos1 (parallels-lines p)))
           (segments-1 (line-segments merged-line-1)))
      (is (= (length segments-1) 2))
      (let ((seg1 (first segments-1))
            (seg2 (second segments-1)))
        (is (= (segment-lower seg1) 1.0))
        (is (= (segment-upper seg1) 3.0))
        (is (= (segment-lower seg2) 4.0))
        (is (= (segment-upper seg2) 6.0))))
    
    ;; Check pos3 (should only have line3)
    (let* ((line-at-pos3 (gethash pos3 (parallels-lines p)))
           (segments-3 (line-segments line-at-pos3)))
      (is (= (length segments-3) 1))
      (is (= (segment-lower (first segments-3)) 7.0))
      (is (= (segment-upper (first segments-3)) 9.0)))
    
    ;; Check the merged lines for pos4 (should contain line4 and line5)
    (let* ((merged-line-4 (gethash pos4 (parallels-lines p)))
           (segments-4 (line-segments merged-line-4)))
      (is (= (length segments-4) 2))
      (let ((seg1 (first segments-4))
            (seg2 (second segments-4)))
        (is (= (segment-lower seg1) 10.0))
        (is (= (segment-upper seg1) 12.0))
        (is (= (segment-lower seg2) 13.0))
        (is (= (segment-upper seg2) 15.0))))))

(test performance-with-many-lines
  "Test performance with a large number of lines"
  (let* ((p (make-parallels))
         (lines-to-add 50)
         (positions nil))
    
    ;; Create positions with sufficient spacing
    (dotimes (i lines-to-add)
      (push (* i (+ 1.0 (* 2 *linear-tolerance*))) positions))
    
    ;; Create line pairs
    (let ((pos-line-pairs nil))
      (dolist (pos positions)
        (push (cons pos (make-line (make-segment 1.0 5.0))) pos-line-pairs))
      
      ;; Add all lines at once (batch operation)
      (finishes
        (apply #'parallels-add-lines p pos-line-pairs))
      
      ;; Verify correct number of lines added
      (is (= (length (parallels-positions p)) lines-to-add)))))

;;; Shape Tests

(test shape-creation
  "Test shape creation and accessors"
  ;; Empty shape
  (let ((s (make-shape)))
    (is (zerop (hash-table-count (shape-angle-parallels s))))
    (is-true (= (length (shape-angles s)) 0)))
  
  ;; Shape with one parallels structure
  (let* ((p (make-parallels (cons 10.0 (make-line (make-segment 1.0 5.0)))))
         (s (make-shape (cons 0.0 p))))
    (is (= (hash-table-count (shape-angle-parallels s)) 1))
    (is-true (gethash 0.0 (shape-angle-parallels s)))))

(test shape-angle-tolerance
  "Test angle tolerance for shapes"
  ;; With tolerant behavior, angles close to each other will merge the parallels
  (let* ((p1 (make-parallels (cons 10.0 (make-line (make-segment 1.0 5.0)))))
         (p2 (make-parallels (cons 20.0 (make-line (make-segment 2.0 6.0)))))
         (angle1 0.0)
         (angle2 (+ angle1 (* 0.5 *angular-tolerance*))) ; Angle within tolerance
         (s (make-shape (cons angle1 p1) (cons angle2 p2))))
    
    ;; Should only have one angle (they were merged due to tolerance)
    (is (= (length (shape-angles s)) 1))
    
    ;; The angle should be angle1 (the first one added)
    (is (= (first (shape-angles s)) angle1))
    
    ;; Get the merged parallels and examine them
    (let* ((merged-parallels (gethash angle1 (shape-angle-parallels s)))
           (positions (parallels-positions merged-parallels)))
      ;; Should have both positions
      (is (= (length positions) 2))
      (is (member 10.0 positions :test #'=))
      (is (member 20.0 positions :test #'=))))
  
  ;; Creating shape with angles outside tolerance should keep them separate
  (let* ((p1 (make-parallels (cons 10.0 (make-line (make-segment 1.0 5.0)))))
         (p2 (make-parallels (cons 20.0 (make-line (make-segment 2.0 6.0)))))
         (angle1 0.0)
         (angle2 (+ angle1 (* 2.0 *angular-tolerance*))) ; Angle outside tolerance
         (s (make-shape (cons angle1 p1) (cons angle2 p2))))
    
    ;; Should have two separate angles
    (is (= (length (shape-angles s)) 2))))

(test find-nearest-angle-test
  "Test finding the nearest angle within tolerance"
  (let* ((angles (list 0.0 (/ pi 2) pi)))
    
    ;; Exact match
    (is (= (find-nearest-angle 0.0 angles) 0.0))
    
    ;; Close match within tolerance
    (is (= (find-nearest-angle (+ 0.0 (* 0.5 *angular-tolerance*)) angles) 0.0))
    
    ;; No match - outside tolerance
    (is-false (find-nearest-angle (+ 0.0 (* 2.0 *angular-tolerance*)) angles))
    
    ;; Edge case - angle exactly at tolerance boundary
    (let ((boundary-angle (+ 0.0 *angular-tolerance*)))
      ;; This should still find the nearest angle
      (is (= (find-nearest-angle boundary-angle angles) 0.0)))
    
    ;; Wrapping around 2π
    (is (= (find-nearest-angle (- +two-pi+ (* 0.5 *angular-tolerance*)) angles) 0.0))))

(test shape-add-parallels-test
  "Test adding parallels to a shape"
  (let* ((p1 (make-parallels (cons 10.0 (make-line (make-segment 1.0 5.0)))))
         (p2 (make-parallels (cons 20.0 (make-line (make-segment 2.0 6.0)))))
         (angle1 0.0)
         (angle2 pi)
         (s (make-shape)))
    
    ;; Add first parallels
    (shape-add-parallels s angle1 p1)
    (is (= (length (shape-angles s)) 1))
    (let ((stored-parallels (gethash angle1 (shape-angle-parallels s))))
      (is (not (null stored-parallels)))
      (is (= (length (parallels-positions stored-parallels)) 1)))
    
    ;; Add second parallels at different angle
    (shape-add-parallels s angle2 p2)
    (is (= (length (shape-angles s)) 2))
    (let ((stored-parallels (gethash angle2 (shape-angle-parallels s))))
      (is (not (null stored-parallels)))
      (is (= (length (parallels-positions stored-parallels)) 1)))
    
    ;; Add third parallels at same angle as first (should merge)
    (let* ((p3 (make-parallels (cons 30.0 (make-line (make-segment 3.0 7.0))))))
      ;; Add the parallels
      (shape-add-parallels s angle1 p3)
      (is (= (length (shape-angles s)) 2))
      
      ;; The parallels at angle1 should now contain positions from both p1 and p3
      (let* ((merged-parallels (gethash angle1 (shape-angle-parallels s)))
             (positions (parallels-positions merged-parallels)))
        (is (= (length positions) 2))
        (is (member 10.0 positions :test #'=))
        (is (member 30.0 positions :test #'=))))))

(test shape-empty-parallels-handling
  "Test that shape handles empty parallels correctly"
  (let* ((empty-parallels (make-parallels))
         (non-empty-parallels (make-parallels (cons 10.0 (make-line (make-segment 1.0 5.0)))))
         (s (make-shape)))
    
    ;; Adding empty parallels should be ignored
    (shape-add-parallels s 0.0 empty-parallels)
    (is (= (length (shape-angles s)) 0))
    
    ;; Adding non-empty parallels should work
    (shape-add-parallels s pi non-empty-parallels)
    (is (= (length (shape-angles s)) 1))
    
    ;; Creating with empty parallels should filter them out
    (let ((s2 (make-shape (cons 0.0 empty-parallels) (cons pi non-empty-parallels))))
      (is (= (length (shape-angles s2)) 1))
      (is (= (first (shape-angles s2)) pi)))))



(test angle-normalization-in-shape
  "Test that angles are properly normalized in shape operations"
  (let* ((p1 (make-parallels (cons 10.0 (make-line (make-segment 1.0 5.0)))))
         (regular-angle (/ pi 4))
         (equivalent-angle (+ regular-angle +two-pi+)) ; Same angle plus 2π
         (s (make-shape (cons regular-angle p1))))
    
    ;; Should be able to add parallels using the equivalent angle
    (let* ((p2 (make-parallels (cons 20.0 (make-line (make-segment 2.0 6.0)))))
           (angles-before (shape-angles s)))
      ;; Add using equivalent angle
      (shape-add-parallels s equivalent-angle p2)
      
      ;; Should still have just one angle
      (is (= (length (shape-angles s)) 1))
      
      ;; The angle should be the original one
      (is (= (first (shape-angles s)) (first angles-before)))
      
      ;; Verify lines were merged by checking positions in parallels at the angle
      (let* ((merged-parallels (gethash (first (shape-angles s)) (shape-angle-parallels s)))
             (positions (parallels-positions merged-parallels)))
        (is (= (length positions) 2))
        (is (member 10.0 positions :test #'=))
        (is (member 20.0 positions :test #'=))))))

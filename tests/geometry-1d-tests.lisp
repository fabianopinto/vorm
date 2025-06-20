;;;; geometry-1d-tests.lisp - Tests for one-dimensional geometric primitives

(in-package :vorm/tests)

;; Ensure we're using the VORM-TESTS suite
(in-suite :vorm-tests)

;;; Segment Tests

(test segment-creation
  "Test segment creation and accessors"
  ;; Normal segment
  (let ((s (make-segment 1.0 5.0)))
    (is (= (segment-lower s) 1.0))
    (is (= (segment-upper s) 5.0))))  

;; Helper function for test-only segment comparison
(defun segments-equal-p (segment1 segment2)
  "Compare two segments by checking if their lower and upper bounds are equal.
   For testing purposes only."
  (and (= (segment-lower segment1) (segment-lower segment2))
       (= (segment-upper segment1) (segment-upper segment2))))

(test segment-degenerate-validation
  "Test that degenerate segments are properly prevented"
  ;; Attempt to create a degenerate segment with equal endpoints
  (signals error
    (make-segment 3.0 3.0))
  
  ;; Attempt to create a degenerate segment with endpoints within tolerance
  (signals error
    (make-segment 3.0 (+ 3.0 (* 0.5 *linear-tolerance*))))
  
  ;; Creating a non-degenerate segment should succeed
  (finishes
    (make-segment 1.0 5.0)))

(test segments-overlap-test
  "Test segment overlap detection"
  ;; Clearly overlapping segments
  (let ((s1 (make-segment 1.0 5.0))
        (s2 (make-segment 3.0 7.0)))
    (is (segments-overlap-p s1 s2)))
  
  ;; Touching segments (exactly)
  (let ((s1 (make-segment 1.0 5.0))
        (s2 (make-segment 5.0 7.0)))
    (is (segments-overlap-p s1 s2)))
  
  ;; Touching segments (within tolerance)
  (let ((s1 (make-segment 1.0 5.0))
        (s2 (make-segment (+ 5.0 (* 0.9 *linear-tolerance*)) 7.0)))
    (is (segments-overlap-p s1 s2)))
  
  ;; Non-overlapping segments
  (let ((s1 (make-segment 1.0 5.0))
        (s2 (make-segment (+ 5.0 (* 2.0 *linear-tolerance*)) 7.0)))
    (is-false (segments-overlap-p s1 s2))))

(test segments-merge-test
  "Test segment merging"
  ;; Overlapping segments
  (let* ((s1 (make-segment 1.0 5.0))
         (s2 (make-segment 3.0 7.0))
         (merged (segments-merge s1 s2)))
    (is (segments-equal-p merged (make-segment 1.0 7.0))))
  
  ;; Segments where one contains the other
  (let* ((s1 (make-segment 1.0 10.0))
         (s2 (make-segment 3.0 7.0))
         (merged (segments-merge s1 s2)))
    (is (segments-equal-p merged s1)))
  
  ;; Touching segments
  (let* ((s1 (make-segment 1.0 5.0))
         (s2 (make-segment 5.0 7.0))
         (merged (segments-merge s1 s2)))
    (is (segments-equal-p merged (make-segment 1.0 7.0))))
  
  ;; Non-overlapping segments
  (let* ((s1 (make-segment 1.0 3.0))
         (s2 (make-segment 4.0 7.0)))
    (is-false (segments-merge s1 s2)))
  
  ;; Edge case: lower limits differ by less than tolerance
  (let* ((lower1 3.0)
         (lower2 (+ lower1 (* 0.5 *linear-tolerance*)))
         ;; lower2 is slightly higher than lower1 but within tolerance
         (s1 (make-segment lower1 5.0))
         (s2 (make-segment lower2 7.0))
         (merged (segments-merge s1 s2)))
    ;; Merged segment should use the lowest of the lower limits (lower1)
    (is (segments-equal-p merged (make-segment lower1 7.0)))
    ;; Explicitly verify the merged segment has the lowest lower limit
    (is (= (segment-lower merged) lower1)))
  
  ;; Edge case: upper limits differ by less than tolerance
  (let* ((upper1 5.0)
         (upper2 (- upper1 (* 0.5 *linear-tolerance*)))
         ;; upper2 is slightly lower than upper1 but within tolerance
         (s1 (make-segment 1.0 upper1))
         (s2 (make-segment 3.0 upper2))
         (merged (segments-merge s1 s2)))
    ;; Merged segment should use the highest of the upper limits (upper1)
    (is (segments-equal-p merged (make-segment 1.0 upper1)))
    ;; Explicitly verify the merged segment has the highest upper limit
    (is (= (segment-upper merged) upper1))))

;;; Line Tests

(test line-creation
  "Test line creation"
  ;; Empty line
  (let ((l (make-line)))
    (is (= (length (line-segments l)) 0)))
  
  ;; Line with one segment
  (let ((l (make-line (make-segment 1.0 5.0))))
    (is (= (length (line-segments l)) 1))
    (is (segments-equal-p (first (line-segments l)) (make-segment 1.0 5.0))))
  
  ;; Line with multiple segments (non-overlapping)
  (let* ((s1 (make-segment 1.0 3.0))
         (s2 (make-segment 5.0 7.0))
         (l (make-line s1 s2)))
    (is (= (length (line-segments l)) 2))
    (is (segments-equal-p (first (line-segments l)) s1))
    (is (segments-equal-p (second (line-segments l)) s2)))
  
  ;; Line with multiple segments (overlapping)
  (let* ((s1 (make-segment 1.0 5.0))
         (s2 (make-segment 3.0 7.0))
         (l (make-line s1 s2)))
    (is (= (length (line-segments l)) 1))
    (is (segments-equal-p (first (line-segments l)) (make-segment 1.0 7.0)))))

(test lines-merge-test
  "Test merging segments and lines"
  ;; Merge non-overlapping segment into line
  (let* ((base-seg (make-segment 1.0 3.0))
         (new-seg (make-segment 5.0 7.0))
         (line1 (make-line base-seg))
         (line2 (make-line new-seg)))
    (let ((merged (lines-merge line1 line2)))
      (is (= (length (line-segments merged)) 2))
      (is (segments-equal-p (second (line-segments merged)) new-seg))))
  
  ;; Merge overlapping segment into line
  (let* ((base-seg (make-segment 1.0 5.0))
         (new-seg (make-segment 3.0 7.0))
         (line1 (make-line base-seg))
         (line2 (make-line new-seg)))
    (let ((merged (lines-merge line1 line2)))
      (is (= (length (line-segments merged)) 1))
      (is (segments-equal-p (first (line-segments merged)) (make-segment 1.0 7.0)))))

  ;; Merge identical segment into line
  (let* ((seg (make-segment 1.0 5.0))
         (line1 (make-line seg))
         (line2 (make-line seg)))
    (let ((merged (lines-merge line1 line2)))
      (is (= (length (line-segments merged)) 1))
      (is (segments-equal-p (first (line-segments merged)) seg))))

  ;; Merge segment that completes a gap
  (let* ((seg1 (make-segment 1.0 3.0))
         (seg2 (make-segment 7.0 9.0))
         (gap-seg (make-segment 3.0 7.0))
         (line1 (make-line seg1 seg2))
         (line2 (make-line gap-seg)))
    (let ((merged (lines-merge line1 line2)))
      (is (= (length (line-segments merged)) 1))
      (is (segments-equal-p (first (line-segments merged)) (make-segment 1.0 9.0)))))
  
  ;; Merge segment that bridges two existing segments
  (let* ((seg1 (make-segment 1.0 3.0))
         (seg2 (make-segment 7.0 9.0))
         (bridge-seg (make-segment 2.0 8.0))
         (line1 (make-line seg1 seg2))
         (line2 (make-line bridge-seg)))
    (let ((merged (lines-merge line1 line2)))
      (is (= (length (line-segments merged)) 1))
      (is (segments-equal-p (first (line-segments merged)) (make-segment 1.0 9.0)))))
  
  ;; Merge multiple segments as lines
  (let* ((seg1 (make-segment 1.0 3.0))
         (seg2 (make-segment 5.0 7.0))
         (line1 (make-line seg1))
         (line2 (make-line seg2)))
    (let ((merged (lines-merge line1 line2)))
      (is (= (length (line-segments merged)) 2))
      (is (segments-equal-p (first (line-segments merged)) seg1))
      (is (segments-equal-p (second (line-segments merged)) seg2))))
  
  ;; Merge multiple lines
  (let* ((line1 (make-line (make-segment 1.0 3.0)))
         (line2 (make-line (make-segment 5.0 7.0))))
    (let ((merged (lines-merge line1 line2)))
      (is (= (length (line-segments merged)) 2))
      (is (segments-equal-p (first (line-segments merged)) (make-segment 1.0 3.0)))
      (is (segments-equal-p (second (line-segments merged)) (make-segment 5.0 7.0)))))
  
  ;; Merge segments from multiple lines
  (let* ((seg1 (make-segment 1.0 3.0))
         (seg2 (make-segment 5.0 7.0))
         (seg3 (make-segment 10.0 12.0))
         (line1 (make-line seg1 seg2))
         (line2 (make-line seg3)))
    (let ((merged (lines-merge line1 line2)))
      (is (= (length (line-segments merged)) 3))
      (is (segments-equal-p (first (line-segments merged)) (make-segment 1.0 3.0)))
      (is (segments-equal-p (second (line-segments merged)) (make-segment 5.0 7.0)))
      (is (segments-equal-p (third (line-segments merged)) (make-segment 10.0 12.0))))))

(test line-empty-p-test
  "Test the line-empty-p predicate function"
  ;; Test with an empty line
  (let ((empty-line (make-line)))
    (is-true (line-empty-p empty-line)
             "An empty line should be detected as empty")
    
    ;; Add a segment and check it's no longer empty
    (let ((non-empty-line (make-line (make-segment 1.0 5.0))))
      (is-false (line-empty-p non-empty-line)
                "A line with segments should not be detected as empty"))
    
    ;; After merging with an empty line, result should not be empty
    (let* ((segment-line (make-line (make-segment 2.0 6.0)))
           (merged-line (lines-merge empty-line segment-line)))
      (is-false (line-empty-p merged-line)
                "Result of merging empty and non-empty line should not be empty")))
  
  ;; Test that merging two empty lines results in an empty line
  (let* ((empty-line1 (make-line))
         (empty-line2 (make-line))
         (merged-line (lines-merge empty-line1 empty-line2)))
    (is-true (line-empty-p merged-line)
             "Result of merging two empty lines should be empty")))

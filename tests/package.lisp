(defpackage :vorm.tests
  (:use :cl :fiveam :vorm)
  (:documentation "Test suite for the VORM system using FiveAM testing framework.
                   
                   This package contains comprehensive tests for all major components
                   of the VORM system, including:
                   - Basic shape functionality tests
                   - Geometric transformation tests
                   - Pattern matching and rule application tests
                   - Grammar parsing and execution tests
                   - Performance and edge case tests
                   
                   Run all tests with (vorm.tests:run-tests)")
  (:export #:run-tests))

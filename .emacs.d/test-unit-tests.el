;;; test-unit-tests.el --- Tests for test-unit mode.

;; Copyright (C) 2007 Phil Hagelberg

(require 'elunit)

;; (context "passing tests"
;; 	 (tag test-unit passing)
;; 	 (specify "should highlight tests")
;; 	 (specify "should increase passing count"))

;; (context "running tests"
;; 	 (tag test-unit running)
;; 	 (specify "should run all tests")
;; 	 (specify "should run filter once per line")
;; 	 (specify "should be able to run a single test"))

;; (context "failing tests"
;; 	 (tag test-unit failing)
;; 	 (specify "should store failure data"
;; 		  (save-excursion
;; 		    (find-file "test-unit-sample.rb")
;; 		    (test-unit-clear)
;; 		    (test-unit-run-tests "test_will_fail")
;; 		    (expect (length test-unit-problems) equal 0)))    ;; ensure the proper description is included
;; 	 (specify "should store line number")
;; 	 (specify "should jump to failures")
;; 	 (specify "should highlight failure")
;; 	 (specify "should highlight error")
;; 	 (specify "should show failure info")) ;; currently broken

;; test these things for every supported framework?
;;; test-unit-tests.el --- Tests for test-unit mode.

;; Copyright (C) 2007 Phil Hagelberg

(require 'test-unit)
(require 'elunit)

(elunit-clear-suites)

(defvar sample-file "test-unit-sample.rb")

;;; The tests:

(defsuite test-unit nil
  :setup-hook 'run-sample-tests)

(deftest passing-tests-should-indicate-success test-unit
  (with-current-buffer sample-file
    (assert-equal 3 test-unit-pass-count)))

(deftest problematic-tests test-unit
  (with-current-buffer sample-file
    (let ((error-description (assoc "test_might_error" test-unit-problems))
	  (failure-description (assoc "test_will_fail" test-unit-problems)))
      (assert-that error-description)
      (assert-match "undefined local variable.*`junk'" (cdr error-description))
      ;; should store line number
      ;; should jump to failures
      (assert-that failure-description)
      (assert-match "bad length" (cdr failure-description)))))

(deftest should-highlight-test test-unit
  (with-current-buffer sample-file
    
  ))

;; setup

(defun wait-for-finished-tests ()
  (while (get-process "test-unit")
    (sit-for 0.3)))

(defun run-sample-tests ()
  (find-file sample-file)
  (switch-to-buffer sample-file)
  (test-unit-clear)
  (test-unit-run-tests)
  (wait-for-finished-tests)) ; wait for tests to run

;; Run the tests if eval-ed from current buffer

(if (string= (buffer-name (current-buffer)) "test-unit-tests.el")
    (save-window-excursion (elunit "test-unit")))

;; test these things for every supported framework?
;;; test-unit-tests.el --- Tests for test-unit mode.

;; Copyright (C) 2007 Phil Hagelberg

(require 'test-unit)
(require 'elunit)

(elunit-clear-suites)

(defvar sample-file "test-unit-sample.rb")

;;; The tests:

(defsuite test-unit
   (deftest passing-tests-should-indicate-success
     (with-current-buffer sample-file
       (assert (equal 3 test-unit-pass-count))))

  (deftest problematic-tests
   (with-current-buffer sample-file
     (let ((error-description (assoc "test_might_error" test-unit-problems))
	   (failure-description (assoc "test_will_fail" test-unit-problems)))
       (assert error-description)
       (assert (string-match "undefined method `foo'" (cdr error-description)))
       ;; should store line number
       ;; should jump to failures
       (assert failure-description)
       (assert (string-match "bad length" (cdr failure-description))))))

  (deftest should-highlight-test

   )

(defsuite test-unit-single-test

)

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

(add-hook 'test-unit-suite-setup-hook 'run-sample-tests)

;; Run the tests if eval-ed from current buffer

(if (string= (buffer-name (current-buffer)) "test-unit-tests.el")
    (elunit "test-unit"))

;; run tests
(local-set-key "\C-c\C-t" 'eval-buffer)

;; test these things for every supported framework?
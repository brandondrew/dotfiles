;;; elunit.el --- Emacs Lisp Unit Testing framework

;; Copyright (C) 2006 Phil Hagelberg

;; Based on regress.el by Wayne Mesard and Tom Breton:
;; http://www.panix.com/~tehom/my-code/regress.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License can be obtained from the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary

;; Elunit is an xUnit-style testing framework for Emacs Lisp designed
;; to support Test-Driven Development.

(defvar *elunit-suites*
  '((default-suite ()))
  "A list of unit test suites")


;;; Defining tests

(defun* deftest (name docstring &key expected form suite)
  (unless (assoc suite *elunit-suites*) (push (list suite) *elunit-suites*))
  (push (list name docstring expected form) (cdr (assoc suite *elunit-suites*))))

(defun elunit-new-test ()
  (interactive)
  (let* ((test-name (read-string "Name this test: "))
	 (suite-name (completing-read "Part of suite: " 
				      (mapcar (lambda (suite) (symbol-name (car suite))) *elunit-suites*))))
    (insert (format "(deftest \"%s\"
  \"Docstring\"
  :expected t
  :form '()" test-name))
    (unless (eq "" suite-name)
      (insert (format "\n  :suite '%s" suite-name)))
    (insert ")")))


;;; Running the unit tests

(defun elunit (suite)
 (interactive (list (completing-read "Run test suite: " 
				     (mapcar (lambda (suite) (symbol-name (car suite))) 
					     *elunit-suites*))))
 (with-output-to-temp-buffer "*elunit*"
   (princ (concat "Loaded suite: " suite "\n\n"))
   (let ((tests (cdr (assoc (intern suite) *elunit-suites*))))
     (elunit-report-results (mapcar #'elunit-run-test tests)))))

(defun elunit-run-test (test)
  "Run the form, compare it with expected, print the status,
return the failure details or t if passed."
  (let* ((name (pop test))
	 (docstring (pop test))
	 (expected (pop test))
	 (form (pop test))
	 (actual (save-excursion (condition-case err
				    (eval form)
				  (error err)))))
    (setq passed (equal expected actual))
    (elunit-status passed)
    (if passed
	t
      (list name docstring expected actual form))))


;;; Showing the results

(defun elunit-status (pass)
  (princ (if pass "." "F")))

(defun elunit-report-results (tests)
  "Give a summary of the failures after the tests have all run"
  (dolist (test tests) ; wups--didn't need a function for this!
      (unless (eq t test)
	(apply 'elunit-report-result test)))
  (princ (format "\n\n\n%d tests total" (length tests)))) ; TODO - count successes vs fails
    
(defun elunit-report-result (name docstring expected actual form)
  (princ (format "\n\nFailure - %s
Expected: %s
  Actual: %s
    Form: %s" name expected actual form))) ; TODO - number the failures

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


(defvar *elunit-suites*
  '((default-suite ()))
  "A list of unit test suites")

(defun elunit-suite (name)
  (cdr (assoc name *elunit-suites*)))

(defun elunit-test (name suite)
  (when (symbolp suite) (setq suite (elunit-suite suite)))
  (assoc name suite))

(defun elunit-delete-test (name suite)
  (when (elunit-test name suite)
    (setf (cdr (assoc suite *elunit-suites*)) (assq-delete-all name (elunit-suite suite)))))


;;; Defining tests

(defun* deftest (name docstring &key (expected t) form suite)
  (unless (elunit-suite suite) (push (list suite) *elunit-suites*))
  (elunit-delete-test name suite)
  (push (list name docstring expected form buffer-file-name (line-number-at-pos))
	(cdr (assoc suite *elunit-suites*))))

(defun elunit-new-test ()
  (interactive)
  (let* ((test-name (read-string "Name this test: "))
	 (suite-name (completing-read "Part of suite: " 
				      (mapcar (lambda (suite) (symbol-name (car suite))) *elunit-suites*))))
    (insert (format "(deftest '%s
  \"Docstring\"
  :expected t
  :form '()" test-name))
    (unless (eq "" suite-name)
      (insert (format "\n  :suite '%s" suite-name)))
    (insert ")")))

(defun elunit-clear-suites ()
  (interactive)
  (setq *elunit-suites* '((default-suite ()))))


;;; Running the unit tests

(defun elunit (suite)
 (interactive (list (completing-read "Run test suite: " 
				     (mapcar (lambda (suite) (symbol-name (car suite))) 
					     *elunit-suites*))))
 (setq *elunit-fail-count* 0)
 (with-output-to-temp-buffer "*elunit*"
   (princ (concat "Loaded suite: " suite "\n\n"))
   (let ((tests (cdr (assoc (intern suite) *elunit-suites*)))
	 (start-time (cadr (current-time))))
       (elunit-report-results (mapcar (lambda (test) (apply 'elunit-run-test test)) 
				      tests))
       (princ (format " in %d seconds." (- (cadr (current-time)) start-time))))))

(defun elunit-run-test (name docstring expected form file-name line-number)
  (let ((actual (save-excursion (condition-case err
				    (eval form)
				  (error err)))))
    (setq passed (equal expected actual))
    (elunit-status passed)
    (if passed
	t
      (list name docstring expected actual form file-name line-number *elunit-fail-count*))))

;(add-hook 'temp-buffer-show-hook 'compilation-minor-mode)

;;; Showing the results

(defun elunit-status (pass)
  (princ (if pass "." "F"))
  (unless pass (incf *elunit-fail-count*)
	  (switch-to-buffer "*elunit*")
	  (overlay-put (make-overlay (point) (- (point) 1)) 'face '(foreground-color . "red"))
	  (switch-to-buffer nil)))

(defun elunit-report-results (tests)
  (dolist (test tests)
      (unless (eq t test)
	(apply 'elunit-report-result test)))
  (princ (format "\n\n\n%d tests total, %d failures" (length tests) *elunit-fail-count*)))
    
(defun elunit-report-result (name docstring expected actual form file-name line-number index)
  (princ (format "\n\n%d) Failure: %s [%s:%s]
  Expected: %s
    Actual: %s
      Form: %s" index name file-name line-number expected actual form)))

(add-to-list 'compilation-error-regexp-alist '("\\[\\([^:]*\\):\\([0-9]+\\)" 1 2))

(provide 'elunit)
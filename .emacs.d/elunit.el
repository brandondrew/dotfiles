;;; elunit.el --- Emacs Lisp Unit Testing framework

;; Copyright (C) 2006 - 2007 Phil Hagelberg

;; Author: Phil Hagelberg
;; URL: http://dev.technomancy.us/wiki/ElUnit

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Description:

;; Inspired by regress.el by Wayne Mesard and Tom Breton, Test::Unit
;; by Nathaniel Talbott, and xUnit by Kent Beck

;; elunit exists to accomodate test-driven development of Emacs Lisp
;; programs. Tests are divided up into suites. Each test makes a
;; number of assertions to ensure that things are going according to
;; expected.  

;; Tests are divided into suites for the purpose of hooks. These hooks
;; are meant to allow for extra setup that happens once per test, for
;; both before and after it runs.

;;; Todo:

;;  * running
;;  * reporting
;;  * distinguish between assertion failures and other errors (like behave.el)
;;  * more assertions

;;; Usage:

;; See http://dev.technomancy.us/phil/wiki/ElUnit for usage details.

(eval-when-compile 
  (require 'cl)
  (require 'compile))

(defstruct test-suite name children tests setup-hook teardown-hook)
(defstruct test name body file line)

(defvar elunit-default-suite
  "default-suite"
  "Choice to use for default suite to run (gets updated to last suite run)")

(defvar elunit-suites nil
  "A list of every suite that's been defined.")

(defun elunit-clear-suites ()
  (setq elunit-suites (list (make-test-suite :name 'default-suite))))

;;; Defining tests

(defmacro* defsuite (suite-name suite-ancestor &key setup-hook teardown-hook)
  "Create a suite, which may be hierarchical."
  `(let ((suite (make-test-suite :name ',suite-name
				 :setup-hook ,setup-hook :teardown-hook ,teardown-hook)))
     (elunit-delete-suite ',suite-name)
     (if ',suite-ancestor
	 (push suite (test-suite-children (elunit-get-suite ',suite-ancestor))))
     (add-to-list 'elunit-suites suite)))

(defun elunit-get-suite (name)
  (if (test-suite-p name)
      name
    (find name elunit-suites :test (lambda (name suite)
				     (equal name (test-suite-name suite))))))

(defun elunit-delete-suite (name)
  (setq elunit-suites (remove (elunit-get-suite name) elunit-suites)))


(defmacro deftest (name suite &rest body)
  ;; TODO: gensym
  `(let ((suite (elunit-get-suite ',suite)))
     (save-excursion
       ;; not a foolproof heuristic to get line number, but good enough.
       (search-backward (symbol-name ',name))
       (elunit-delete-test ',name suite)
       (push (make-test :name ',name :body (lambda () ,@body)
				     :file buffer-file-name :line (line-number-at-pos))
	     (test-suite-tests suite)))))

(defun elunit-get-test (name suite)
  (if (test-p name) name
    (find name (test-suite-tests (elunit-get-suite suite))
	  :test (lambda (name test) (equal name (test-name test))))))

(defun elunit-delete-test (name suite)
  (let ((suite (elunit-get-suite suite)))
    (setf (test-suite-tests suite)
	  (delete (elunit-get-test name suite) (test-suite-tests suite)))))

;;; Running the unit tests

(defun elunit (suite)
  "Ask for a single suite, run all its tests, and display the results."
  (interactive (list (completing-read (concat "Run test suite (default " elunit-default-suite "): " )
				      (mapcar (lambda (suite) (symbol-name (test-suite-name suite))) 
					      elunit-suites) nil t nil nil elunit-default-suite)))
 (setq elunit-default-suite suite)
 (setq elunit-fail-count 0)
 (setq elunit-test-count 0)

 (with-output-to-temp-buffer "*elunit*"
   (princ (concat "Loaded suite: " suite "\n\n"))
   (let ((start-time (cadr (current-time))))
     (elunit-run-suite (elunit-get-suite (intern suite)))
     (princ (format "%d tests in %d seconds." elunit-test-count (- (cadr (current-time)) start-time))))))

(defun elunit-run-suite (suite)
  "Run a suite's tests and children."
  (dolist (test (test-suite-tests suite))
    (elunit-run-test test))
  (dolist (child-suite (test-suite-children suite))
    (elunit-run-suite child-suite)))

(defun elunit-run-test (test)
  "Run a single test."
  (incf elunit-test-count)
  (funcall (test-body test)))
  
;;   (let* ((passed nil)
;; 	 (docstring (if (stringp (car body)) (pop body) ""))
;; 	 (result (condition-case err
;; 		     (save-excursion (eval (cons 'progn body)) (setq passed t))
;; 		   (error err))))
;;     (elunit-status passed)
;;     (if passed t
;;       (list name docstring result body file-name line-number *elunit-fail-count*))))


;; ;;; Showing the results

;; (defun elunit-status (pass) 
;;   "Output status while the tests are running"
;;   (princ (if pass "." "F"))
;;   (unless pass (incf *elunit-fail-count*)
;; 	  (switch-to-buffer "*elunit*")
;; 	  (overlay-put (make-overlay (point) (- (point) 1)) 'face '(foreground-color . "red"))
;; 	  (switch-to-buffer nil)))

;; (defun elunit-report-results (tests) 
;;   "For when the tests are finished and we want details"
;;   (dolist (test tests)
;;       (unless (eq t test)
;; 	(apply 'elunit-report-result test)))
;;   (princ (format "\n\n\n%d tests total, %d failures" (length tests) *elunit-fail-count*)))
    
;; (defun elunit-report-result (name docstring result body file-name line-number index)
;;   "Report a single test failure"
;;   (princ (format "\n\n%d) Failure: %s [%s:%s]
;;             %s
;;     Result: %s
;;       Form: %s" index name file-name line-number docstring result (car body))))

;; (add-hook 'temp-buffer-pshow-hook 'compilation-minor-mode)
;; (add-to-list 'compilation-error-regexp-alist '("\\[\\([^\]]*\\):\\([0-9]+\\)\\]" 1 2))

(provide 'elunit)
;;; elunit.el ends here
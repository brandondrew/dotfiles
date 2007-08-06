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

;; Tests are divided into suites for the purpose of hierarchical
;; structure and hooks. The hierarchy allows suites to belong to
;; suites, in essence creating test trees. The hooks are meant to
;; allow for extra setup that happens once per test, for both before
;; and after it runs.

;; The file `elunit-assertions.el' provides a number of helpful
;; assertions for ensuring that things are going properly. You may use
;; emacs' built-in `assert' function for checking such things, but the
;; assertions in that file provide much better reporting if you use
;; them. Using `assert-that' is preferred over built-in `assert'.

;;; Todo:

;;  * more helper functions

;;; Usage:

;; See http://www.emacswiki.org/cgi-bin/wiki/ElUnit for discussion and usage.
;; The file `elunit-test.el' contains meta-tests that you may find helpful
;; to refer to as samples.

;; Add the lines:
;; (make-local-variable 'after-save-hook)
;; (add-hook 'after-save-hook (lambda () (elunit "meta-suite")))
;; to the file containing your tests for convenient auto-running.

;;; Code:

(eval-when-compile 
  (require 'cl)
  (require 'compile))

(defstruct test-suite name children tests setup-hook teardown-hook)
(defstruct test name body file line message problem)

(put 'elunit-test-failed 'error-conditions '(failure))

(defvar elunit-default-suite
  "default-suite"
  "Choice to use for default suite to run (gets updated to last suite run)")

(defvar elunit-suites (list (make-test-suite :name 'default-suite))
  "A list of every suite that's been defined.")

(defvar elunit-test-count 0)
(defvar elunit-failures nil
  "A list of tests that have failed.")

(defvar elunit-done-running-hook nil
  "Runs when the tests are finished; passed a total test count and a failure count.")

(defun elunit-clear-suites ()
  (setq elunit-suites (list (make-test-suite :name 'default-suite))))

;;; Defining tests

(defmacro* defsuite (suite-name suite-ancestor &key setup-hook teardown-hook)
  "Define a suite, which may be hierarchical."
  `(let ((suite (make-test-suite :name ',suite-name
				 :setup-hook ,setup-hook :teardown-hook ,teardown-hook)))
     (elunit-delete-suite ',suite-name)
     (if ',suite-ancestor
	 (push suite (test-suite-children (elunit-get-suite ',suite-ancestor))))
     (add-to-list 'elunit-suites suite)))

(defun elunit-get-suite (suite)
  "Fetch a suite by its name."
  (if (test-suite-p suite)
      suite
    (find suite elunit-suites :test (lambda (suite asuite)
				     (equal suite (test-suite-name asuite))))))

(defun elunit-delete-suite (name)
  (setq elunit-suites (remove (elunit-get-suite name) elunit-suites)))


(defmacro deftest (name suite &rest body)
  ;; TODO: gensym
  (save-excursion
    (search-backward (symbol-name name) nil t)
    (let ((line (line-number-at-pos))
	  (file buffer-file-name))
      `(let ((suite (elunit-get-suite ',suite)))
	 ;; not a foolproof heuristic to get line number, but good enough.
	 (elunit-delete-test ',name suite)
	 (push (make-test :name ',name :body (lambda () ,@body)
				       :file ,file :line ,line)
	       (test-suite-tests suite))))))

(defun elunit-get-test (name suite)
  (if (test-p name) name
    (find name (test-suite-tests (elunit-get-suite suite))
	  :test (lambda (name test) (equal name (test-name test))))))

(defun elunit-delete-test (name suite)
  (let ((suite (elunit-get-suite suite)))
    (setf (test-suite-tests suite)
	  (delete (elunit-get-test name suite) (test-suite-tests suite)))))

;; TODO: broken.
(defun elunit-total-test-count (suite)
  (let ((suite (elunit-get-suite suite)))
    (if suite
	(+ (apply #'+ (elunit-total-test-count (test-suite-children suite)))
	   (length (test-suite-tests suite))))))

(defun elunit-test-docstring (test)
  (if (equal (car (test-body test)) 'lambda)
      (if (stringp (caddr (test-body test)))
	  (caddr (test-body test))
	"")))

;;; Running the unit tests

(defun elunit (suite)
  "Ask for a single suite, run all its tests, and display the results."
  (interactive (list (completing-read (concat "Run test suite (default " elunit-default-suite "): " )
				      (mapcar (lambda (suite) (symbol-name (test-suite-name suite))) 
					      elunit-suites) nil t nil nil elunit-default-suite)))
 (setq elunit-default-suite suite)
 (setq elunit-test-count 0)
 (setq elunit-failures nil)

 (with-output-to-temp-buffer "*elunit*"
   (switch-to-buffer "*elunit*")
   (compilation-minor-mode)
   (switch-to-buffer nil)

   (princ (concat "Loaded suite: " suite "\n\n"))
   (let ((start-time (cadr (current-time))))
     (elunit-run-suite (elunit-get-suite (intern suite)))
     (princ (format "\n\n%d tests with %d failures in %d seconds."
		    elunit-test-count (length elunit-failures)
		    (- (cadr (current-time)) start-time))))
   (elunit-report-failures)))

(defun elunit-run-suite (suite)
  "Run a suite's tests and children."
  (dolist (test (reverse (test-suite-tests suite)))
    (if (test-suite-setup-hook suite) (funcall (test-suite-setup-hook suite)))
    (elunit-run-test test)
    (if (test-suite-teardown-hook suite) (funcall (test-suite-teardown-hook suite))))
  (dolist (child-suite (test-suite-children suite))
    (elunit-run-suite child-suite))
  (run-hook-with-args 'elunit-done-running-hook elunit-test-count (length elunit-failures)))

(defun elunit-run-test (test)
  "Run a single test."
  (condition-case err
      (progn
	(incf elunit-test-count)
	(funcall (test-body test))
	(princ "."))
    (failure
     (elunit-failure test err "F"))
    (error
     (elunit-failure test err "E"))))

(defun elunit-failure (test err output)
  "Display and store failure info."
  (princ output)
  (setf (test-problem test) err)
  ;; color overlays are GNU-only IIRC
  (unless (featurep 'xemacs)
    (switch-to-buffer "*elunit*")
    (overlay-put (make-overlay (point) (- (point) 1)) 'face '(foreground-color . "red"))
    (switch-to-buffer nil))
  (setf (test-message test) err)
  (push test elunit-failures))

(defun elunit-report-failures ()
  "Summarize failures."
  (let ((count 0))
    (dolist (test elunit-failures)
      (incf count)
      (princ (format "\n\n%d) %s %s [%s:%s]
            %s
   Message: %s
      Form: %s" count
      (if (equal (car (test-problem test)) 'elunit-test-failed)
	  "Failure:" "  Error:")
      (test-name test) (test-file test) (test-line test)
      (elunit-test-docstring test) (test-message test) (test-body test))))))

(add-to-list 'compilation-error-regexp-alist '("\\[\\([^\]]*\\):\\([0-9]+\\)\\]" 1 2))

(require 'elunit-assertions)
(require 'elunit-helpers)
(provide 'elunit)
;;; elunit.el ends here
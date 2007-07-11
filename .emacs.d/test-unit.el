;;; test-unit.el --- Run unit tests within their own buffers

;; Copyright (C) 2007 Phil Hagelberg

;; Author: Phil Hagelberg <technomancy@gmail.com>
;; Created: 15 Jun 2007
;; Version: 0.2
;; Keywords: testing

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs; see the file COPYING, or type `C-h C-c'. If not,
;; write to the Free Software Foundation at this address:

;;   Free Software Foundation
;;   51 Franklin Street, Fifth Floor
;;   Boston, MA 02110-1301
;;   USA

;;; Commentary:

;; Rather than using compilation-mode for running unit tests, it is
;; desired that the tests run within the buffer containing them. This
;; will minimize buffer switching and allow users to see immediately
;; which tests pass and where the failures are.

;; Right now it only works with Ruby's test/unit library, but I've
;; tried to extract the language/framework specific code so it should
;; be easy to add support for other systems. See the
;; "Language-specific" section at the end of the file.

;; For now it relies on ruby-mode, which should come with your ruby
;; distribution and is slated to be included in the next Emacs. If you
;; don't have it, you can get it from the Ruby SVN repository:
;; http://svn.ruby-lang.org/repos/ruby/trunk/misc/

;; For ruby-mode it adds a hook that will try to determine if a buffer
;; contains unit tests and switch to test-unit-mode if it does.  It
;; also requires that the "turn" library be installed. It's packaged
;; as a gem, so "gem install turn" should get it.

;; If you want quick toggling between tests and implementation, (trust
;; me: if you're reading this, you do) try toggle.el:
;; http://www.emacswiki.org/cgi-bin/emacs/toggle.el

;; Primary URL for test-unit.el is http://dev.technomancy.us/wiki/TestUnitMode

;; Please contact me if you're interested in extending language support.

;;; Todo:

;;  * Finish behave.el support
;;  * Modeline could be better (moving indicator to the end)
;;  * Add in other languages? (PDI!)

;;; Bugs:

;;  * Can't switch buffers while test is running
;;  * Test output larger than 15 lines or so can't be seen
;;  * Certain test outputs cause errors in the filter

;;; Code:

(eval-when-compile
  (require 'cl))

(defface test-unit-pass-face
  `((t (:background "green")))
  "Face for passing unit tests"
  :group 'test-unit-faces)

(defface test-unit-fail-face
  `((t (:background "red1")))
  "Face for failed unit tests"
  :group 'test-unit-faces)

(defface test-unit-line-face
  `((t (:background "red3")))
  "Face for highlighting lines that cause problems"
  :group 'test-unit-faces)

(defface test-unit-error-face
  `((t (:background "chocolate1")))
  "Face for errored unit tests"
  :group 'test-unit-faces)


(define-minor-mode test-unit-mode
  "Major mode for editing and running unit tests.
\\{test-unit-mode-map}"

  :lighter "-test" 
  :keymap (setq test-unit-mode-map (make-sparse-keymap))

  (define-key test-unit-mode-map
    "\C-c\C-s" 'test-unit-run-tests)
  (define-key test-unit-mode-map
    "\C-c\C-d" 'test-unit-run-single-test)
  (define-key test-unit-mode-map
    "\C-c\C-i" 'test-unit-show-info)
  (define-key test-unit-mode-map
    "\C-c\C-k" 'test-unit-clear)
  (define-key test-unit-mode-map
    "\C-x`" 'test-unit-jump-to-next-failure)

  (set (make-local-variable 'test-unit-pass-count) 0)
  (set (make-local-variable 'test-unit-problems) ()) ; each entry is a cons of the test name and the description of the problem and line number
  (set (make-local-variable 'test-unit-incomplete-line) "") ; a buffer where we wait for a complete line from the test process
  (set (make-local-variable 'test-unit-failure-lines) ())

  (make-local-variable 'mode-line-format)
  ;; FIXME: backwards! (using append in add-to-list doesn't work)
  (add-to-list 'mode-line-format '("[" (:eval (number-to-string test-unit-pass-count)) "/" (:eval (number-to-string (length test-unit-problems))) "]")))

(defun test-unit-run-tests (&optional test)
  "Run unit tests and display results in same buffer."
  (interactive)
  (save-some-buffers)
  (test-unit-clear)
  (let ((process (funcall start-test-process-function (buffer-file-name) test)))
    (set-process-filter process 'test-unit-filter)))

(defun test-unit-run-single-test ()
  "Run just one unit test."
  (interactive)
  (test-unit-run-tests (funcall get-current-test-function)))

(defun test-unit-jump-to-next-failure ()
  (interactive)
  (let ((next-failure (pop test-unit-failure-lines)))
    (goto-line next-failure)
    (add-to-list 'test-unit-failure-lines next-failure t)))

(defun test-unit-clear ()
  (interactive)
  (setq test-unit-pass-count 0)
  (setq test-unit-problems ())
  (setq test-unit-failure-lines ())
  (setq test-unit-incomplete-line "")
  (remove-overlays))

(defun test-unit-filter (process output)
  "Calls appropriate test-filter only when a full line has been received."
  ;; SO bloody annoying; why can't processes do this by default?!
  ;; At least we can use the Power of Recursion (tm)!
  (if (string-match "\n" output)
      (progn
	;; Send everything up to the first newline to the real filter
	(funcall test-filter-function process (concat test-unit-incomplete-line (car (split-string output "\n"))))
	;; Recurse on the rest
	(test-unit-filter process (substring output (+ 1 (string-match "\n" output)))))
    ;; Save the remainder to a buffer
    (setq test-unit-incomplete-line (concat test-unit-incomplete-line output))))

(defun test-unit-pass-test (name)
  (incf test-unit-pass-count)
  (force-mode-line-update)
  (funcall colorize-test-function name 'test-unit-pass-face))

(defun test-unit-fail-test (name)
  (push (cons name "") test-unit-problems)
  (force-mode-line-update)
  (funcall colorize-test-function name 'test-unit-fail-face))

(defun test-unit-error-test (name)
  (push (cons name "") test-unit-problems)
  (force-mode-line-update)
  (funcall colorize-test-function name 'test-unit-error-face))

(defun test-unit-explain-problem (explanation)
  "Add the problem explanation to the latest entry in the
problems table. Highlight the line if given."
  (if (string-match "\\.rb:\\([0-9]*\\):in `test" explanation)
      ;; problem indicates its line number
      (test-unit-highlight-line (string-to-number (match-string-no-properties 1 explanation)) 'test-unit-line-face))
  (setf (cdar test-unit-problems) (concat (cdar test-unit-problems) "\n" explanation)))

(defun test-unit-tests-running ()
  (message "Tests running..."))
(defun test-unit-tests-done ()
  (setq test-unit-failure-lines (sort test-unit-failure-lines '<))
  (message "Tests running...done"))

(defun test-unit-highlight-line (line face)
  (save-excursion
    (add-to-list 'test-unit-failure-lines line)
    (goto-line line)
    (beginning-of-line)
    (let ((line-start (point)))
      (end-of-line)
      (overlay-put (make-overlay line-start (+ 1 (point))) 'face face))))

(defun test-unit-show-info ()
  (interactive)
  (message (cdar (assoc (ruby-get-current-test) test-unit-problems))))

;;;; Language-specific parts

(defun test-unit-use-framework (framework)
  (set (make-local-variable 'test-framework) framework) ; for debugging purposes mostly
  ;; these functions are necessary to support a language
  (set (make-local-variable 'get-current-test-function) (intern (concat framework "-get-current-test")))
  (set (make-local-variable 'start-test-process-function) (intern (concat framework "-start-test-process")))
  (set (make-local-variable 'colorize-test-function) (intern (concat framework "-colorize-test")))
  (set (make-local-variable 'test-filter-function) (intern (concat framework "-test-filter"))))


;;; Ruby - test/unit with "turn" gem

(ignore-errors
  (require 'ruby-mode)

  ;; required functions
  (defun ruby-get-current-test ()
    "Where are we now?"
    (substring-no-properties (caddr (split-string (ruby-add-log-current-method) ":"))))

  (defun ruby-colorize-test (test face)
    (save-excursion
      (overlay-put (make-overlay (ruby-beginning-of-method test) (ruby-end-of-method test)) 'face face)))

  (defun ruby-start-test-process (file &optional test)
					; (set-process-buffer 
			(if test
			    (start-process "test-unit" nil
					   "ruby" "-rturn" file "--name" test)
			  (start-process "test-unit" nil
					 "ruby" "-rturn" file)) 
					;(current-buffer))
			)
	

  (defun ruby-test-filter (process output)
    "Do a different thing based on the output of the test."
    (setq test-unit-incomplete-line "")
    (save-excursion
      ;; FIXME: switch to proper buffer
      ;;    (switch-to-buffer (getf (process-plist process) :test-buffer))
    (cond ((string-match "^Loaded suite" output)
	   (test-unit-tests-running))
	  ((string-match "==============================================================================" output)
	   (test-unit-tests-done))
	  ((string-match "\\(test[^ ]*\\) *PASS$" output)
	   (test-unit-pass-test (match-string-no-properties 1 output)))
	  ((string-match "\\(test[^ ]*\\) *FAIL$" output)
	   (test-unit-fail-test (match-string-no-properties 1 output)))
	  ((string-match "\\(test[^ ]*\\) *ERROR$" output)
	   (test-unit-error-test (match-string-no-properties 1 output)))
	  ((string-match "^	\\(.*\\)$" output) ;; failure explanation uses tabs. ick!
	   (test-unit-explain-problem (match-string-no-properties 1 output))))))

  ;; support functions
  (defun ruby-beginning-of-method (method)
    (beginning-of-buffer)
    (search-forward (concat "def " method))
    (beginning-of-line)
    (point))

  (defun ruby-end-of-method (&optional method)
    (ruby-end-of-block)
    (end-of-line)
    (forward-char)
    (point))
  
  ;; well, we want to actually *use* this.
  (add-hook 'ruby-mode-hook 
	    (lambda () (save-excursion (when (search-forward-regexp "class.*Test::Unit::TestCase" nil t)
				      (test-unit-mode)
				      (test-unit-use-framework "ruby"))))))

;;; Emacs Lisp - behave.el

(ignore-errors
  (require 'behave)

  (defun behave-get-current-test ()
    (save-excursion
      (end-of-line)
      (search-backward-regexp (concat "(specify \"\\([^\"]*\\)\""))
      (match-string-no-properties 1)))

  (defun behave-colorize-test (test face) 
    (save-excursion
      (beginning-of-buffer)
      (search-forward (concat "(specify " test))
      (overlay-put (make-overlay (beginning-of-defun) (end-of-defun)) 'face face)))

  (defun behave-start-process (file &optional test)
    )

  (add-hook 'emacs-lisp-mode-hook
	    (lambda () (save-excursion (when (search-forward "(context \"" nil t)
				      (test-unit-mode)
				      (test-unit-use-framework "behave"))))))

;;; More languages later?

(provide 'test-unit)

;;; test-unit.el ends here

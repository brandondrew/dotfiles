;;; test-unit.el --- Run Ruby unit tests within their own buffers

;; Copyright (C) 2007 Phil Hagelberg

;; Author: Phil Hagelberg <technomancy@gmail.com>
;; Created: 15 Jun 2007
;; Version: 0.1
;; Keywords: testing

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
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

;; This file defines a hook for ruby-mode that will try to determine
;; if a buffer contains unit tests and switch to test-unit-mode if it
;; does.

;; This mode is based on ruby-mode, which should come with your ruby
;; distribution and is slated to be included in the next Emacs. If you
;; don't have it, you can get it from the Ruby SVN repository:
;; http://svn.ruby-lang.org/repos/ruby/trunk/misc/

;; This also requires that the "turn" library be installed. It's
;; packaged as a gem, so "gem install turn" should get it.

;; If you want quick toggling between tests and implementation, try
;; toggle.el: http://www.emacswiki.org/cgi-bin/emacs/toggle.el

;; Primary URL for test-unit.el is http://dev.technomancy.us/wiki/TestUnitMode

;;; Code:

(require 'ruby-mode)

(defvar test-unit-incomplete-line ""
  "A buffer where we wait for a complete line from the test process")

(defface test-unit-pass-face
  `((t (:background "green")))
  "Face for passing unit tests"
  :group 'test-unit-faces)

(defface test-unit-fail-face
  `((t (:background "red")))
  "Face for failed unit tests"
  :group 'test-unit-faces)

(add-hook 'ruby-mode-hook 
	  (lambda () (save-excursion (if (search-forward-regexp "class.*Test::Unit::TestCase" nil t)
				    (test-unit-mode)))))

(define-derived-mode test-unit-mode
  ruby-mode "test/unit"
  "Major mode for editing and running Ruby unit tests.
\\{test-unit-mode-map}"
  
  (define-key test-unit-mode-map
    "\C-c\C-s" 'test-unit-run-tests)
  (define-key test-unit-mode-map
    "\C-c\C-d" 'test-unit-run-test))

(defun test-unit-run-tests ()
  "Run unit tests and display results in same buffer."
  (interactive)
  (remove-overlays)
  (let ((process (start-process "test-unit" nil
				"ruby" "-rturn" (buffer-file-name))))
    (set-process-filter process 'test-unit-filter-full-line)))


(defun test-unit-filter-full-line (process output)
  "Calls test-unit-filter only when a full line has been received."
  ;; SO bloody annoying; why can't processes do this by default?!
  (if (string-match "\n" output)
      (progn
	;; Send everything up to the first newline to the real filter
	(test-unit-filter (concat test-unit-incomplete-line (car (split-string output "\n"))))
	;; Recurse on the rest
	(test-unit-filter-full-line process (substring output (+ 1 (string-match "\n" output)))))
    ;; Save the remainder to a buffer
    (setq test-unit-incomplete-line (concat test-unit-incomplete-line output))))

(defun test-unit-filter (output)
  (setq test-unit-incomplete-line "")
  (cond ((string-match "^Loaded suite" output)
	 (test-unit-tests-running))
	((string-match "==============================================================================" output)
	 (test-unit-tests-done))
	((string-match "\\(test[^ ]*\\) *PASS$" output)
	 (test-unit-pass-test (match-string-no-properties 1 output)))
	((string-match "\\(test[^ ]*\\) *FAIL$" output)
	 (test-unit-fail-test (match-string-no-properties 1 output)))
	((string-match "^        \\(.*\\)$" output) ;; TODO: failure explanation uses tabs. =\
	 (test-unit-explain-failure (match-string-no-properties 1 output)))))

(defun test-unit-pass-test (name)
  (test-unit-colorize-method name 'test-unit-pass-face))

(defun test-unit-fail-test (name)
  (test-unit-colorize-method name 'test-unit-fail-face))

(defun test-unit-tests-running ()
  (message "Tests running..."))
(defun test-unit-tests-done ()
  (message "Tests running...done"))

(defun test-unit-colorize-method (method face) 
  (save-excursion
    (beginning-of-buffer)
    (search-forward (concat "def " method))
    (beginning-of-line)
    (let ((method-start (point)))
      (ruby-end-of-block)
      (end-of-line)
      (overlay-put (make-overlay method-start (point)) 'face face))))
;    (font-lock-append-text-property method-start (point) 'background-color 'red))))

(provide 'test-unit)
;;; test-unit.el ends here
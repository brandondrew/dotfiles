;;; test-unit-ruby.el --- Run Ruby unit tests within their own buffers

;; Copyright (C) 2007 Phil Hagelberg

;; Author: Phil Hagelberg <technomancy@gmail.com>
;; Created: 14 August 2007
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

;; This file provides ruby-specific functions to integrate with
;; `test-unit-mode'.

;; For now it relies on ruby-mode, which should come with your ruby
;; distribution and is slated to be included in the next Emacs
;; release. If you don't have it, you can get it from the Ruby SVN
;; repository: http://svn.ruby-lang.org/repos/ruby/trunk/misc/

;; For ruby-mode it adds a hook that will try to determine if a buffer
;; contains unit tests and switch to test-unit-mode if it does. It
;; also requires that the "turn" library be installed. It's packaged
;; as a gem, but it needs to be installed in your load path. In Debian
;; and derivatives the place it goes is /usr/local/lib/site_ruby/1.8/

;; See test-unit.el for details. See also test-unit-autotest.el for
;; smarter invokation rules.

(require 'ruby-mode)

;; required functions
(defun ruby-get-current-test ()
  "Where are we now?"
  (substring-no-properties (caddr (split-string (ruby-add-log-current-method) ":"))))

(defun ruby-colorize-test (test face)
  (save-excursion
    (overlay-put (make-overlay (ruby-beginning-of-method test) (ruby-end-of-method test)) 'face face)))

(defun ruby-start-test-process (file &optional test)
  (if test
      (start-process "test-unit" nil
		     "ruby" "-rturn" file "--name" test)
    (start-process "test-unit" nil
		   "ruby" "-rturn" file)))
	

(defun ruby-test-filter (process output)
  "Do a different thing based on the output of the test."
  (setq test-unit-incomplete-line "")
  (save-excursion
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
	   (test-unit-explain-problem (match-string-no-properties 1 output)))
	  ((string-match "\\([A-Za-z_-]+.rb\\):[0-9]+: .*error" output)
	   (error (concat "Error in test: "
			  (match-string-no-properties 1 output))))
	  ((string-match "/usr/bin/ruby: no such file to load -- turn (LoadError)" output)
	   (error "Need to install \"turn\" library. The gem is not good enough; it must be on your site path.")))))

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
;; (add-hook 'ruby-mode-hook 
;; 	  (lambda () (save-excursion
;; 		  (when (ruby-mode-test-p)
;; 		    (test-unit-mode)
;; 		    (test-unit-use-framework "ruby")))))

(defun ruby-mode-test-p ()
  (search-forward-regexp "class.*Test::Unit::TestCase" nil t))
  
(defun test-unit-impl-run ()
  (when (and (not (ruby-mode-test-p))
	     (toggle-filename (buffer-file-name) toggle-mappings))
    (toggle-buffer)
    (test-unit-run-tests)))

(provide 'test-unit-ruby)
;;; test-unit-ruby.el ends here
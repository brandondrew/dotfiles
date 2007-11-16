;;; test-unit-elunit.el --- Run elunit tests within their own buffers

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

;; This file provides emacs-lisp-specific functions to integrate with
;; `test-unit-mode'.

(require 'elunit)
(require 'thingatpt)

(defun elunit-get-current-test ()
  (save-excursion
    (search-backward "deftest" nil t)
    (forward-word 2)
    (thing-at-point 'symbol)))

;; TODO: WRITE ME.
(defun elunit-colorize-test (test face)
  (save-excursion
    (overlay-put (make-overlay (ruby-beginning-of-method test)
			       (ruby-end-of-method test)) 'face face)))

(defun elunit-start-test-process (file &optional test)
  )

(defun elunit-test-filter (process output)
  )

;; (add-hook 'emacs-lisp-mode-hook 
;; 	  (lambda () (save-excursion (when (search-forward-regexp "defsuite" nil t)
;; 				  (test-unit-mode)
;; 				  (test-unit-use-framework "elunit")))))

(provide 'test-unit-elunit)
;;; test-unit-elunit.el ends here
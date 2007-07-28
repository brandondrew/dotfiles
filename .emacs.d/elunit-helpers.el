;;; elunit-helpers.el --- Emacs Lisp Unit Testing framework helper functions

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

;; This file provides helper functions to allow further abstractions
;; and convenience in your tests. See elunit.el for more details.

;;; Code:

;;; Defining convenience



;;; Running convenience

(defun elunit-quiet (suite)
  (interactive (list (completing-read (concat "Run test suite (default " elunit-default-suite "): " )
				      (mapcar (lambda (suite) (symbol-name (test-suite-name suite))) 
					      elunit-suites) nil t nil nil elunit-default-suite)))
  (save-window-excursion
    (elunit suite))
  (message "%d tests with %d failures" elunit-test-count (length elunit-failures)))

(provide 'elunit-helpers)
;;; elunit-assertions.el ends here
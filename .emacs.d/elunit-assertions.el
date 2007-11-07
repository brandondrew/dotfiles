;;; elunit-assertions.el --- Emacs Lisp Unit Testing framework assertions

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

;; This file provides more assertions with which to write your elunit
;; tests. See elunit.el for more details.

;;; Code:

(defun fail (&rest args)
  "Like `error', but reported differently."
    (signal 'elunit-test-failed (list (apply 'format args))))

;;; General assertions

;; These are preferred over stuff like (assert (equal [...] because
;; they use the `fail' function, which reports errors nicely.

(defun assert-that (actual)
  (unless actual
    (fail "%s expected to be non-nil" actual)))

(defun assert-nil (actual)
  (when actual
    (fail "%s expected to be nil" actual)))

(defun assert-equal (expected actual)
  (unless (equal expected actual)
    (fail "%s expected to be %s" actual expected)))

(defun assert-not-equal (expected actual)
  (when (equal expected actual)
    (fail "%s expected to not be %s" actual expected)))

(defun assert-member (elt list)
  (unless (member elt list)
    (fail "%s expected to include %s" list elt)))

(defun assert-match (regex string)
  (unless (string-match regex string)
    (fail "%s expected to match %s" string regex)))

(defmacro assert-error (&rest body)
  `(condition-case err
       (progn
	 ,@body
	 (fail "%s expected to signal an error" body))
     (error t)))

(defmacro assert-changed (form &rest body)
  `(assert-not-equal (eval ,form)
		     (progn
		       ,@body
		       (eval ,form))))

(defmacro assert-not-changed (form &rest body)
  `(assert-equal (eval ,form)
		     (progn
		       ,@body
		       (eval ,form))))

;; Buffer-specific assertions

(defun assert-in-buffer (target &optional buffer)
  (save-window-excursion
    (if buffer (switch-to-buffer buffer))
    (goto-char (point-min))
    (unless (search-forward target nil t)
      (fail "%s expected to be found in buffer %s" target buffer))))

(defun assert-background (target face &optional buffer)
  (save-window-excursion
    (if buffer (switch-to-buffer buffer))
    (goto-char (point-min))
    (unless (search-forward target nil t)
      (fail "%s expected to be found in buffer %s" target buffer))
    (unless (equal (face (get-text-property (point) 'background)))
      (fail "%s expected to be displayed with face %s" target face))))

(defun assert-overlay (pos)
  (unless (overlays-at pos)
    (fail "Expected overlay at position %d" pos)))

(defun assert-no-overlay (pos)
  (if (overlays-at pos)
    (fail "Expected no overlay at position %d" pos)))

(provide 'elunit-assertions)
;;; elunit-assertions.el ends here
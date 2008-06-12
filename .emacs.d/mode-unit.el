;;; mode-unit.el --- Testing Framework for Emacs Modes

;; Copyright (C) 2008 Phil Hagelberg

;; Author: Phil Hagelberg <technomancy@gmail.com>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/ModeUnit
;; Version: 0.1
;; Created: 2008-06-11
;; Keywords: tools lisp
;; EmacsWiki: ModeUnit

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; ModeUnit builds on top of ElUnit to provide higher-level tests to
;; be written. Specifically it's focused on tests for Emacs modes
;; rather than on the lower level of Emacs Lisp functions.

;; To this end it is mostly a collection of helper functions (for
;; buffer setup) and higher-level assertions that handle the kind of
;; thing major modes are usually concerned with, like font-lock and
;; indentation.

;; Note: ModeUnit expects to be run in a clean "emacs -Q" invocation.
;; The rationale behind this is that it's very easy to write modes
;; that accidentally rely on functions defined somewhere in your
;; custom Emacs config, but your mode should be usable independently;
;; only relying on what it explicitly requires. So to run ModeUnit you
;; would use something like:

;;   $ emacs -Q -l my-mode-unit-test.el

;; You would want to invoke mode-unit at the bottom of your test file.

;;; Code:

(require 'elunit)

;; Buffer-specific assertions

(defun assert-in-buffer (target &optional buffer)
  "Fails if TARGET is not a string found in current buffer or BUFFER."
  (save-window-excursion
    (if buffer (switch-to-buffer buffer))
    (goto-char (point-min))
    (unless (search-forward target nil t)
      (fail "%s expected to be found in buffer %s" target buffer))))

(defun assert-background (target face &optional buffer)
  "Fails if TARGET is not rendered in FACE face.

May be passed BUFFER, otherwise defaults to current buffer."
  (save-window-excursion
    (if buffer (switch-to-buffer buffer))
    (goto-char (point-min))
    (unless (search-forward target nil t)
      (fail "%s expected to be found in buffer %s" target buffer))
    (unless (equal face (get-text-property (point) 'background))
      (fail "%s expected to be displayed with face %s" target face))))

(defun assert-overlay (pos)
  "Fails if overlay is not present at POS."
  (unless (overlays-at pos)
    (fail "Expected overlay at position %d" pos)))

(defun assert-no-overlay (pos)
  "Fails if overlay is present at POS."
  (if (overlays-at pos)
    (fail "Expected no overlay at position %d" pos)))

(defun assert-correct-indentation (filename)
  "Fails if the indenting rules change indentation of the contents of filename."
  (save-excursion
    (find-file filename)
    (let ((buffer-original-indentation (buffer-string))
	  (kill-buffer-query-functions nil))
      (indent-region (point-min) (point-max))
      (let ((buffer-new-indentation (buffer-string)))
	(kill-buffer nil)
	(unless (equal buffer-original-indentation buffer-new-indentation)
	  (fail "Indentation incorrect for %s" filename))))))
      

(provide 'mode-unit)
;;; mode-unit.el ends here
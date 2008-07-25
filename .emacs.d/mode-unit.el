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
;; be written.  Specifically it's focused on tests for Emacs modes
;; rather than lower level Emacs Lisp functions.

;; To this end it is mostly a collection of helper functions (for
;; buffer setup) and higher-level assertions that handle the kind of
;; thing major modes are usually concerned with, like font-lock and
;; indentation.  So ModeUnit tests end up looking a lot like ElUnit
;; tests; see the documentation for that library for some examples.

;; The main difference is that each ModeUnit suite is bound to the
;; context of a given file and mode.  So in addition to elunit's
;; defsuite, you may use mode-unit-suite to define a suite that always
;; loads up a file in a fresh buffer and invokes a mode before running
;; each of its tests:

;; (defsuite my-mode-suite default-suite ;; ancestor of default.
;;   "foo-sample.my" my-mode)

;; Note: ModeUnit expects to be run in a clean "emacs -Q" invocation.
;; The rationale behind this is that it's very easy to write modes
;; that accidentally rely on functions defined somewhere in your
;; custom Emacs config, but your mode should be usable independently;
;; only relying on what it explicitly requires.  So to run ModeUnit you
;; would use something like:

;;  $ emacs -Q -l my-mode-unit-test.el

;; or better yet:


;; (global-set-key (kbd "C-c C-m")
;;                 (shell-command "emacs -Q -l my-mode-unit-test.el"))
;; TODO: figure out how to non-block w/o async output buffer

;; You would want to include an invokation of elunit with your given
;; suite at the bottom of your test file.

;;; TODO:

;; - defsuite assumes tests do not open new buffers.
;; - make it easy to jump to problem in fixtures in fontification tests
;; - port assertions to predicates for ERT

;;; Code:

(require 'elunit)

(defstruct test-suite name children tests
  setup-hooks teardown-hooks file mode)

(elunit-clear-suites) ;; so default-suite is a struct of the same definition

(defmacro* mode-unit-suite (suite-name suite-ancestor file mode
                                       &key setup-hooks teardown-hooks)
  "Create a test suite with extra mode-unit hooks."
  `(let ((suite (defsuite ,suite-name ,suite-ancestor
                  :setup-hooks (cons (lambda ()
                                       (find-file (test-suite-file suite))
                                       (funcall (test-suite-mode suite)))
                                     ,setup-hooks)
                  :teardown-hooks (cons (lambda ()
                                          (kill-buffer nil)) ,teardown-hooks))))
     (setf (test-suite-file suite) ,file)
     (setf (test-suite-mode suite) ',mode)))

(defmacro with-test-buffer (&rest body)
  "Execute BODY in a test buffer named `*mode-unit-output*'."
  `(save-excursion
     (switch-to-buffer "*mode-unit-output*")
     ,@body
     (kill-buffer "*mode-unit-output*")))

;;; Buffer-specific assertions

(defun assert-in-buffer (target &optional buffer)
  "Fails if TARGET is not a string found in current buffer or BUFFER."
  (save-window-excursion
    (if buffer (switch-to-buffer buffer))
    (goto-char (point-min))
    (unless (search-forward target nil t)
      (fail "%s expected to be found in buffer %s" target buffer))))

(defun assert-face-at-point (face &optional point)
  "Fails if text at POINT is not rendered in FACE."
  (font-lock-mode t)
  (jit-lock-fontify-now)
  (unless (equal face (get-text-property (or point (point)) 'face))
    (fail "Text at \"%s\" expected to be displayed with face %s, but was %s."
          (symbol-name (symbol-at-point))
          face
          (get-text-property (or point (point)) 'face))))

(defun assert-overlay (pos)
  "Fails if overlay is not present at POS."
  (unless (overlays-at pos)
    (fail "Expected overlay at position %d" pos)))

(defun assert-no-overlay (pos)
  "Fails if overlay is present at POS."
  (if (overlays-at pos)
    (fail "Expected no overlay at position %d" pos)))

(defun assert-font-lock (face targets)
  "Fails if all instances of TARGETS regexps are not font-locked with FACE."
  (if (stringp targets) (setq targets (list targets))) ;; allow single argument
  (save-excursion
    (dolist (target targets)
      (beginning-of-buffer)
      (while (search-forward-regexp target nil t)
        (backward-char)
        (assert-face-at-point face)))))

(defun assert-correct-indentation (filename)
  "Fails if the indenting rules change indentation of the contents of FILENAME."
  (save-excursion
    (find-file filename)
    (let ((buffer-original-indentation (buffer-string))
          (kill-buffer-query-functions nil))
      (indent-region (point-min) (point-max))
      (let ((buffer-new-indentation (buffer-string)))
        (kill-buffer nil)
        (unless (equal buffer-original-indentation buffer-new-indentation)
          (fail "Indentation incorrect for %s" filename))))))

(font-lock-add-keywords 'emacs-lisp-mode
                        '(("mode-unit-suite" . 'font-lock-keyword-face)))

(provide 'mode-unit)
;;; mode-unit.el ends here
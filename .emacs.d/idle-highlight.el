;;; idle-highlight.el --- Highlight the word the point is on.

;; Copyright (C) 2008 Phil Hagelberg

;; Author: Phil Hagelberg <technomancy@gmail.com>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/IdleHighlight
;; Version: 1.0
;; Created: 2008-05-13
;; Keywords: convenience
;; EmacsWiki: IdleHighlight

;; This file is NOT part of GNU Emacs.

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

;;; Commentary:

;; Based on some snippets by fledermaus from #emacs channel.

;; M-x idle-highlight sets an idle timer that highlights all
;; occurences in the buffer of the word under the point.

;;; Code: 

(require 'thingatpt)

(defun idle-highlight-word-at-point ()
  (let* ((target-symbol (symbol-at-point)) ;; need to save this value to check for nil
	 (target (symbol-name target-symbol)))
    (when idle-highlight-last-word
      (unhighlight-regexp (concat "\\<" (regexp-quote idle-highlight-last-word) "\\>")))
    (when (and target-symbol (not (in-string-p)))
      (highlight-regexp (concat "\\<" (regexp-quote target) "\\>") 'region)
      (setq idle-highlight-last-word target))))

(defun idle-highlight ()
  "Activate idle-highlighting."
  (interactive)
  (make-local-variable 'idle-highlight-last-word)
  (set (make-local-variable 'idle-highlight-timer)
       (run-with-idle-timer 0.5 :repeat 'idle-highlight-word-at-point)))

(defun idle-highlight-disable ()
  "Disable idle-highlighting caused by `idle-highlight'."
  (interactive)
  (if (boundp 'idle-highlight-timer)
      (cancel-timer idle-highlight-timer)
    (message "idle-highlight not enabled")))

(provide 'idle-highlight)
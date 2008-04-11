;;; vc-buttons.el --- Create hyperlinks in vc-log-view-mode

;; Copyright (C) 2008 Phil Hagelberg

;; Author: Phil Hagelberg
;; URL: http://www.emacswiki.org/cgi-bin/wiki/VCButtons
;; Version: 0.1
;; Created: 2008-04-08
;; Keywords: vc
;; EmacsWiki: VCButtons

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

;; This code provides hyperlinks for each entry in vc's
;; log-view-mode. Following a hyperlink opens the given revision of
;; the file whose log is being viewed.

;;; Todo:

;; remove runtime cl dependency for submission to GNU Emacs
;; incorporate into log-view.el
;; show diffs as well as files?
;; add regexps for other vc backends
;; make it work with filesets instead of individual files. show a menu of files.

(eval-when-compile (require 'cl))

(defvar vc-button-regexp-alist
  '((Git . "^commit +\\([0-9a-f]\\{40\\}\\)$")
    (SVN . "^r\\([0-9]+\\) ")
    (CVS . "revision \\([0-9]+.[0-9]+\\)"))
  "An alist pairing VC backends to regexps describing what commits look like.")

(defun vc-log-make-buttons ()
  "Make each reference to a commit in the current buffer act as a hyperlink."
  (let* ((buffer-read-only nil)
	 (file (buffer-file-name vc-parent-buffer))
	 (button-regexp (assocref (vc-backend (list file)) vc-button-regexp-alist)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp button-regexp nil t)
	(lexical-let ((cl-file file) ;; closure time!
		      (revision (match-string 1)))
	  (make-text-button (match-beginning 1) (match-end 1)
			    'action (lambda (arg) (interactive)
				      (switch-to-buffer
				       (vc-find-revision cl-file
							 revision)))))))))

(add-hook 'log-view-mode-hook 'vc-log-make-buttons)

(provide 'vc-buttons)
;; vc-buttons.el ends here
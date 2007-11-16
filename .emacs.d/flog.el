;;; flog.el --- Check pain-level of ruby code as you write it

;; Copyright (C) 2007 Phil Hagelberg

;; Author: Phil Hagelberg <technomancy@gmail.com>
;; Created: 2 Aug 2007
;; Version: 0.1
;; Keywords: ruby flog

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

;; Flog is a ruby program that will tell you how much pain your code
;; is in or causing. flog.el will give you feedback within Emacs as
;; you write your code.

;; You will need to have flog installed:
;; # gem install flog

;;; Usage:

;; M-x flog
;; 
;; or
;; 
;; (add-hook 'ruby-mode-hook 'flog-mode) to get flog to run upon saves.

;; Would recommend combining this with `jao-toggle-selective-display'
;; so you can get a quick overview of the file. I like to bind it to
;; M-\, but to each his/her own. See for details:
;; http://emacs.wordpress.com/2007/01/16/quick-and-dirty-code-folding/

;;; Todo

;; * class methods

;;; Code:

(defvar flog-colors '((10 . "green1") ;; Be happy!
		      (20 . "green2")
		      (30 . "green3")
		      (40 . "green4")
		      (50 . "orange1") ;; Start to worry
		      (75 . "orange2")
		      (100 . "orange3") 
		      (125 . "orange4")
		      (150 . "red1") ;; Stop what you're doing and refactor
		      (200 . "red2")
		      (250 . "red3")
		      (300 . "red4")) ;; Ideally only _why should reach this and live
  
  "An alist of flog scores to colors they should be highlighted
  with. Green is better than orange is better than red. Bright is
  better than dark.")

(defvar flogging-current-buffer nil)
(defvar flog-incomplete-line "")

(define-minor-mode flog-mode
  "Minor mode for getting feedback about code \"pain levels\" in Ruby."

  :lighter "-flog"
  (local-set-key "\C-c\C-f" 'flog)
  (local-set-key "\C-c\C-k" 'flog-clear)
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook 'flog))

(defun flog ()
  "Flog the file currently being visited and highlight the methods in
the buffer with their pain levels."
  (interactive)
  (flog-clear)
  (setq flogging-current-buffer (current-buffer))
  (set-process-filter (start-process "flog" nil "flog" "-a" (buffer-file-name)) 'flog-filter))

(defun flog-filter (process output)
  (dolist (line (split-string output "\n"))
    (when (string-match "^\\([A-Za-z]+\\)#\\([^:]+\\): (\\([0-9]+\\)" line)
      (flog-method (match-string 1 line)
		   (match-string 2 line)
		   (string-to-number (match-string 3 line))))))

(defun flog-method (class method score)
  ;; TODO: class is ignored for now
  (if (not (equal "none" method))
      (with-current-buffer flogging-current-buffer
        (save-excursion
          (goto-char (point-min))
          ;; TODO: improve this regex? Need to be able to match class methods
          (search-forward-regexp (concat "def " (regexp-quote method) "[ (\n]") nil t)
	  (ignore-errors
	    (backward-char)
	    (beginning-of-line))
          (overlay-put (make-overlay (point) (progn (end-of-line) (forward-char) (point)))
                       'face (cons 'background-color (flog-color score)))))))

(defun flog-color (score)
  (rest (assoc (flog-nearest-score score flog-colors) flog-colors)))

(defun flog-nearest-score (score scores)
  "Step through the color list finding the closest match."
  (if (> score (caar scores))
      (flog-nearest-score score (cdr scores))
    (caar scores)))

(defun flog-clear ()
  "Remove flog pain level indicators."
  (interactive)
  (remove-overlays))

(provide 'flog)

;;; flog.el ends here
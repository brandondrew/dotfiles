;;; scpaste.el --- Paste to the web via SCP.

;; Copyright (C) 2008 Phil Hagelberg

;; Author: Phil Hagelberg
;; URL: http://www.emacswiki.org/cgi-bin/wiki/SCPaste
;; Version: 0.2
;; Created: 2008-04-02
;; Keywords: convenience hypermedia
;; EmacsWiki: SCPaste

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This will place an HTML copy of a buffer on the web on a server
;; that the user has shell access on.

;; It's similar to services such as http://rafb.net or
;; http://paste.lisp.org but much simpler since it uses `scp' as its
;; transport and uses Emacs' font-lock as its syntax highlighter
;; instead of a third-party syntax highlighter for which individual
;; language support must be added one-by-one.

;; It has been tested in Emacs 23, but should work in 22 and perhaps 21.

;; To invoke: do M-x scpaste, enter a name, and press return. The name
;; will be incorporated into the URL by escaping it and adding it to
;; the end of `scpaste-http-destination'.

;; Run this to generate the splash page: (scpaste "index")

;; You probably want to set up SSH keys for your destination to avoid
;; having to enter your password once for each paste.

;;; Todo:

;; Find some way to list extant pastes
;; Make htmlize convert all URLs to hyperlinks
;;   (currently htmlize-make-hyperlinks just does ones surrounded by <>)

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

;;; Code:

(require 'url)
(require 'htmlize) ;; http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el.html

(defvar scpaste-scp-destination
  "philhag@hagelb.org:p.hagelb.org"
  "Directory to place files via `scp' command.")

(defvar scpaste-http-destination
  "http://p.hagelb.org"
  "Http-accessible location that corresponds to `scpaste-scp-destination'.")

(defvar scpaste-footer
  (concat "<p style='font-size: 6pt; font-family: monospace;'>Generated using "
	  "<a href='http://p.hagelb.org'>scpaste</a> by "
	  user-full-name " at %s.</p>")
  "HTML message to place at the bottom of each file.")

(defvar scpaste-tmp-dir "/tmp"
  "Writable location to store temporary files.")

(defun scpaste (original-name)
  (interactive "MName: ")
  (let* ((b (htmlize-buffer))
	 (name (url-hexify-string original-name))
	 (full-url (concat scpaste-http-destination "/" name ".html"))
	 (scp-destination (concat scpaste-scp-destination "/" name ".html"))
	 (tmp-file (concat scpaste-tmp-dir "/" name)))

    ;; Add the footer
    (save-excursion
      (switch-to-buffer b)
      (goto-char (point-max))
      (search-backward "  </body>\n</html>")
      (insert (format scpaste-footer (current-time-string)))
      (write-file tmp-file))

    ;; Could use shell-command here instead of eshell-command if you don't
    ;; mind the popup password prompt. eshell-command has the disadvantage
    ;; that it breaks save-excursions, so it's a trade-off.
    (eshell-command (concat "scp " tmp-file " " scp-destination))
    
    (kill-new full-url)
    (kill-buffer b)
    (ignore-errors (kill-buffer "*EShell Command Output*"))
    (message "Pasted to %s" full-url)))

(provide 'scpaste)
;;; scpaste.el ends here
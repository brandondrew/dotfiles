;;; rdoc-submit.el --- Send RDoc patches

;; Copyright (C) 2006 Phil Hagelberg <technomancy, gmail>
;;               2005  Edward O'Connor <ted@oconnor.cx>

;; Author: Phil Hagelberg <technomancy, gmail>, Edward O'Connor <ted@oconnor.cx>
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
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

;; Please set `rdoc-username' and `rdoc-password' appropriately.

;;; Code:

(require 'rest-api) ;; http://edward.oconnor.cx/elisp/rest-api.el
(require 'url)      ;; Tested with the URL package in CVS Emacs
(require 'xml)      ;; xml.el in CVS Emacs
(require 'cl)

;; Placate the byte-compiler.
(defvar url-http-end-of-headers)

;; * User-serviceable parts.

(defgroup rdoc nil
  "Emacs interface to Caboose RDoc patch app."
  :group 'processes
  :prefix "rdoc-"
  :link '(url-link :tag "Rdoc"
                   "http://rdoc.caboo.se"))

(defcustom rdoc-username "quentin"
  "Your Rdoc username."
  :group 'rdoc
  :type '(string))

(defcustom rdoc-password "test"
  "Your Rdoc password."
  :group 'rdoc
  :type '(string))

(defcustom rdoc-host "localhost:3333"
  "Host to connect to."
  :group 'rdoc
  :type '(string))

;; * HTTP request/response handling

(put 'rdoc-error 'error-message "Rdoc error")
(put 'rdoc-error 'error-conditions '(rdoc-error error))

(defvar rdoc-debug t)

(defun rdoc-request (path &optional params method)
  "Perform a Rdoc API request to PATH.
PARAMS may contain extra arguments to certain API calls."
  (when (and params (not (stringp params)))
    (setq params (rest-api-xmlify params)))
  (let ((url-package-name "rdoc.el")
        (url-request-method (or method "GET"))
        (url-request-extra-headers '(("Accept" . "text/xml")))
        (url-request-data (or params "")))
    (url-retrieve-synchronously
     (concat "http://" rdoc-host path))))

(defun rdoc-login ()
  (rdoc-request "/sessions/create" (concat "login=" rdoc-username "&password=" rdoc-password) "POST"))

(defun rdoc-get-file-list ()
  (switch-to-buffer (rdoc-request "/doc/fr_file_index.html"))
  (let ((files ()))
    (while (search-forward-regexp "\\(files/[^\"]*\\)" nil t)
      (add-to-list 'files (buffer-substring (car (match-data)) (cadr (match-data)))))
    (kill-buffer (current-buffer))
    files))


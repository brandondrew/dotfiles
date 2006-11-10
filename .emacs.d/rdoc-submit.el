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

;; Please log on manually first and tell your openid server to always
;; allow rdoc to authenticate. Then set `openid-url' appropriately.

;;; Code:

(require 'rest-api) ;; http://edward.oconnor.cx/elisp/rest-api.el
(require 'url)      ;; Tested with the URL package in CVS Emacs
(require 'xml)      ;; xml.el in CVS Emacs
(require 'cl)

;; Placate the byte-compiler.
(defvar url-http-end-of-headers)

;; * User-serviceable parts.

(defcustom openid-url "quentin.myopenid.com"
  "Your openID url."
  :group 'rdoc
  :type '(string))

(defcustom openid-username "quentin"
  "Your openID username."
  :group 'rdoc
  :type '(string))

(defcustom openid-password "testy"
  "Your openID password."
  :group 'rdoc
  :type '(string))

(defgroup rdoc nil
  "Emacs interface to Caboose RDoc patch app."
  :group 'processes
  :prefix "rdoc-"
  :link '(url-link :tag "Rdoc"
                   "http://rdoc.caboo.se"))

(defcustom rdoc-host "localhost:3333"
  "Host to connect to."
  :group 'rdoc
  :type '(string))

;; * HTTP request/response handling

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
  (interactive)
  (rdoc-request "/sessions" (concat "openid_url=" openid-url) "POST"))

;; (let ((url-request-method "POST")
;;       (url-request-data "openid_url=quentin.myopenid.com"))
;;   (url-retrieve-synchronously "http://localhost:3333/sessions")
;;   (let ((url-request-data (concat "user_name=quentin&password=testy")))
;;     (url-retrieve-synchronously "http://myopenid.com/signin_submit")))

(defun rdoc-get-file-list ()
  (switch-to-buffer (rdoc-request "/doc/fr_file_index.html"))
  (let ((files ()))
    (while (search-forward-regexp "\\(files/[^\"]*\\)" nil t)
      (add-to-list 'files (buffer-substring (+ 6 (car (match-data))) (cadr (match-data)))))
    (kill-buffer (current-buffer))
    files))

(defun rdoc-get-class-list ()
  (switch-to-buffer (rdoc-request "/doc/fr_class_index.html"))
  (let ((classes ()))
    (while (search-forward-regexp "\\(classes/[^\"]*\\)" nil t)
      (add-to-list 'classes (buffer-substring (+ 8 (car (match-data))) (cadr (match-data)))))
    (kill-buffer (current-buffer))
    classes))

(defun rdoc-find-file (file)
  (interactive (list (completing-read "Open RDoc for file: " 
				      (rdoc-get-file-list))))
  (rdoc-prepare-buffer (rdoc-request (concat "/doc/files/" file)) file))

(defun rdoc-find-class (class)
  (interactive (list (completing-read "Open RDoc for class: " 
				      (rdoc-get-class-list))))
  (rdoc-prepare-buffer (rdoc-request (concat "/doc/classes/" class)) class))

(defun rdoc-find-method (method)
  ;; TODO: write
)

(defun rdoc-submit ()
  (interactive)
  (rdoc-request "/patches/new" (concat "patch[filename]=" filename "&patch[original_rdoc_source]=" 
				       original-rdoc-source "&patch[line]=0&patch[modified_rdoc_source]=" 
				       (buffer-string)) "POST"))

(defun rdoc-prepare-buffer (buffer name)
  (switch-to-buffer buffer)
  (rename-buffer name t)

  ;; Delete HTML and leave us with just the contents of the textarea
  (delete-region 1
		 (search-forward-regexp "<textarea [^>]*>"))
  (delete-region (- (search-forward-regexp "</textarea>") 11)
		 (buffer-end 1))
  (beginning-of-buffer)

  (rdoc-edit-mode)

  (buffer-enable-undo))

(define-derived-mode rdoc-edit-mode
  text-mode "RDoc"
  "RDoc editing mode"
  (set (make-local-variable 'filename) (buffer-name))
  (set (make-local-variable 'original-rdoc-source) (buffer-string))

  (interactive))

(define-key rdoc-edit-mode-map
  "\C-c\C-c" 'rdoc-submit)

(define-key rdoc-edit-mode-map
  "C-c\C-f" 'rdoc-find-file)
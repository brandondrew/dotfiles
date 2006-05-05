
;; Copyright (C) 2006 Phil Hagelberg

;; Author: Phil Hagelberg
;; URL: http://dev.technomancy.us/phil/wiki/ebby

;;; License:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Description:

;; Obby is a protocol that allows for collaborative editing. (See
;; http://darcs.0x539.de/trac/obby/cgi-bin/trac.cgi) Currently the
;; only other editor that supports Obby is Gobby, a multiplatform GTK
;; client. Ebby is meant to bring Obby client support to Emacs.  Note
;; that Ebby currently supports version 0.3 of the protocol.

;;; Usage

;; M-x ebby           - connects you to the server
;; M-x ebby-subscribe - subscribes to a document

;;; To do

;; Style issues
;;  * Write case-string macro (to replace ugly conds)
;;  * Quit using setq!
;;  * Store more in buffer-local variables than document-table?

;; Bugs
;;  * Gobby crashes randomly... (sigh)

;; See http://dev.technomancy.us/phil/report/13 for a ticket list

;;; Not to do

;;  * Become a server (unless someone else wants to write it) 
;;  * Multiple Obby servers (Even Gobby doesn't do this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Init

;(global-set-key [f7] (lambda () (interactive) (message "%s" (point))))

(defgroup ebby nil
  "Implementation of the obby collaborative editing protocol"
  :version "22.1" ; not sure what to put here...
  :prefix "ebby-"
  :group 'applications)

(defcustom ebby-default-server "localhost"
  "Server to connect to."
  :type 'string
  :group 'ebby)

(defcustom ebby-default-name user-login-name
  "Your user name."
  :type 'string
  :group 'ebby)

(defcustom ebby-default-color "88ff44"
  "Hexadecimal color to distinguish your text."
  :type 'string
  :group 'ebby)

(defcustom ebby-default-port 6522
  "Default port to connect to."
  :type 'integer
  :group 'ebby)

(setq client-table (make-hash-table :test 'equal)) ; clients referenced by net6-user-id

(setq document-table (make-hash-table :test 'equal)) ; documents referenced by doc-id string (owner id + index)

(defvar *user-id* nil)
(defvar *user-name* nil)

;; This is a flag so that incoming changes are not transmitted as local changes
(defvar ebby-incoming-change nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Connection:

(defalias 'ebby 'ebby-connect)

(defun ebby-connect (&optional server name color port)
  (interactive)
  (save-excursion
    (let* ((server (or server (read-string "Server: " "localhost")))
	   (name (or name (read-string "Name: " ebby-default-name)))
	   (color (or color (read-string "Hexadecimal Color: " ebby-default-color)))
	   (port-number (if port
			    (if (stringp port)
				(string-to-number port)
			      port)
			  (string-to-number (read-string "Port: " (number-to-string ebby-default-port)))))
           (process (open-network-stream server nil server port-number)))

      (setq *user-name* name)
      (message "Connecting to %s..." server)
      
      ;; set up process
      (set-process-coding-system process 'raw-text 'raw-text)
      (set-process-filter process 'ebby-filter)

      ;; associate with a buffer
      (switch-to-buffer "*ebby*")
      (set-process-buffer process (current-buffer))

      ;; log in
      (ebby-send-string (concat "net6_client_login:" name ":" color)))))

(defun ebby-filter (process output)
  (set-buffer "*ebby*")
  (insert output)
  (dolist (line (split-string output "\n"))
    (ebby-filter-line process line)))

(defun ebby-filter-line (process line)
  (setq tokens (split-string line ":"))
  (cond
    ((equal (car tokens) "obby_welcome") (apply 'ebby-welcome (cdr tokens)))
    ((equal (car tokens) "obby_sync_final") (message "Logged in."))
    ((equal (car tokens) "net6_client_join") (apply 'ebby-client-join (cdr tokens)))
    ((equal (car tokens) "net6_client_part") (apply 'ebby-client-part (cdr tokens)))
    ((equal (car tokens) "obby_sync_doclist_document") (apply 'ebby-synch-doclist-document (cdr tokens)))
    ((equal (car tokens) "obby_document_create") (apply 'ebby-document-create (cdr tokens)))
    ((equal (car tokens) "obby_document") (apply 'ebby-document-handler (cdr tokens)))))

(defun ebby-send-string (string &optional process)
  (unless process (setq process (get-buffer-process "*ebby*")))
  (unless (eq (process-status process) 'open)
    (error "Network connection to %s is not open"
	   (process-name process)))
  (process-send-string process (concat string "\n")))

(defun ebby-subscribe (&optional doc-id)
  (interactive)
  (let ((doc-id (or doc-id (read-string "Document id: " "1 1"))))
    (ebby-set-doc-remote-count doc-id 0)
    (ebby-set-doc-local-count doc-id 0)
    (ebby-send-string (concat "obby_document:" doc-id ":subscribe:" *user-id*))))

(defun ebby-unsubscribe (&optional doc-id)
  (interactive)
  (setq doc-id (or doc-id this-doc-id))
  (if (get-buffer-process "*ebby*")
      (ebby-send-string (concat "obby_document:" doc-id ":unsubscribe"))))

(defun ebby-resubscribe (&optional doc-id)
  (interactive "MDocument id: ")
  (setq doc-id (or doc-id this-doc-id))
  (kill-buffer (concat "ebby-" (ebby-doc-id-to-name doc-id)))
  (ebby-subscribe doc-id))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General Handlers

(defun ebby-welcome (protocol-version &rest args)
  (unless (equal "5" protocol-version) 
    (message "Warning: incompatible version of obby protocol: %s" protocol-version)))


(defun ebby-client-join (net6-user-id name obby-user-id color)
  ;; add client to client-table
  (if (equal name *user-name*)
      (setq *user-id* obby-user-id)) ; only chance at getting our own user ID
  (puthash net6-user-id (list :net6-id net6-user-id :name name :obby-id obby-user-id :color color) client-table))

(defun ebby-client-part (net6-user-id)
  ;; drop client from client-table
  (remhash net6-user-id client-table))

(defun ebby-synch-doclist-document (obby-user-id doc-index doc-name &rest users)
  ;; add document to document-table
  (ebby-document-create obby-user-id doc-index doc-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Document Handlers

(defun ebby-document-handler (doc-id command &rest args)
  (cond
    ((equal command "sync_init") (apply 'ebby-document-sync-init doc-id args))
    ((equal command "sync_line") (apply 'ebby-document-sync-line doc-id args))
    ((equal command "subscribe") (ebby-document-subscribe doc-id))
    ((equal command "record") (apply 'ebby-document-record doc-id args))))

(defun ebby-document-sync-init (document-id total-line-count)
  (let ((document (gethash document-id document-table)))

    (switch-to-buffer (concat "ebby-" (getf document :name)))

    (make-local-variable 'this-doc-id)
    (setq this-doc-id document-id)

    (setq line-count 0) ; for sync-line below
    (setq total-lines (string-to-number total-line-count 16))

    (add-hook 'after-change-functions 'ebby-change-hook nil t)
    (add-hook 'kill-buffer-hook 'ebby-unsubscribe nil t)

    (make-local-variable 'subscribed-users)))

(defun ebby-document-sync-line (doc-id &optional line &rest args)
  (when line
      (setq ebby-incoming-change t)
      (end-of-buffer)
      (incf line-count)
      (ebby-document-record-ins doc-id nil (concat line (unless (= line-count total-lines) "\n")))
      (setq ebby-incoming-change nil)))

(defun ebby-document-create (doc-owner-id doc-count doc-name)
  "Add document to document-table"
  (puthash (concat doc-owner-id " " doc-count) ; key
	   (list :owner doc-owner-id :index doc-count :name doc-name :users () 
		 :remote-count 0 :local-count 0)
	   document-table))

(defun ebby-document-subscribe (doc-id)
  (beginning-of-buffer)
  (message "Subscribed to %s" (ebby-doc-id-to-name doc-id)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Document Record Handlers

(defun ebby-document-record (doc-id user-id remote-count local-count command &rest args)
  (ebby-set-doc-remote-count doc-id (string-to-number remote-count 16))
  (setq ebby-incoming-change t)
  (cond
    ((equal "ins" command) (apply 'ebby-document-record-ins doc-id args))
    ((equal "del" command) (apply 'ebby-document-record-del doc-id args)))
  (setq ebby-incoming-change nil))

(defun ebby-document-record-ins (doc-id position string)
  (set-buffer (concat "ebby-" (ebby-doc-id-to-name doc-id)))
  (save-excursion
    (if position
	(goto-char (+ (string-to-number position 16) 1))) ; obby starts at zero
    (insert (ebby-unescape string))))

(defun ebby-document-record-del (doc-id position char-count)
  (set-buffer (concat "ebby-" (ebby-doc-id-to-name doc-id)))
  (save-excursion
    (goto-char (+ (string-to-number position 16) 1))
    (delete-char (string-to-number char-count 16))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Document functions

(defun ebby-doc-id-to-name (doc-id)
  (getf (gethash doc-id document-table) :name))

(defun ebby-unescape (char)
  (cond ((equal char "\\n") "\n")
	((equal char "\\d") ":")
	(t char)))

(defun ebby-escape (char)
  (cond ((equal char "\n") "\\n")
	((equal char ":") "\\d")
	(t char)))

(defun ebby-get-doc-local-count (doc-id)
  (getf (gethash doc-id document-table) :local-count))

(defun ebby-set-doc-local-count (doc-id local-count)
  (setf (getf (gethash doc-id document-table) :local-count) local-count))

(defun ebby-inc-doc-local-count (doc-id)
  (ebby-set-doc-local-count doc-id (+ (ebby-get-doc-local-count doc-id) 1)))

(defun ebby-get-doc-remote-count (doc-id)
  (getf (gethash doc-id document-table) :remote-count))

(defun ebby-set-doc-remote-count (doc-id remote-count)
  (setf (getf (gethash doc-id document-table) :remote-count) remote-count))

(defun ebby-inc-doc-remote-count (doc-id)
  (ebby-set-doc-remote-count doc-id (+ (ebby-get-doc-remote-count doc-id) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sending

(defun ebby-send-ins (doc-id string position)
  (ebby-send-string (concat "obby_document:" doc-id ":record:" 
			    (format "%x" (ebby-get-doc-local-count doc-id)) 
			    ":" (format "%x" (ebby-get-doc-remote-count doc-id)) 
			    ":ins:" (format "%x" position) ":" 
			    string))
  (message "local: %s remote: %s" (ebby-get-doc-local-count doc-id) (ebby-get-doc-remote-count doc-id)) 
  (ebby-inc-doc-local-count doc-id))

(defun ebby-send-del (doc-id position &optional length)
  (ebby-send-string (concat "obby_document:" doc-id ":record:" 
			    (format "%x" (ebby-get-doc-local-count doc-id)) 
			    ":" (format "%x" (ebby-get-doc-remote-count doc-id)) 
			    ":del:" (format "%x" position) ":" (format "%x" (or length 1))))
  (ebby-inc-doc-local-count doc-id))

(defun ebby-change-hook (begin end length)
  (setq change (list begin end length))
  (setq text (ebby-escape (buffer-substring begin end)))
  (unless ebby-incoming-change ; mutex
    (if (< 0 length)
	;; deletion
	(ebby-send-del this-doc-id (- begin 1) length)
      ;; insertion
      (ebby-send-ins this-doc-id (ebby-escape (buffer-substring begin end)) (- begin 1)))))

(provide 'ebby)
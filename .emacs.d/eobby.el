
;; Copyright (C) 2006 Phil Hagelberg

;; Author: Phil Hagelberg
;; URL: http://dev.technomancy.us/phil/wiki/eobby

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Init

(defgroup eobby nil
  "Implementation of the obby collaborative editing protocol"
  :version "22.1" ; not sure what to put here...
  :prefix "eobby-"
  :group 'applications)

(defcustom eobby-default-port 6522
  "Default port to connect to."
  :type 'integer
  :group 'eobby)

(defcustom eobby-default-name (user-login-name)
  "Your user name."
  :type 'string
  :group 'eobby)

(defvar client-table (make-hash-table)) ; clients referenced by net6-user-id

(defvar document-table (make-hash-table)) ; documents referenced by doc-id string (owner id + index)

(defvar *user-id* nil)
(defvar *user-name* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Connection:

(defun eobby-connect (server name color &optional port)
  (save-excursion
    (message "Connecting to %s..." server)
    (let* ((port-number (if port
			    (if (stringp port)
				(string-to-number port)
			      port)
			  eobby-default-port))
	   (name (or name eobby-default-name))
           (process (open-network-stream server nil server port-number)))

      (setq *user-name* name)
      (message "Connected.")
      
      ;; set up process
      (set-process-coding-system process 'raw-text 'raw-text)
      (set-process-filter process 'eobby-filter)

      ;; associate with a buffer?
      (switch-to-buffer "eobby")
      (set-process-buffer process (current-buffer))

      ;; log in
      (eobby-send-string (concat "net6_client_login:" name ":" color)))))

(defun eobby-filter (process output)
  (setq lines (split-string output "\n"))
  (set-buffer "eobby")
  (insert output)
  (dolist (line lines)
    (eobby-filter-line process line)))

(defun eobby-filter-line (process line)
  (setq tokens (split-string line ":"))
  (unless (equal (car tokens) "") (setq blah tokens))
  (cond
    ((equal (car tokens) "obby_welcome") (eobby-welcome (cdr tokens)))
    ((equal (car tokens) "obby_sync_init")) ; well? what's it for?
    ((equal (car tokens) "obby_sync_final")) ; right...
    ((equal (car tokens) "net6_client_join") (eobby-client-join (cdr tokens)))
    ((equal (car tokens) "net6_client_part") (eobby-client-part (cdr tokens)))
    ((equal (car tokens) "obby_sync_doclist_document") (eobby-synch-doclist-document (cdr tokens)))
    ((equal (car tokens) "obby_document_create") (eobby-document-create (cdr tokens)))
    ((equal (car tokens) "obby_document") (eobby-document-handler (cdr tokens)))
    ;; more functions here
    ))

(defun eobby-send-string (string &optional process)
  (unless process (setq process (get-buffer-process "eobby")))
  (unless (eq (process-status process) 'open)
    (error "Network connection to %s is not open"
	   (process-name process)))
  (process-send-string process (concat string "\n")))

(defun eobby-subscribe (author document)
  (eobby-send-string (concat "obby_document:" (get-document-id document) ":subscribe:" *user-id*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General Handlers

(defun eobby-welcome (protocol-version)
  ;; should throw an error if the protocol version is wrong
  )

(defun eobby-client-join (args)
  (setq net6-user-id (pop args))
  (setq name (pop args))
  (setq obby-user-id (pop args))
  (setq color (pop args))
  ;; add client to client-table
  (if (equal name *user-name*)
      (setq *user-id* obby-user-id))
;  (puthash net6-user-id (:net6-id net6-user-id :name name :obby-id obby-user-id :color color) client-table))
)

(defun eobby-client-part (net6-user-id)
  ;; drop client from client-table
  (remhash net6-user-id client-table))

(defun eobby-synch-doclist-document (obby-user-id doc-index doc-name &rest users)
  ;; add document to document-table
  ;; TODO: add users to document
  (eobby-document-create obby-user-id doc-index doc-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Document Handlers

(defun eobby-document-handler (args)
  (setq doc-id (pop args))
  (setq command (pop args))
  (cond
    ((equal command "sync_init") (eobby-document-sync-init doc-id args))
    ((equal command "sync_line") (eobby-document-sync-line doc-id args))
    ((equal command "subscribe") (eobby-document-subscribe doc-id args))
    ((equal command "record") (eobby-document-record doc-id args))))

(defun eobby-document-sync-init ()
)

(defun eobby-document-sync-line (line number number2)
)

(defun eobby-document-create (args)
  (setq doc-owner-id (pop args))
  (setq doc-count (pop args))
  (setq doc-name (pop args))
  ;; add document to document-table
  (puthash (concat (number-to-string doc-owner-id) " " (number-to-string doc-count)) ; key
	   (:owner doc-owner-id :index doc-count :name doc-name :users ())
	   document-table))

(defun eobby-document-subscribe (doc-owner-id doc-count doc-name)
  (switch-to-buffer (concat "eobby-" doc-name))
  (make-local-variable 'owner-id)
  (setq owner-id doc-owner-id)
  (make-local-variable 'doc-index)
  (setq doc-index doc-count)
  (make-local-variable 'subscribed-users)
  (setq subscribed-users ())
  (message (concat "Subscribed to " doc-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Document Record Handlers

(defun eobby-document-record (doc-id args)
  (setq user-id (pop args))
  (setq version (pop args))
  (setq zero (pop args))
  (setq command (pop args))
  (cond
    ((equal "ins" command) (eobby-document-record-ins doc-id args))
    ((equal "del" command) (eobby-document-record-del doc-id args))))

(defun eobby-document-record-ins (doc-id args)
  (setq position (pop args))
  (setq char (pop args))
;  (save-excursion
    (set-buffer (doc-id-to-buffer doc-id))
    (goto-char (+ (string-to-number position 16) 1)) ; obby starts at zero
    (insert char));)

(defun eobby-document-record-del (position number)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Document function

(defun get-document-id (document)
)

(defun doc-id-to-buffer (doc-id)
  (gethash doc-id document-table)
  "eobby-first") ; for now...

(provide 'eobby)

;(eobby-connect "127.0.0.1" "eobby-test" "00ff00" 6523)
;(eobby-send-string "obby_document:1 1:subscribe:2")

;; Copyright (C) 2006 Phil Hagelberg

;; Author: Phil Hagelberg
;; URL: http://dev.technomancy.us/phil/wiki/ebby

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

;; TODO

;; write case-string macro
;; quit using setq!
;; more... (search for TODO below)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Init

(defgroup ebby nil
  "Implementation of the obby collaborative editing protocol"
  :version "22.1" ; not sure what to put here...
  :prefix "ebby-"
  :group 'applications)

(defcustom ebby-default-port 6522
  "Default port to connect to."
  :type 'integer
  :group 'ebby)

(defcustom ebby-default-name (user-login-name)
  "Your user name."
  :type 'string
  :group 'ebby)

(setq client-table (make-hash-table :test 'equal)) ; clients referenced by net6-user-id

(setq document-table (make-hash-table :test 'equal)) ; documents referenced by doc-id string (owner id + index)

(defvar *user-id* nil)
(defvar *user-name* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Connection:

(defun ebby-connect (server name color &optional port)
  (save-excursion
    (message "Connecting to %s..." server)
    (let* ((port-number (if port
			    (if (stringp port)
				(string-to-number port)
			      port)
			  ebby-default-port))
	   (name (or name ebby-default-name))
           (process (open-network-stream server nil server port-number)))

      (setq *user-name* name)
      (message "Connected.")
      
      ;; set up process
      (set-process-coding-system process 'raw-text 'raw-text)
      (set-process-filter process 'ebby-filter)

      ;; associate with a buffer?
      (switch-to-buffer "ebby")
      (set-process-buffer process (current-buffer))

      ;; log in
      (ebby-send-string (concat "net6_client_login:" name ":" color)))))

(defun ebby-filter (process output)
  (setq lines (split-string output "\n"))
  (set-buffer "ebby")
  (insert output)
  (dolist (line lines)
    (ebby-filter-line process line)))

(defun ebby-filter-line (process line)
  (setq tokens (split-string line ":"))
  (cond
    ((equal (car tokens) "obby_welcome") (apply 'ebby-welcome (cdr tokens)))
;    ((equal (car tokens) "obby_sync_init")) ; well? what's it for?
;    ((equal (car tokens) "obby_sync_final")) ; right...
    ((equal (car tokens) "net6_client_join") (apply 'ebby-client-join (cdr tokens)))
    ((equal (car tokens) "net6_client_part") (apply 'ebby-client-part (cdr tokens)))
    ((equal (car tokens) "obby_sync_doclist_document") (apply 'ebby-synch-doclist-document (cdr tokens)))
    ((equal (car tokens) "obby_document_create") (apply 'ebby-document-create (cdr tokens)))
    ((equal (car tokens) "obby_document") (apply 'ebby-document-handler (cdr tokens)))
    ;; more functions here
    ))

(defun ebby-send-string (string &optional process)
  (unless process (setq process (get-buffer-process "ebby")))
  (unless (eq (process-status process) 'open)
    (error "Network connection to %s is not open"
	   (process-name process)))
  (process-send-string process (concat string "\n")))

(defun ebby-subscribe (author document)
  (ebby-send-string (concat "obby_document:" (get-document-id document) ":subscribe:" *user-id*)))

(defun ebby-unsubscribe (doc-id)
  ;; TODO
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General Handlers

(defun ebby-welcome (protocol-version &rest args)
  ;; TODO: should throw an error if the protocol version is wrong
  )

(defun ebby-client-join (net6-user-id name obby-user-id color)
  ;; add client to client-table
  (if (equal name *user-name*)
      (setq *user-id* obby-user-id)) ; only chance at getting our own user ID
  (puthash net6-user-id '(:net6-id net6-user-id :name name :obby-id obby-user-id :color color) client-table))

(defun ebby-client-part (net6-user-id)
  ;; drop client from client-table
  (remhash net6-user-id client-table))

(defun ebby-synch-doclist-document (obby-user-id doc-index doc-name &rest users)
  ;; add document to document-table
  ;; TODO: add users to document
  (ebby-document-create obby-user-id doc-index doc-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Document Handlers

(defun ebby-document-handler (doc-id command &rest args)
  (setq blah args)
  (cond
    ((equal command "sync_init") (apply 'ebby-document-sync-init doc-id args))
    ((equal command "sync_line") (apply 'ebby-document-sync-line doc-id args))
    ((equal command "subscribe") (apply 'ebby-document-subscribe doc-id))
    ((equal command "record") (apply 'ebby-document-record doc-id args))
    ;; TODO determine complete list of commands, write case-string
))

(defun ebby-document-sync-init (&rest args)
  ;; TODO write? do we care about this at all?
)

(defun ebby-document-sync-line (&rest args)
  ;; TODO write
  ;; necessary for subscribing to documents that already have content.
)

(defun ebby-document-create (doc-owner-id doc-count doc-name)
  "Add document to document-table"
  (puthash (concat doc-owner-id " " doc-count) ; key
	   '(:owner doc-owner-id :index doc-count :name doc-name :users ())
	   document-table))

(defun ebby-document-subscribe (doc-id)
  (setq document (gethash "1 1" document-table))

  (switch-to-buffer (concat "ebby-" (getf document :name)))

  ;; not sure what we do with these, but it's good to store them, i guess
  (make-local-variable 'owner-id)
  (setq owner-id (getf document :name))

  (make-local-variable 'doc-index)
  (setq doc-index (getf document :index))

  (make-local-variable 'subscribed-users)

  (message (concat "Subscribed to " doc-name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Document Record Handlers

(defun ebby-document-record (doc-id user-id version zero command &rest args)
  (cond
    ((equal "ins" command) (apply 'ebby-document-record-ins doc-id args))
    ((equal "del" command) (apply 'ebby-document-record-del doc-id args))
    ;; TODO: determine if there are more functions
    ))

(defun ebby-document-record-ins (doc-id position char)
  (save-excursion
    (set-buffer (doc-id-to-buffer doc-id))
    (goto-char (+ (string-to-number position 16) 1)) ; obby starts at zero
    (insert (ebby-unescape char))))

(defun ebby-document-record-del (doc-id position char-count)
  (save-excursion
    (set-buffer (doc-id-to-buffer doc-id))
    (goto-char (+ (string-to-number position 16) 1)) ; obby starts at zero
    (delete-char (string-to-number char-count 16))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Document function

(defun get-document-id (doc-name)
  ;; TODO write if needed
  )

(defun doc-id-to-buffer (doc-id)
  (getf (gethash doc-id document-table) :name))

(defun ebby-unescape (char)
  (cond ((equal char "\\n") "\n")
	((equal char "\\d") ":")
	(t char)))

(provide 'ebby)

;(ebby-connect "192.168.1.44" "ebby-test" "ff0000" 6524)
;(ebby-send-string "obby_document:1 1:subscribe:2")

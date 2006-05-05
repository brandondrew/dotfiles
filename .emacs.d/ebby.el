
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

;;; Description:

;; Obby is a protocol that allows for collaborative editing. (See
;; http://darcs.0x539.de/trac/obby/cgi-bin/trac.cgi) Currently the
;; only editor that supports Obby is Gobby, a multiplatform GTK
;; client. Ebby is meant to bring Obby client support to Emacs.

;;; To do

;; Style issues
;;  * Write case-string macro
;;  * Quit using setq!

;; Minor features
;;  * Unsubscribe on buffer kill
;;  * Keep point where it should be on insertion/deletion
;;  * Allow color changing for self and other users?

;; Major features
;;  * Transmit edit information (high priority)
;;  * Color text based on user
;;  * Port to Obby 0.4, TLS (later)

;; more... (search for TODO below)

;;; Not to do

;;  * Become a server (unless someone else wants to write it) 
;;  * Chatting (don't see the point) 
;;  * Multiple Obby servers (Even Gobby doesn't do this)

;; If someone else wants these features, they can implement them; I
;; just don't feel the need to do them myself

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
  (interactive "BConnect to: \nBName: \nBColor (hexadecimal):")
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

      ;; associate with a buffer
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
    ((equal (car tokens) "obby_document") (apply 'ebby-document-handler (cdr tokens)))))

(defun ebby-send-string (string &optional process)
  (unless process (setq process (get-buffer-process "ebby")))
  (unless (eq (process-status process) 'open)
    (error "Network connection to %s is not open"
	   (process-name process)))
  (process-send-string process (concat string "\n")))

(defun ebby-subscribe (doc-id)
  (ebby-send-string (concat "obby_document:" doc-id ":subscribe:" *user-id*)))

(defun ebby-unsubscribe (doc-id)
  ;; TODO tell the server and kill the buffer
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
  (puthash net6-user-id (list :net6-id net6-user-id :name name :obby-id obby-user-id :color color) client-table))

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
    ((equal command "subscribe") (ebby-document-subscribe doc-id))
    ((equal command "record") (apply 'ebby-document-record doc-id args))))

(defun ebby-document-sync-init (&rest args)
  (setq document (gethash doc-id document-table))

  (switch-to-buffer (concat "ebby-" (getf document :name)))

  ;; not sure what we do with these, but it's good to store them, i guess
  (make-local-variable 'owner-id)
  (setq owner-id (getf document :name))

  (make-local-variable 'doc-index)
  (setq doc-index (getf document :index))

  (make-local-variable 'subscribed-users))

(defun ebby-document-sync-line (doc-id &optional line zero user-id)
  (if line
      (ebby-document-record-ins doc-id nil line)))

(defun ebby-document-create (doc-owner-id doc-count doc-name)
  "Add document to document-table"
  (puthash (concat doc-owner-id " " doc-count) ; key
	   (list :owner doc-owner-id :index doc-count :name doc-name :users ())
	   document-table))

(defun ebby-document-subscribe (doc-id)
  (message "Subscribed to %s" (ebby-doc-id-to-name doc-id)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Document Record Handlers

(defun ebby-document-record (doc-id user-id version zero command &rest args)
  (cond
    ((equal "ins" command) (apply 'ebby-document-record-ins doc-id args))
    ((equal "del" command) (apply 'ebby-document-record-del doc-id args))))    ))

(defun ebby-document-record-ins (doc-id position string)
  (save-excursion
    (set-buffer (concat "ebby-" (ebby-doc-id-to-name doc-id)))
    (if position
	(goto-char (+ (string-to-number position 16) 1))) ; obby starts at zero
    (insert (ebby-unescape string))))

(defun ebby-document-record-del (doc-id position char-count)
  (save-excursion
    (set-buffer (concat "ebby-" (ebby-doc-id-to-name doc-id)))
    (goto-char (+ (string-to-number position 16) 1)) ; obby starts at zero
    (delete-char (string-to-number char-count 16))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Document functions

(defun ebby-doc-id-to-name (doc-id)
  (getf (gethash doc-id document-table) :name))

(defun ebby-unescape (char)
  (cond ((equal char "\\n") "\n")
	((equal char "\\d") ":")
	(t char)))

(provide 'ebby)

; To use:
;(ebby-connect "localhost" "ebby-test" "ff0000" 6522)
;(ebby-subscribe "1 1")


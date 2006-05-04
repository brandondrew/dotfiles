
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

;;; Code:

(defgroup eobby nil
  "Implementation of the obby collaborative editing protocol"
  :version "22.1"
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

(defvar client-table nil)

(defvar document-table nil)

(defvar *process* nil)

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
      
      ;; set up process
      (set-process-coding-system process 'raw-text 'raw-text)
      (set-process-filter process 'eobby-filter)

      ;; associate with a buffer?
      (switch-to-buffer "eobby")
      (set-process-buffer process (current-buffer))

      ;; log in
      (eobby-send-string (concat "net6_client_login:" name ":" color) process))))


;;; Input!
(defun eobby-filter (process output)
  (setq lines (split-string output "\n"))
  (dolist (line lines)
    (eobby-filter-line process line))
    (set-buffer "eobby")
    (insert output))

(defun eobby-filter-line (process line)
  (setq tokens (split-string line ":"))
  (case (car tokens)
    ("obby_welcome" (eobby-welcome (cdr tokens)))
    ("obby_sync_init") ; well? what's it for?
    ("obby_sync_final") ; right...
    ("net6_client_join" (eobby-client-join (cdr tokens)))
    ("net6_client_part" (eobby-client-part (cdr tokens)))
    ("obby_sync_doclist_document" (eobby-synch-doclist-document (cdr tokens)))
    ("obby_document_create" (eobby-document-create (cdr tokens)))
    ("obby_document" (eobby-document-handler (cdr tokens)))
    ;; add functions here
    ))

(defun eobby-welcome (protocol-version)
  ;; should throw an error if the protocol version is wrong
  )

(defun eobby-client-join (net6-user-id name obby-user-id color)
  ;; add client to client-table
  )

(defun eobby-client-part (net6-user-id)
  ;; add client to client-table
  )

(defun eobby-synch-doclist-document (obby-user-id doc-count doc-name (users that are connected))
  ;; add document to document-table
)

(defun eobby-document (doc-name command &rest args)
  (case command
    ("sync_init" (eobby-document-sync-init args))
    ("sync_line" (eobby-document-sync-line args))
    ("subscribe")
    ("record" (eobby-document-record args))))

(defun eobby-document-sync-init ()
)

(defun eobby-document-sync-line (line number number2)
)

(defun eobby-document-create (obby-user-id doc-count doc-name)
)

(defun eobby-document-record (user-id version zero command &rest args)
  (case command
    ("ins" eobby-document-record-ins args)
    ("del" eobby-document-record-del args)
))

(defun eobby-document-record-ins (position char)
)

(defun eobby-document-record-del (position number)
)

;;; Output!

(defun eobby-send-string (string &optional process)
  "Send PROCESS a STRING plus a newline."
  (unless (eq (process-status process) 'open)
    (error "Network connection to %s is not open"
	   (process-name process)))
  (process-send-string process (concat string "\n")))

(defun eobby-subscribe (author document)
  (eobby-send-string (concat "obby_document:" (get-document-id document) ":subscribe:" *user-id*)))

(provide 'eobby)

;(eobby-connect "192.168.1.44" "phile" "00ff00")

;;; hate.el --- Emacs interface to Utu

;; Copyright (C) 2007 Phil Hagelberg

;; Author: Phil Hagelberg
;; URL: http://dev.technomancy.us/wiki/utu.el

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

;; "The Internet needs more hate.  Much more." -- Zed A. Shaw

;; Utu is a communications system that hopes to improve the quality of
;; communications on the Internet by introducing cryptographic models
;; of processes found in existing human social interactions.

;;; Usage

;; M-x utu to connect

;;; To do

;; Everything! (practically)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Init

(eval-when-compile
  (require 'cl))

(defgroup utu nil
  "Interface to the Utu communications protocol."
  :version "22.1"
  :prefix "utu-"
  :group 'applications)

(defcustom utu-default-server "myutu.name"
  "Server to connect to."
  :type 'string
  :group 'utu)

(defcustom utu-default-name (user-login-name)
  "Your user name."
  :type 'string
  :group 'utu)

(defcustom utu-default-port 10000
  "Default port to connect to."
  :type 'integer
  :group 'utu)

(defcustom utu-initial-room "help"
  "Default room to join."
  :type 'string
  :group 'utu)

(defcustom utu-debug nil
  "Toggle for Utu debugging info"
  :type 'boolean
  :group 'utu)

(defvar utu-user-name)
(defvar utu-user-key) ; Mendicant may generate automatically
(defvar utu-host-key) ; Ignore for now
(defvar utu-buffer-name)

(defvar utu-send-count 0)
(defvar utu-recv-count 0)
(defvar utu-config) ;; hash?
(defvar utu-socket-name)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The fun starts here

(defalias 'utu 'utu-connect)

(defun utu-connect (&optional server name port room)
  "Connect to an Utu server."
  (interactive)
  (save-excursion
    (let* ((server (or server (read-string "Server: " utu-default-server)))
	   (name (or name (read-string "Name: " utu-default-name)))
	   (room (or room (read-string "Room: " utu-initial-room)))
	   (port-number (if port
			    (if (stringp port)
				(string-to-number port)
			      port)
			  (string-to-number 
			   (read-string "Port: " 
					(number-to-string utu-default-port))))))

      (setq utu-buffer-name (concat "*utu:" server "*")
	    utu-user-name name
	    utu-socket-name (make-temp-file "hate-el-"))
      (delete-file utu-socket-name)
	    
      (setq utu-process (make-network-process :name "utu"
					      :host 'local :family 'local
							   :service utu-socket-name
							   :server t
							   :buffer utu-buffer-name
							   :coding 'raw-text
							   :filter 'utu-filter))

      (utu-start-mendicant)
    
      (message "Connecting to %s..." server)

      (when utu-debug 
	(switch-to-buffer utu-buffer-name))

      (utu-register "chat" '(room))
      ;; say something nice, you goof! something sweet.
      )))


(defun utu-start-mendicant ()
  (shell-command (concat "utumendicant " utu-socket-name "2> err.log &"))
  ;; TODO
  ;; accept connection on socket
  ;; send config to mendicant
  ;; flush socket?
  ;; get host info and client info?
  )

(defun utu-register (root children) 
  ;; TODO - grab from client.rb
)

(defun utu-filter (process data)
  ;; TODO - can we pull just a given number of bytes like client.rb
  (let* ((data (utu-from-stackish data))
;	 (room data) ; TODO - extract these
	 (room-name nil)
	 (text data)
	 (from nil)
	 (type nil)
	 (msg (concat "[" room-name "]" "[" from "] " 
		      (if (not (string= type "say")) (concat "(" type ") "))))

    (save-excursion
      (set-buffer utu-buffer-name)
      (insert msg text)))))

(defun utu-from-stackish (nodes)
  ;; TODO - port from stackish.rb
  nodes)

(when nil ; for eval'ing live
(setq utu-process (make-network-process :name "utu"
					:host 'local :family 'local
						     :service "/tmp/hate-el-manual"
						     :server t
						     :buffer "*utu*"
						     :coding 'raw-text
;						     :filter 'utu-filter
))
(process-send-string (get-process "utu <*1*>" "\"technomancy\" name my")
;; how to get processes from accepted clients?
)
;;; pcmpl-ssh.el --- functions for completing SSH hostnames

;; Copyright (C) 2007 Phil Hagelberg

;; Author: Phil Hagelberg <technomancy@gmail.com>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/PcompleteSSH
;; Version: 0.1
;; Created: 2007-12-02
;; Keywords: shell completion ssh
;; EmacsWiki: PcompleteSSH

;; This file is NOT part of GNU Emacs.

;; Last-Updated: Sun Dec 02 15:58:06 2007 PST
;; By: Phil Hagelberg
;; Update #: 1

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

;;; Commentary:

;; This will allow eshell to autocomplete SSH hosts from the list of
;; known hosts in your ~/.ssh/known_hosts file. Note that newer
;; versions of ssh hash the hosts by default to prevent Island-hopping
;; SSH attacks. (https://itso.iu.edu/Hashing_the_OpenSSH_known__hosts_File)
;; You can disable this by putting the following line in your ~/.ssh/config
;; file following the "Host *" directive:

;; HashKnownHosts no

;; Note that this will make you vulnerable to the Island-hopping
;; attack described in the link above if you allow key-based
;; passwordless logins and your account is compromised.

;;; Code:

(require 'pcomplete)
(require 'executable)

;;;###autoload
(defun pcomplete/ssh ()
  "Completion rules for the `ssh' command."
  (pcomplete-here (pcmpl-ssh-hosts)))

(defun pcmpl-ssh-hosts ()
  (with-temp-buffer
    (insert-file-contents-literally "~/.ssh/known_hosts")
    (goto-char (point-min))
    (pcmpl-build-ssh-host-list)))

(defun pcmpl-build-ssh-host-list ()
  (when (not (eobp))
    (cons (pcmpl-host-at-point)
          (progn (forward-line)
                 (pcmpl-build-ssh-host-list)))))

(defun pcmpl-host-at-point ()
  (buffer-substring (point) (- (search-forward ",") 1)))

(provide 'pcmpl-ssh)
;;; pcmpl-ssh.el ends here
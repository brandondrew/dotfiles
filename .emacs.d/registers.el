
;;; Part of my .emacs file

;; by Phil Hagelberg
;; Much thanks to emacswiki.org and RMS.

;; Note: this relies on files found in my dotfiles repository:
;; http://dev.technomancy.us/phil/browser/dotfiles/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     registers (C-x C-r)

(set-register ?e '(file . "~/.emacs"))
(set-register ?d '(file . "~/.emacs.d"))
(set-register ?r '(file . "~/.emacs.d/rinari/rinari.el"))
(set-register ?y '(file . "~/.emacs.d/ebby.el"))
(set-register ?g '(file . "~/.gnus.el"))
(set-register ?b '(file . "~/.bashrc"))
(set-register ?s '(file . "~/.screenrc"))
(set-register ?c '(file . "~/.contacts"))
(set-register ?k '(file . "/home/phil/.mozilla/firefox/i6m4tefk.default/extensions/{a79fe89b-6662-4ff4-8e88-09950ad4dfde}/chrome/conkeror.jar"))

(provide 'registers)
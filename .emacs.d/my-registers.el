
;;; Part of my .emacs project

;; by Phil Hagelberg
;; Much thanks to RMS and the folks at emacswiki.org.

;; Note: this relies on files found in my dotfiles repository:
;; http://git.caboo.se/?p=technomancy.git;a=summary

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     registers (C-x C-r)

(mapc (lambda (args) (apply #'set-register args))
      '((?e (file . "~/.emacs.d/init.el"))
	(?d (file . "~/.emacs.d"))
	(?g (file . "~/.gnus.el"))
	(?b (file . "~/.bashrc"))
	(?t (file . "~/.todo"))
	(?r (file . "~/.emacs.d/my-registers.el"))
	(?s (file . "~/.screenrc"))
	(?c (file . "~/.contacts"))
	(?f (file . "~/.feeds.opml"))
	(?x (file . "~/.xbindkeys.scm"))
	(?n (file . "~/documents/NOTES"))))

(provide 'my-registers)
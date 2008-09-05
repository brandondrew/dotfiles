
;;; Part of my .emacs project

;; by Phil Hagelberg
;; Much thanks to RMS and the folks at emacswiki.org.

;; Note: this relies on files found in my dotfiles repository:
;; http://github.com/technomancy/dotfiles

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     registers (C-x C-r)

(mapc (lambda (args) (apply #'set-register args))
      '((?e (file . "~/.emacs.d/init.el"))
	(?d (file . "~/.emacs.d"))
	(?g (file . "~/.gnus.el"))
	(?r (file . "~/.emacs.d/my-registers.el"))
	(?x (file . "~/.xbindkeys.scm"))))

(provide 'my-registers)

;;; Part of my .emacs project

;; by Phil Hagelberg
;; Much thanks to RMS and the folks at emacswiki.org.

;; Note: this relies on files found in my dotfiles repository:
;; http://git.caboo.se/?p=technomancy.git;a=summary

;;; hooks

(add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'js-mode-hook 'moz-minor-mode)
(add-hook 'html-mode-hook 'auto-fill-mode)
;(add-hook 'before-save-hook 'delete-trailing-whitespace)
;(add-hook 'before-save-hook 'untabify-buffer)

(provide 'my-hook-setup)

;;; Part of my .emacs file

;; by Phil Hagelberg
;; Much thanks to emacswiki.org and RMS.

;; Note: this relies on files found in my dotfiles repository:
;; http://dev.technomancy.us/phil/browser/dotfiles/

;;; hooks

(add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'js-mode-hook 'moz-minor-mode)
(add-hook 'html-mode-hook 'auto-fill-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'untabify-buffer)

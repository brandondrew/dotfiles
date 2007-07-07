
;;; Part of my .emacs file

;; by Phil Hagelberg
;; Much thanks to emacswiki.org and RMS.

;; Note: this relies on files found in my dotfiles repository:
;; http://dev.technomancy.us/phil/browser/dotfiles/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     misc things

(when window-system
  (mouse-wheel-mode t)
  (global-hl-line-mode t)
;; this slows boot significantly in emacs 22
;  (set-scroll-bar-mode 'right) ; mostly for seeing how far down we are, not for clicking
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (setq browse-url-browser-function 'browse-url-epiphany)
  (setq browse-url-epiphany-new-window-is-tab t)
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (blink-cursor-mode -1)
  (ignore-errors (set-default-font "terminus-16")))

(when (not window-system)
  (keyboard-translate ?\C-h ?\C-?))

(add-to-list 'comint-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'find-file-hook 'jao-toggle-selective-display)

(setq visible-bell t)
(setq font-lock-maximum-decoration t)
(setq inhibit-startup-message t)
(setq transient-mark-mode t)
(auto-compression-mode t) ; load .gz's automatically
(global-font-lock-mode t)
(menu-bar-mode -1) ; toggled by F1
(show-paren-mode 1)
(setq color-theme-is-global nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'qrr 'query-replace-regexp)
(random t)
;(add-to-list 'bs-configurations
;	     '("gnus" nil nil "^[^#]" nil nil))

;; don't clutter directories!
(setq backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups"))))
(setq auto-save-default nil)

(add-to-list 'auto-mode-alist '("\\.ds$" . emacs-lisp-mode))

;; w3m
(setq w3m-use-cookies t)
(setq w3m-default-save-directory "~/")
(setq w3m-default-display-inline-images t)

(mapc (lambda (v) (set v nil))
      '(w3m-show-graphic-icons-in-header-line
        w3m-show-graphic-icons-in-mode-line
        w3m-track-mouse
        w3m-use-favicon
        w3m-use-toolbar))

(provide 'misc)
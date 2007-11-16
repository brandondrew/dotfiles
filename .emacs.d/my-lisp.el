
;;; Part of my .emacs file

;; by Phil Hagelberg
;; Much thanks to emacswiki.org and RMS.

;; Note: this relies on files found in my dotfiles repository:
;; http://dev.technomancy.us/phil/browser/dotfiles/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp
;;

;; (add-to-list 'load-path "~/.emacs.d/slime-2.0/")
;; (setq inferior-lisp-program "/usr/bin/sbcl")
;; (require 'slime)

(add-hook 'emacs-lisp-mode-hook 'pretty-lambdas)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-mode-hook 'pretty-lambdas)

(defun my-lisp-hook ()
  (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)
  (font-lock-add-keywords nil
                          '(("(\\|)" . 'paren-face))))

(add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)
(add-hook 'lisp-mode-hook 'my-lisp-hook)
(add-hook 'lisp-mode-hook 'my-coding-hook)

(defface paren-face
   '((((class color) (background dark))
      (:foreground "grey20"))
     (((class color) (background light))
      (:foreground "grey55")))
   "Face used to dim parentheses.")

(provide 'my-lisp)
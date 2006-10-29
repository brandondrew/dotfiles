
;;; Part of my .emacs file

;; by Phil Hagelberg
;; Much thanks to emacswiki.org and RMS.

;; Note: this relies on files found in my dotfiles repository:
;; http://dev.technomancy.us/phil/browser/dotfiles/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp
;;

(add-to-list 'load-path "~/.emacs.d/slime-2.0/")
(setq inferior-lisp-program "/usr/local/bin/lisp")
(autoload 'slime "slime-setup")

(add-hook 'emacs-lisp-mode-hook 'pretty-lambdas)
(add-hook 'lisp-mode-hook 'pretty-lambdas)

(add-hook 'emacs-lisp-mode-hook 
 	  (lambda ()
 	    (font-lock-add-keywords nil 
 				    '(("(\\|)" . 'paren-face)))))
(add-hook 'lisp-mode-hook 
 	  (lambda ()
 	    (font-lock-add-keywords nil 
 				    '(("(\\|)" . 'paren-face)))))

(defface paren-face
   '((((class color) (background dark))
      (:foreground "grey20"))
     (((class color) (background light))
      (:foreground "grey55")))
   "Face used to dim parentheses.")

(provide 'lisp)
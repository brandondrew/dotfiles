
;;; Part of my .emacs file

;; by Phil Hagelberg
;; Much thanks to emacswiki.org and RMS.

;; Note: this relies on files found in my dotfiles repository:
;; http://git.caboo.se/?p=technomancy.git;a=summary

;;; Text manipulation

(global-set-key "\C-\M-h" 'backward-kill-word)
(global-set-key (kbd "C-c l") (lambda () (interactive) (insert "lambda")))

;;; Navigation

(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-x\C-r" 'jump-to-register)

(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-\M-s" 'isearch-forward)
(global-set-key "\C-\M-r" 'isearch-backward)

;;; Buffer management

(global-set-key "\M-p" 'previous-buffer)
(global-set-key "\M-n" 'next-buffer)
(global-set-key (kbd "C-<next>") 'buffer-stack-up)
(global-set-key (kbd "C-<prior>") 'buffer-stack-down)

(global-set-key [XF86Back] 'buffer-stack-up)
(global-set-key [XF86Forward] 'buffer-stack-down)
(global-set-key (kbd "C-x C-b") 'bs-show)

;;; Window management

(global-set-key "\C-x-" 'shrink-window)
(global-set-key "\C-x=" 'enlarge-window)

(global-set-key (kbd "C-x !") 'window-small-and-large)
(global-set-key "\C-xO" (lambda () (interactive) (other-window -1)))
(global-set-key "\C-x." (lambda () (interactive) (enlarge-window 1 t)))
(global-set-key "\C-x," (lambda () (interactive) (shrink-window 1 t)))

(global-set-key (kbd "C-x M-k") (lambda () (interactive) (kill-buffer (current-buffer)) (delete-window)))
(global-set-key (kbd "C-x d") 'toggle-dedicated-window)

;;; Lisp

(global-set-key (kbd "C-c C-e") 'my-eval-and-replace)
(global-set-key (kbd "C-c v") 'eval-buffer)

;;; Shell

(global-set-key (kbd "C-x m") 'eshell)
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;;; Display

(global-set-key (kbd "M-\\") 'my-selective-display)
(global-set-key [f2] (lambda () (interactive)
                       (set-default-font "-xos4-terminus-medium-r-normal--16-160-72-72-c-80-iso8859-9")
                       (require 'flymake)
                       (set-face-attribute 'mumamo-background-chunk-submode nil :background "grey18")
                       (set-face-attribute 'flymake-errline nil :background "Maroon4")
                       (set-face-background 'flymake-warnline "dark slate blue")
                       (color-theme-zenburn)))

;;; Utility

(global-set-key "\C-xh" 'view-url)
(global-set-key (kbd "C-c p") (lambda () (interactive) (message "%s" (point))))
(global-set-key [f1] 'menu-bar-mode)
(global-set-key (kbd "C-h a") 'apropos)

(define-key isearch-mode-map (kbd "C-o") ; occur easily inside isearch
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

(provide 'my-bindings)

;;; Part of my .emacs file

;; by Phil Hagelberg
;; Much thanks to emacswiki.org and RMS.

;; Note: this relies on files found in my dotfiles repository:
;; http://git.caboo.se/?p=technomancy.git;a=summary

;;; Text manipulation

(global-set-key "\C-\M-h" 'backward-kill-word)
(global-set-key (kbd "C-c l") (lambda () (interactive) (insert "lambda")))
(global-set-key (kbd "C-x \\") 'align-regexp)

;;; Navigation

(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-x\C-r" 'jump-to-register)

(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-\M-s" 'isearch-forward)
(global-set-key "\C-\M-r" 'isearch-backward)

;;; Buffer management

(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-b") 'bs-show)
(global-set-key (kbd "C-c C-r") 'revert-buffer)

;;; Window management

(global-set-key "\C-x-" 'shrink-window)
(global-set-key "\C-x=" 'enlarge-window)

(global-set-key "\C-xO" (lambda () (interactive) (other-window -1)))
(global-set-key "\C-x\C-o" (lambda () (interactive) (other-window 2)))
(global-set-key "\C-x." (lambda () (interactive) (enlarge-window 1 t)))
(global-set-key "\C-x," (lambda () (interactive) (shrink-window 1 t)))

(global-set-key (kbd "C-x M-k") (lambda () (interactive) (kill-buffer (current-buffer)) (delete-window)))
(global-set-key (kbd "C-x d") (lambda () (interactive) (toggle-dedicated-window)))

;;; Lisp

(global-set-key (kbd "C-c C-e") 'my-eval-and-replace)
(global-set-key (kbd "C-c v") 'eval-buffer)

;;; Shell

(global-set-key (kbd "C-x m") 'eshell)
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Web

(global-set-key (kbd "C-x w") 'w3m)
(global-set-key "\C-xh" 'view-url)
(global-set-key "\C-cT" 'twittering-update-status-interactive)

;;; Utility

(global-set-key (kbd "C-c p") (lambda () (interactive) (message "%s" (point))))
(global-set-key [f1] 'menu-bar-mode)
(global-set-key (kbd "C-h a") 'apropos)
(global-set-key (kbd "C-h c") 'cheat)
(define-key read-expression-map (kbd "TAB") #'lisp-complete-symbol)
(global-set-key (kbd "M-\\") 'my-selective-display)
(global-set-key (kbd "C-x v d") 'vc-status)
(global-set-key (kbd "C-c j") (lambda () (interactive) (switch-or-start 'jabber-connect "*-jabber-*")))
(global-set-key (kbd "C-c g") (lambda () (interactive) (switch-or-start 'gnus "*Group*")))
(global-set-key (kbd "C-c t") (lambda () (interactive) (switch-or-start 'twittering-mode "*twittering*")))

(define-key isearch-mode-map (kbd "C-o") ;; occur easily inside isearch
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

(provide 'my-bindings)
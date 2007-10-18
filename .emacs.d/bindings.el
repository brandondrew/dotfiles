
;;; Part of my .emacs file

;; by Phil Hagelberg
;; Much thanks to emacswiki.org and RMS.

;; Note: this relies on files found in my dotfiles repository:
;; http://dev.technomancy.us/phil/browser/dotfiles/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     key bindings

(global-set-key "\C-\M-h" 'backward-kill-word)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-x\C-r" 'jump-to-register)
(global-set-key "\C-x-" 'shrink-window)
(global-set-key "\C-x=" 'enlarge-window)
(global-set-key (kbd "C-x !") 'window-small-and-large)
(global-set-key "\C-xO" (lambda () (interactive) (other-window -1)))
(global-set-key "\C-x." (lambda () (interactive) (enlarge-window 1 t)))
(global-set-key "\C-x," (lambda () (interactive) (shrink-window 1 t)))

(global-set-key "\C-xd" (lambda () (interactive)
                          (shell-command (concat "dict " (read-string (concat "Define word (default " (word-at-point) "): ") nil nil (word-at-point))))))

(global-set-key (kbd "C-c e") 'fc-eval-and-replace)
(global-set-key (kbd "C-c v") 'eval-buffer)
(global-set-key (kbd "C-x m") 'eshell)
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))
(global-set-key (kbd "C-x M-k") (lambda () (interactive) (kill-buffer (current-buffer)) (delete-window)))

(global-set-key (kbd "M-\\") 'jao-toggle-selective-display)

(global-set-key "\C-xh" 'view-url)
(global-set-key (kbd "C-c l") (lambda () (interactive) (insert "lambda")))

(global-set-key (kbd "C-c p") (lambda () (interactive) (message "%s" (point))))

; searching with regexes by default
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-\M-s" 'isearch-forward)
(global-set-key "\C-\M-r" 'isearch-backward)

; linear buffer-switching
(global-set-key "\M-p" 'previous-buffer)
(global-set-key "\M-n" 'next-buffer)
(global-set-key (kbd "C-<next>") 'next-buffer)
(global-set-key (kbd "C-<prior>") 'previous-buffer)

; just useful for learning new modes
(global-set-key [f1] 'menu-bar-mode)

(global-set-key (kbd "C-h a") 'apropos)

(global-set-key [f2] (lambda () (interactive)
                       (set-default-font "-xos4-terminus-medium-r-normal--16-160-72-72-c-80-iso8859-9")
		       (require 'flymake)
		       (set-face-attribute 'erb-face nil :background "grey18")
		       (set-face-attribute 'erb-delim-face nil :background "grey15")
		       (set-face-attribute 'flymake-errline nil :background "Maroon4")
		       (set-face-attribute 'test-unit-pass-face nil :background "PaleGreen4")
		       (set-face-attribute 'test-unit-fail-face  nil :background "firebrick3")
		       (set-face-attribute 'test-unit-error-face nil :background "sienna")
		       (set-face-attribute 'test-unit-line-face nil :background "firebrick4")
                       (color-theme-zenburn)))

(global-set-key [(control shift 2)] 'mark-string)
(global-set-key [(shift f2)] 'color-theme-standard)

(global-set-key (kbd "C-h r") 'ri)

(add-hook 'c-mode-hook (lambda () (define-key c-mode-map "\C-\M-h" 'backward-kill-word)))

(define-key isearch-mode-map (kbd "C-o") ; occur easily inside isearch
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

(provide 'bindings)
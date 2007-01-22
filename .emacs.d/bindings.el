
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

(global-set-key (kbd "C-c e") 'fc-eval-and-replace)
(global-set-key (kbd "C-x m") 'pmacro)
(global-set-key (kbd "M-\\") 'jao-toggle-selective-display)

(global-set-key "\C-xh" (lambda (url) (interactive "MUrl: ") 
			  (switch-to-buffer (url-retrieve-synchronously url))
			  (rename-buffer url t)
			  (html-mode)))

(setq outline-minor-mode-prefix [(control o)])

; searching with regexes by default
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-\M-s" 'isearch-forward)
(global-set-key "\C-\M-r" 'isearch-backward)

;; ; hide-show
(global-set-key "\C-]" 'hs-hide-level)
(global-set-key (kbd "C-}") 'hs-hide-block)
(global-set-key (kbd "C-{") 'hs-show-block)

; linear buffer-switching
(global-set-key "\M-p" 'bs-cycle-next)
(global-set-key "\M-n" 'bs-cycle-previous)
(global-set-key [XF86Back] 'bs-cycle-next)
(global-set-key [XF86Forward] 'bs-cycle-previous)
(setq bs-cycle-configuration-name "files")

; 2D spatial buffer-switching
(global-set-key [(control shift p)] 'tabbar-backward-group)
(global-set-key [(control shift n)] 'tabbar-forward-group)
(global-set-key [(control shift b)] 'tabbar-backward)
(global-set-key [(control shift f)] 'tabbar-forward)

; sometimes my hands aren't in the right place
(global-set-key [(control shift up)] 'tabbar-backward-group)
(global-set-key [(control shift down)] 'tabbar-forward-group)
(global-set-key [(control shift left)] 'tabbar-backward)
(global-set-key [(control shift right)] 'tabbar-forward)

; just useful for learning new modes
(global-set-key [f1] 'menu-bar-mode)

(global-set-key [f2] 'color-theme-zenburn)
(global-set-key [(control shift 2)] 'mark-string)
(global-set-key [(shift f2)] 'color-theme-standard)

(global-set-key [f3] 'rename-buffer)

; music management
(global-set-key [f4] (lambda ()
		       (interactive)
		       (shell-command "ssh philisha.net mpc toggle")))

(global-set-key [(f5)] (lambda ()
			       (interactive)
			       (shell-command "ssh philisha.net mpc next")))

(global-set-key [(f3)] (lambda ()
			       (interactive)
			       (shell-command "ssh philisha.net mpc prev")))

(global-set-key [(control f3)] 'random-music)

(global-set-key [(control f4)] 
		(lambda (dir)
		  (interactive (list (completing-read "Play directory: " 
						      (split-string 
						       (shell-command-to-string "find /home/phil/music -type d | cut -c 18-") "\n"))))
		  (shell-command (concat 
				  "ssh philisha.net mpc clear; "
				  "ssh philisha.net mpc add " dir
				  "; ssh philisha.net mpc play > /dev/null"))))



;; For Ebby debugging, mostly
(global-set-key [f7] (lambda () (interactive) (message "%s" (point))))

(global-set-key [f9] '(lambda () 
			(interactive) 
			(if (get-buffer "*ansi-term*")
			    (switch-to-buffer "*ansi-term*")
			  (ansi-term "/bin/bash"))))

; great for quick googles
(global-set-key [f10] 'w3m)

(global-set-key [(meta f10)] 'ruby-xmp-region)

(global-set-key [f11] 'ri)
;(global-set-key "\C-hr" 'ri)

(global-set-key [f12] '(lambda ()
			 (interactive)
			 (write-file "~/mjolnir/apps/technomancy/public/tmp/tmp.txt")))


(add-hook 'c-mode-hook (lambda () (define-key c-mode-map "\C-\M-h" 'backward-kill-word)))

(define-key isearch-mode-map (kbd "C-o") ; occur easily inside isearch
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

(provide 'bindings)
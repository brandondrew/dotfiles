
;;; Part of my .emacs project

;; by Phil Hagelberg
;; Much thanks to RMS and the folks at emacswiki.org.

;; Note: this relies on files found in my dotfiles repository:
;; http://github.com/technomancy/dotfiles

(require 'ansi-color) ;; has to be eval'd before load

(eval-after-load 'eshell
  '(progn
     (require 'pcmpl-rake)
     (require 'em-prompt)
     (require 'em-cmpl)
     (require 'esh-mode)
     (load "em-term.el")

     (setq eshell-cmpl-cycle-completions nil)
     (setq eshell-save-history-on-exit t)
     (set-face-attribute 'eshell-prompt nil :foreground "DeepSkyBlue")

     (add-to-list 'eshell-visual-commands "ssh")
     (add-to-list 'eshell-visual-commands "autotest")
     (add-to-list 'eshell-visual-commands "tail")

     (add-to-list 'eshell-command-completions-alist
                  (cons "gunzip" "gz\\'"))
     (add-to-list 'eshell-command-completions-alist
                  '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))

     (defun eshell-handle-ansi-color ()
       (ansi-color-apply-on-region eshell-last-output-start
				   eshell-last-output-end))

     (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)

     (defun eshell-maybe-bol ()
       (interactive)
       (let ((p (point)))
	 (eshell-bol)
	 (if (= p (point))
	     (beginning-of-line))))

     (add-hook 'eshell-mode-hook
	       '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-maybe-bol)))

     (setq eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\)/\\'")))

(provide 'my-eshell)
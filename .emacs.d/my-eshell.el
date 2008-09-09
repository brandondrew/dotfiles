
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

     (setq eshell-cmpl-cycle-completions nil
           eshell-save-history-on-exit t
           eshell-highlight-prompt nil)
     
     (set-face-attribute 'eshell-prompt nil :foreground "turquoise1")

     (defface eshell-branch-face
       `((t (:foreground "DeepSkyBlue3")))
       "Face for VC branch display.")

     (add-to-list 'eshell-visual-commands "ssh")
     ;; (add-to-list 'eshell-visual-commands "autotest")
     (add-to-list 'eshell-visual-commands "tail")

     (add-to-list 'eshell-command-completions-alist
                  (cons "gunzip" "gz\\'"))
     (add-to-list 'eshell-command-completions-alist
                  '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))

     (defun eshell-handle-ansi-color ()
       (ansi-color-apply-on-region eshell-last-output-start
				   eshell-last-output-end))

     (ignore-errors
       (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color))

     (defun eshell-maybe-bol ()
       (interactive)
       (let ((p (point)))
	 (eshell-bol)
	 (if (= p (point))
	     (beginning-of-line))))

     (setq eshell-prompt-function
           (lambda ()
             (concat (propertize (or (eshell/branch) "") 'face 'eshell-branch-face) " "
                     (propertize (concat (eshell/pwd)
                                         (if (= (user-uid) 0) " # " " $"))
                                 'face 'eshell-prompt) " ")))

     (defun eshell/branch ()
       "Return the current git branch, if applicable."
       ;; TODO: should use VC so it works with more than just git
       ;; but VC doesn't return branch info for directories yet
       (let ((branch (shell-command-to-string "git branch")))
         (string-match "^\\* \\(.*\\)" branch)
         (match-string 1 branch)))
;;;          (propertize (match-string 1 branch)
;;;                      'face 'eshell-branch-face)))

     ;; Slow as the dickens on large non-VC'd directories. =\
     (defun eshell/vc (&optional files)
       "Return the VC backend and branch of the first VC-controlled file in the current dir."
       ;; first two are . and .., which we drop with cdr
       (let ((file (find t (cdr (cdr (directory-files (eshell/pwd))))
                         :test (lambda (true f) (vc-registered (expand-file-name f))))))
         (when file
           (vc-mode-line (expand-file-name file))
           (propertize (substring-no-properties vc-mode 1)
                       :foreground "yellow green"))))
                       

     (vc-mode-line buffer-file-name)
     (defun eshell-make-primary ()
       "Make the current buffer swap names with \"*eshell*\"."
       (interactive)
         (let ((old-name (buffer-name)))
           (switch-to-buffer "*eshell*")
           (rename-buffer "*eshell-temp*")
           (switch-to-buffer old-name)
           (rename-buffer "*eshell*")
           (switch-to-buffer "*eshell-temp*")
           (rename-buffer old-name))
         (switch-to-buffer "*eshell*"))
  
     (add-hook 'eshell-mode-hook
	       '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-maybe-bol)))

     (setq eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\)/\\'")))

(provide 'my-eshell)
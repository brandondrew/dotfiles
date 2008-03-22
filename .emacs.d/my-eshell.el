;;; TODO
;; PID completion for kill/killall
;; git auto-complete
;; apt-get

(eval-after-load 'eshell
  '(progn
     (require 'pcmpl-rake)
     (require 'em-prompt)
     (require 'em-cmpl)
     (setq eshell-cmpl-cycle-completions nil)
     (setq eshell-save-history-on-exit t)
     (set-face-attribute 'eshell-prompt nil :foreground "DeepSkyBlue")

     (add-to-list 'eshell-command-completions-alist
                  (cons "gunzip" "gz\\'"))
     (add-to-list 'eshell-command-completions-alist
                  '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))

     ;; as is this one:
     (setq eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\)/\\'")))

(provide 'my-eshell)
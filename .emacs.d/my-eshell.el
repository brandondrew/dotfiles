;;; TODO
;; Rake auto-complete
;; ssh/scp autocomplete from ~/.ssh/known_hosts
;; PID completion for kill/killall
;; git auto-complete
;; apt-get

(setq eshell-cmpl-cycle-completions nil)
(setq eshell-save-history-on-exit t)
;;(add-hook 'eshell-mode-hook (lambda () 
;;			      (set-face-attribute 'eshell-prompt nil :foreground "DeepSkyBlue")))
;; see pcmpl-cvs.el in ~/src/emacs/lisp

;; this variable seems to be ignored:
;(add-to-list 'eshell-command-completions-alist
;	     (cons "gunzip" "gz\\'"))
;(add-to-list 'eshell-command-completions-alist
;	     '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))

;; as is this one:
;(setq eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\)/\\'")

;;; (defun eshell/touch (&rest args)
;;;   "Implementation of touch in Lisp."
;;;   (eshell-eval-using-options
;;;    "touch" args
;;;    '((?a "accesstime" nil accesstime
;;; 	 "change only the access time")
;;;      (?m "modtime" nil modtime
;;; 	 "change only the modification time")
;;;      :preserve-args
;;;      :external "touch"
;;;      :show-usage
;;;      :usage "[OPTION]... FILE...")))
   

;;; check out what pcomplete says:
;;; (defcustom eshell-command-completion-function
;;;   (function
;;;    (lambda ()
;;;      (pcomplete-here (eshell-complete-commands-list))))
;;;   (documentation-property 'pcomplete-command-completion-function
;;; 			  'variable-documentation)
;;;   :type (get 'pcomplete-command-completion-function 'custom-type)
;;;   :group 'eshell-cmpl)

(provide 'my-eshell)
;; ete -- Errors to Emacs
;;
;; Written and placed into the Public Domain by
;; Christian Neukirchen <http://purl.org/net/chneukirchen>, 2004.

(require 'compile)

(defun ete ()
  "Jump to eventual errors found by ete (Errors To Emacs)"
  (interactive)
  (let ((logfile (concat (or (getenv "TMPDIR") "/tmp") "/ete." (user-login-name))))
    (if (= 0 (nth 7 (file-attributes "/tmp/ete.chris")))
	(message "No errors")
      (compilation-start (concat "cat -- " logfile) 'ete-mode))))
  
(define-compilation-mode ete-mode "ete" "Compilation mode used by ete"
  (make-local-variable 'compilation-finish-function)
  (setq compilation-finish-function
	(lambda (buffer return)
	  (next-error)
	  t)))

(define-key global-map (kbd "\C-x :") 'ete)

(provide 'ete)

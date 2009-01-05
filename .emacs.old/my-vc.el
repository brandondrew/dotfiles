
;;; Part of my .emacs project

;; by Phil Hagelberg
;; Much thanks to RMS and the folks at emacswiki.org.

;; Note: this relies on files found in my dotfiles repository:
;; http://github.com/technomancy/dotfiles

(require 'vc)

(eval-after-load 'vc-annotate
  '(progn
     (require 'log-view)
     (define-key log-view-mode-map (kbd "RET") 'log-view-find-revision)
     (define-key vc-annotate-mode-map (kbd "RET")
       'vc-annotate-find-revision-at-line)))

(defun my-vc-add-or-register ()
  "Register the file if it hasn't been registered, otherwise git add it."
  (interactive)
  (if (eq 'Git (vc-backend buffer-file-name))
      (shell-command (format "git add %s" buffer-file-name))
    (vc-register)))

;; (global-set-key (kbd "C-x v i") 'my-vc-add-or-register)

(provide 'my-vc)
;;; my-vc.el ends here
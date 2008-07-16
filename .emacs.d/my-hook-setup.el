
;;; Part of my .emacs project

;; by Phil Hagelberg
;; Much thanks to RMS and the folks at emacswiki.org.

;; Note: this relies on files found in my dotfiles repository:
;; http://github.com/technomancy/dotfiles

;;; hooks

(add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'js-mode-hook 'moz-minor-mode)
(add-hook 'html-mode-hook 'auto-fill-mode)
;(add-hook 'before-save-hook 'delete-trailing-whitespace)
;(add-hook 'before-save-hook 'untabify-buffer)

(add-hook 'oddmuse-mode-hook
          (lambda ()
            (unless (string-match "question" oddmuse-post)
              (setq oddmuse-post (concat "uihnscuskc=1;" oddmuse-post)))))

(eval-after-load 'jabber
  '(progn
     (add-hook 'jabber-post-disconnect-hook
               (lambda () (kill-buffer "*-jabber-*")
                 (kill-buffer " *-jabber-process-*")))

     (add-hook 'jabber-post-connect-hook (lambda () (switch-to-buffer "*-jabber-*")))))

(provide 'my-hook-setup)
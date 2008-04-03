(require 'rcirc)

;; example:
;(setq rcirc-authinfo '(("freenode" nickserv "technomancy" "password")))

(setq rcirc-server-alist '(("irc.freenode.net" (:channels ("#emacs" "#seattle.rb")))))
(setq rcirc-fill-column 72)
(add-hook 'rcirc-mode-hook (lambda () (rcirc-track-minor-mode 1)))

;; Turn on spell checking.
(add-hook 'rcirc-mode-hook (lambda ()
                             (flyspell-mode 1)))

;; Keep input line at bottom.
(add-hook 'rcirc-mode-hook
          (lambda ()
            (set (make-local-variable 'scroll-conservatively)
                 8192)))

(setq rcirc-default-nick "technomancy")
(setq rcirc-server-alist '(("irc.freenode.net" "#emacs" "#seattle.rb")))
(setq rcirc-omit-mode t)

(setq rcirc-buffer-maximum-lines 10240)

;; TODO: fix
(defun rcirc-notify-nick (process sender response target text)
  (if (and (executable-find "notify-send")
           (string-match rcirc-default-nick text))
      (shell-command (concat "notify-send \"" text "\""))))

(add-hook 'rcirc-print-hooks 'rcirc-notify-nick)

(require 'rcirc-color)
(require 'rcirc-completion)
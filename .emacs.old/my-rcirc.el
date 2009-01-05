(require 'rcirc)

;; example:
(setq rcirc-server-alist '(("irc.freenode.net" . (:channels .
                                                  ("#emacs" "#seattle.rb")))
                           ("central" . (:channels "#chatter"))))

(setq rcirc-startup-channels '("#emacs" "#seattle.rb"))

(setq rcirc-fill-column 72)

;; Turn on spell checking.
(add-hook 'rcirc-mode-hook (lambda ()
                             (set (make-local-variable 'scroll-conservatively)
                                  8192)
                             (rcirc-track-minor-mode 1)
                             (rcirc-omit-mode)
                             (flyspell-mode 1)))

(setq rcirc-default-nick "technomancy")
(setq rcirc-server-alist '(("irc.freenode.net" "#emacs" "#seattle.rb")))
(setq rcirc-omit-mode t)

(setq rcirc-buffer-maximum-lines 10240)

;; TODO: fix escaping
(defun rcirc-notify-nick (process sender response target text)
  (if (and (executable-find "notify-send")
           (string-match rcirc-default-nick text))
      (shell-command (concat "notify-send \"" text "\""))))

(add-hook 'rcirc-print-hooks 'rcirc-notify-nick)

(defun-rcirc-command reconnect (arg)
     "Reconnect the server process."
     (interactive "i")
     (unless process
       (error "There's no process for this target"))
     (let* ((server (car (process-contact process)))
            (port (process-contact process :service))
            (nick (rcirc-nick process))
            channels query-buffers)
       (dolist (buf (buffer-list))
         (with-current-buffer buf
           (when (eq process (rcirc-buffer-process))
             (remove-hook 'change-major-mode-hook
                          'rcirc-change-major-mode-hook)
             (if (rcirc-channel-p rcirc-target)
                 (setq channels (cons rcirc-target channels))
               (setq query-buffers (cons buf query-buffers))))))
       (delete-process process)
       (rcirc-connect server port nick
                      rcirc-default-user-name
                      rcirc-default-user-full-name
                      channels)))

(require 'rcirc-color)
(require 'rcirc-completion)
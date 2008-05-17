;; By fledermaus, apparently
;; Fixes by Phil Hagelberg (technomancy)

(require 'thingatpt)

(defvar last-idle-highlight-word nil)
(make-variable-buffer-local 'last-idle-highlight-word)

(defun idle-highlight-word-at-point ()
  (interactive)
  (let ((target (symbol-name (symbol-at-point))))
    (when last-idle-highlight-word
      (unhighlight-regexp (concat "\\<" (regexp-quote last-idle-highlight-word) "\\>")))
    (when target
      (highlight-regexp (concat "\\<" (regexp-quote target) "\\>") 'region)
      (setq last-idle-highlight-word target))))

(defun idle-highlight ()
  "Activate idle-highlighting."
  (interactive)
  (setq idle-highlight-timer
	(run-with-idle-timer 0.5 :repeat 'idle-highlight-word-at-point)))

(defun idle-highlight-disable ()
  (interactive)
  (if (boundp 'idle-highlight-timer)
      (cancel-timer idle-highlight-timer)
    (message "idle-highlight not enabled")))

(provide 'idle-highlight)
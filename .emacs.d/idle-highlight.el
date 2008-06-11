;; By fledermaus, apparently
;; Fixes by Phil Hagelberg (technomancy)

(require 'thingatpt)

(defun idle-highlight-word-at-point ()
  (interactive)
  (if idle-highlight-mode
      (let* ((target-symbol (symbol-at-point))
             (target (if target-symbol (symbol-name target-symbol))))
        (when idle-highlight-last-word
          (unhighlight-regexp (concat "\\<" (regexp-quote idle-highlight-last-word) "\\>")))
        (when (and target-symbol target)
          (highlight-regexp (concat "\\<" (regexp-quote target) "\\>") 'region)
          (setq idle-highlight-last-word target)))))

(defun idle-highlight ()
  "Activate idle-highlighting."
  (interactive)
  (set (make-local-variable 'idle-highlight-mode) t)
  (make-variable-buffer-local 'idle-highlight-last-word)
  (setq idle-highlight-timer
        (run-with-idle-timer 0.5 :repeat 'idle-highlight-word-at-point)))

(defun idle-highlight-disable ()
  (interactive)
  (if (boundp 'idle-highlight-timer)
      (cancel-timer idle-highlight-timer)
    (message "idle-highlight not enabled")))

(idle-highlight)
(provide 'idle-highlight)
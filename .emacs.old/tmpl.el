;;; tmpl.el --- String Templating

;; By Phil Hagelberg (C) 2008
;; Licensed under the same terms as Emacs

(eval-when-compile (require 'cl))

(defmacro tmpl (string)
  (save-window-excursion
    (switch-to-buffer "*tmpl*")
    (insert string) (goto-char (point-min))

    (while (search-forward-regexp "#{\\([^}]+\\)}" nil t)
      (let* ((value (match-string 1))
             (replacement (eval (read value))))
        (kill-region (point) (- (point) (length value) 3))
        (insert replacement)))
    (let ((return-value (buffer-string)))
      (kill-buffer "*tmpl*")
      return-value)))

;;; tmpl.el ends here
(defun rails-switch-to-view()
  (let ((pos (if (functionp 'posn-at-point)
                 (nth 2 (posn-at-point))
               (cons 200 100)))) ; mouse position at point
    (save-excursion
      (let (action path files)
        (goto-char (line-end-position))
        (search-backward-regexp "^[ ]*def \\([a-z_]+\\)")
        (setq action (match-string 1))
        (search-backward-regexp "^[ ]*class \\([a-zA-Z0-9_]+\\(::\\([a-zA-Z0-9_]+\\)\\)?\\)Controller[ ]+<")
        (setq path (rails-inflector-underscore (match-string 1)))
        (setq path (concat "app/views/" path "/"))

        (setq files (directory-files
                     (concat (rails-root) path)
                     nil
                     (concat "^" action (rails-make-template-regex))))

        (if (= 1 (list-length files))
            (progn
              (find-file (concat (rails-root) path (car files)))
              (message (concat path action))))

        (if (< 1 (list-length files))
            (let (items tmp file)
              (setq tmp files)
              (setq items (list))
              (while (car tmp)
                (add-to-list 'items (cons (car tmp) (car tmp)))
                (setq tmp (cdr tmp)))

              (setq file
                    (x-popup-menu
                     (list (list (car pos) (cdr pos))
                           (selected-window))
                     (list "Please select.." (cons "Please select.." items ))))
              (if file
                  (progn
                    (find-file (concat (rails-root) path file))
                    (message (concat path action))))))

        (if (> 1 (list-length files))
            (if (y-or-n-p (format "%s%s not found, create %s.rhtml? " path action action))
                (let ((root (rails-root)))
                  (make-directory (concat root path) t)
                  (find-file (format "%s%s%s.rhtml" root path action)))))))))


(defun rails-switch-to-action()
  (let (file path action root)
    (setq file buffer-file-name)
    (string-match "views/\\([^/]+\\)/\\([^/\.]+\\)\\(/\\([^/\.]+\\)\\)?" file)
    (if (match-beginning 4)
        (progn
          (setq path
                (concat (substring file (match-beginning 1) (match-end 1))
                        "/"
                        (substring file (match-beginning 2) (match-end 2)) ))
          (setq path (concat path "_controller.rb"))
          (setq action (substring file (match-beginning 4) (match-end 4))))
      (progn
        (setq path (concat
                    (substring file (match-beginning 1) (match-end 1))
                    "_controller.rb" ))
        (setq action (substring file (match-beginning 2) (match-end 2))))
      )
    (setq root (rails-root))
    (setq path (concat "app/controllers/" path))
    (if (file-exists-p (concat root path))
        (progn
          (find-file (concat root path))
          (goto-char (point-min))
          (message (concat path "#" action))
          (if (search-forward-regexp (concat "^[ ]*def[ ]*" action))
              (recenter))))))


(defun rails-switch-view-action()
  (interactive)
  (if (string-match "\\.rb$" buffer-file-name)
      (rails-switch-to-view)
    (rails-switch-to-action)))

(provide 'rails-switch)
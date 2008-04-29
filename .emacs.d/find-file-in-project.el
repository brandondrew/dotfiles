;;; extracted from rinari
;; by Phil Hagelberg and Doug Alcorn

(defun populate-project-files-table (file)
  (if (file-directory-p file)
      (mapc 'populate-project-files-table (directory-files file t "^[^\.]"))
    (let* ((file-name (file-name-nondirectory file))
	   (existing-record (assoc file-name project-files-table))
	   (unique-parts (get-unique-directory-names file (cdr existing-record))))
      (if (project-include file-name)
	(if existing-record
	    (let ((new-key (concat file-name " - " (car unique-parts)))
		  (old-key (concat (car existing-record) " - " (cadr unique-parts))))
	      (setf (car existing-record) old-key)
	      (setq project-files-table (acons new-key file project-files-table)))
	  (setq project-files-table (acons file-name file project-files-table)))))))

(defun project-include (file-name)
  (string-match "\\(\\.el$\\|\\.rb$\\|\\.js$\\|\\.emacs\\)" file-name))

(defun get-unique-directory-names (path1 path2)
  (let* ((parts1 (and path1 (split-string path1 "/" t)))
	 (parts2 (and path2 (split-string path2 "/" t)))
	 (part1 (pop parts1))
	 (part2 (pop parts2))
	 (looping t))
    (while (and part1 part2 looping)
      (if (equal part1 part2)
	  (setq part1 (pop parts1) part2 (pop parts2))
	(setq looping nil)))
    (list part1 part2)))

(defun find-file-in-project (file)
  (interactive (list (if (functionp 'ido-completing-read)
			 (ido-completing-read "Find file in project: " (mapcar 'car (project-files)))
		       (completing-read "Find file in project: " (mapcar 'car (project-files))))))
  (find-file (cdr (assoc file project-files-table))))

(defun project-files (&optional file)
  (when (or (not project-files-table) ;; initial load
	    (not (string-match (rails-root) (cdar project-files-table)))) ;; switched projects
    (setq project-files-table nil)
    (populate-project-files-table (or file (project-root)))
    project-files-table))

(defun project-root (&optional dir)
  (or dir (setq dir default-directory))
  (if (file-exists-p (concat dir ".emacs-project"))
      dir
    (if (equal dir  "/")
	nil
      (project-root (expand-file-name (concat dir "../"))))))

(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)

(eval-after-load 'ruby-mode
  '(progn
     (defun rails-project-files (&optional file)
       (setq project-files-table nil)
       (populate-project-files-table (or file (concat (rails-root) "/app")))
       (populate-project-files-table (or file (concat (rails-root) "/lib")))
       (populate-project-files-table (or file (concat (rails-root) "/test")))
       project-files-table)

     (defun find-file-in-rails (file)
       (interactive (list (if (functionp 'ido-completing-read)
			      (ido-completing-read "Find file in project: " (mapcar 'car (rails-project-files)))
			    (completing-read "Find file in project: " (mapcar 'car (rails-project-files))))))
       (find-file (cdr (assoc file project-files-table))))

     (define-key ruby-mode-map (kbd "C-x C-M-f") 'find-file-in-rails)
     (eval-after-load 'nxhtml-mode
       '(define-key nxhtml-mode-map (kbd "C-x C-M-f") 'find-file-in-rails))))

(provide 'find-file-in-project)
;;; find-file-in-project.el ends here
;;; extracted from rinari
;; by Phil Hagelberg and Doug Alcorn
;; refactored to work with project-local-variables.el

(require 'project-local-variables)

(defvar ffip-regexp
  (concat ".*\\.\\(" (mapconcat (lambda (x) x) '("rb" "rhtml" "el") "\\|") "\\)")
  "Regexp of things to look for when using find-file-in-project.
For Rails projects, a good value is \".*\\.\\(rb\\|rhtml\\)\"")

(defvar ffip-find-options
  ""
  "Extra options to pass to `find' when using find-file-in-project.

Use this to exclude portions of your project: \"-not -regex \\\".*vendor.*\\\"\"")

;; TODO: deal with files with the same name but different paths like
;; the version in rinari does (without being slow)
(defun ffip-project-files ()
  "Return an assoc-list of all filenames and their path."
  (mapcar (lambda (file) (cons
		     (file-name-nondirectory file)
		     file))
	  (split-string (shell-command-to-string (concat "find " (project-root)
							 " -type f -regex \""
							 ffip-regexp
							 "\" " ffip-find-options)))))
  
(defun find-file-in-project ()
  (interactive)
  (let* ((project-files (ffip-project-files))
	 (file (if (functionp 'ido-completing-read)
		   (ido-completing-read "Find file in project: "
					(mapcar 'car project-files))
		 (completing-read "Find file in project: "
				  (mapcar 'car project-files)))))
    (find-file (cdr (assoc file project-files)))))

(defun project-root (&optional dir)
  (file-name-directory (plv-find-project-file default-directory "")))

(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)

(provide 'find-file-in-project)
;;; find-file-in-project.el ends here

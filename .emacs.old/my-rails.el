
;;; Part of my .emacs project

;; by Phil Hagelberg
;; Much thanks to RMS and the folks at emacswiki.org.

;; Note: this relies on files found in my dotfiles repository:
;; http://github.com/technomancy/dotfiles

(defun rails-root (&optional dir)
  (or dir (setq dir default-directory))
  (if (file-exists-p (concat dir "config/environment.rb"))
      dir
    (unless (equal dir "/")
      (rails-root (expand-file-name (concat dir "../"))))))

;;;###autoload
(defun rails-console ()
  (interactive)
  (run-ruby (concat (rails-root) "script/console")))

(defun rails-find-view ()
  "View toggling for rails"
  (interactive)
  (let* ((funname (which-function))
 	 (cls (rails-make-dirname (rails-name-components funname)))
	 (fn (and (string-match "#\\(.*\\)" funname) (match-string 1 funname)))
 	 (appdir (file-name-directory (directory-file-name (file-name-directory (buffer-file-name))))))
    (find-file (concat appdir "views/" cls "/" fn ".rhtml"))))

(defun rails-find-action (action &optional controller)
  (interactive)
  (if controller
      (find-file (concat (rails-root "app/controllers/" controller ".rb"))))
  (beginning-of-buffer)
  (search-forward-regexp (concat "def\\s" action))
  (recenter))

(defun rails-controller-name-from-view ()
  (concat (rails-root) 
	  "app/controllers/"
	   (file-name-nondirectory 
	    (expand-file-name "."))
	  "_controller.rb"))

;;; TODO: make this work with rails2-style view filenames
(defun rails-find-action ()
  (interactive)
  (let ((action (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
    (find-file (rails-controller-name-from-view))
    (beginning-of-buffer)
    (search-forward-regexp (concat "def *" action))
    (recenter)))

(defun rails-name-components (name)
  "Helper for view toggling"
  (let ((case-fold-search nil))
	(labels ((rnc (in)
			(let ((ind (string-match "\\([A-Z][a-z0-9]+\\)[A-Z]" name in)))
			  (if (eq ind nil)
			      nil
			    (cons (downcase (match-string 1 name)) (rnc (match-end 1)))))))
	  (rnc 0))))

(defun rails-make-dirname (comps)
  "Helper for view toggling"
  (reduce #'(lambda (str next) (concat str (concat "_" next))) comps))

(defun rails-insert-erb-skeleton (no-equals)
  (interactive "P")
  (setq no-e no-equals)
  (insert "<%")
  (unless no-equals (insert "="))
  (insert "  %>")
  (backward-char 3))

(define-key ruby-mode-map (kbd "C-c C-v") 'rails-find-view)
(define-key ruby-mode-map (kbd "C-c C-t") 'toggle-buffer)
(define-key ruby-mode-map (kbd "C-c C-M-t") 'ruby-test-file)
;;(define-key ruby-mode-map (kbd "C-c C-S-t") 'ruby-test-one)

;; nxhtml-mode is the cats!
(eval-after-load 'nxhtml-mode
  '(progn
     (define-key nxhtml-mode-map (kbd "C-c C-v") 'rails-find-action)
     (define-key nxhtml-mode-map (kbd "C-c C-e") 'rails-insert-erb-skeleton)))

(provide 'my-rails)
;;; my-rails.el ends here
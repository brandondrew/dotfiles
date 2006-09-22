 ;;;
;;;  arorem.el -
;;;
;;;  Assorted Ruby on Rails Emacs Modes
;;;  
;;;  Latest version may be found at
;;;  http://dev.technomancy.us/phil/wiki/arorem
;;;

;;; Motivation

;; Arorem used to be Another Ruby on Rails Emacs Mode. However, it
;; turns out rather than having a single mode for Rails editing, it's
;; more effective work with a combination of a number of modes and add
;; a few rails-specific features to ruby-mode. So Arorem has become a
;; modular collection of elisp to help with rails.

;;; Usage

;; - toggle
;; - console
;; - ri
;; - snippets
;; - rhtml
;; - psvn help?
;; - config file registers?
;;   - environment.rb
;;   - database.yml
;;   - routes.rb
;;   - schema.rb
;; - rake?

;;; TODO

;; See the open ticket list: http://dev.technomancy.us/phil/report/10

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar arorem-version "0.2")
(require 'ruby-mode)
(require 'inf-ruby)
(require 'snippet)
(require 'rhtml-mode)
(require 'toggle)
(require 'rails-test)

(load "abbrevs")

;;; view toggling

(defun ruby-find-view ()
  (interactive)
  (let* ((funname (which-function))
 	 (cls (and (string-match "\\(.*\\)Controller#" funname) (downcase (match-string 1 funname))))
 	 (fn (and (string-match "#\\(.*\\)" funname) (match-string 1 funname)))
 	 (appdir (file-name-directory (directory-file-name (file-name-directory (buffer-file-name))))))
    (find-file (concat appdir "views/" cls "/" fn ".rhtml"))))

;;; find-file-in-project

(defvar project-files-table ())

(defun populate-project-files-table (file)
  (if (file-directory-p file)
      (mapc 'populate-project-files-table (directory-files file t "^[^\.]"))
    (let* ((file-name (file-name-nondirectory file))
	   (existing-record (assoc file-name project-files-table))
	   (unique-parts (get-unique-directory-names file (cdr existing-record))))
      (if existing-record
	  (let ((new-key (concat file-name " - " (car unique-parts)))
		(old-key (concat (car existing-record) " - " (cadr unique-parts))))
	    (setf (car existing-record) old-key)
	    (setq project-files-table (acons new-key file project-files-table)))
	(setq project-files-table (acons file-name file project-files-table))))))

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

(defun project-files ()
; uncomment these lines if it's too slow to load the whole project-files-table
;  (when (or (not project-files-table) ; initial load
;	    (not (string-match (rails-root) (cdar project-files-table)))) ; switched projects
    (setq project-files-table nil)
    (populate-project-files-table (concat (rails-root) "/app"))
    project-files-table)


(defun rails-root (&optional dir)
  (or dir (setq dir default-directory))
  (if (file-exists-p (concat dir "config/environment.rb"))
      dir
    (if (equal dir  "/")
	nil
      (rails-root (expand-file-name (concat dir "../"))))))

(defun arorem-console ()
  (interactive)
  (run-ruby (concat (rails-root) "/script/console")))

(define-key ruby-mode-map
  "\C-c\C-s" 'arorem-console)
(define-key ruby-mode-map
  "\C-c\C-v" 'ruby-find-view)
(define-key ruby-mode-map
  "\C-c\C-t" 'toggle-test)
(define-key ruby-mode-map
  "\C-c\C-f" 'toggle-fixture)
(define-key ruby-mode-map
  "\C-x\C-\M-F" 'find-file-in-project)
(define-key ruby-mode-map
  [(shift f5)] 'ruby-test-function)
(define-key ruby-mode-map
  [(control f5)] (lambda () (interactive)
		   (compile (concat "ruby " (file-name-nondirectory buffer-file-name)))))

(defun rails-get-path (path)
  (interactive "MPath: ")
  (switch-to-buffer (concat "rails-" path))
  (insert (shell-command-to-string (concat (rails-root) "/script/runner \"app = ActionController::Integration::Session.new; app.get '"
					   path "'; puts app.response.body\"")))
  (html-mode)
  ;; work around the fact that it inserts some random testing output
  (kill-region (search-backward "Loaded suite") (point-max)))


;; (global-set-key [f7] 'symbol-links-to-string-links)
;; (defun symbol-links-to-string-links ()
;;   (interactive)
;;   (query-replace-regexp " => :\\([^,]*\\)" " => '\\1'"))

(provide 'arorem)

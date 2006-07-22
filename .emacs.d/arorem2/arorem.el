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
;; - psvn help
;; - config file registers
;; - rake

;;; TODO

;; See the open ticket list: http://dev.technomancy.us/phil/report/10

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar arorem-version "0.3")
(require 'ruby-mode)
(require 'inf-ruby)
(require 'toggle)
(require 'ri-ruby)
(require 'snippet)
(require 'rhtml-mode)
;(require 'abbrevs)


(set-register ?d '(file . (concat (rails-root) "/config/database.yml")))
(set-register ?v '(file . (concat (rails-root) "/config/environment.rb")))
(set-register ?r '(file . (concat (rails-root) "/config/routes.rb")))

;;; view toggling

(defun ruby-find-view ()
  (interactive)
  (let* ((funname (which-function))
 	 (cls (and (string-match "\\(.*\\)Controller#" funname) (downcase (match-string 1 funname))))
 	 (fn (and (string-match "#\\(.*\\)" funname) (match-string 1 funname)))
 	 (appdir (file-name-directory (directory-file-name (file-name-directory (buffer-file-name))))))
    (find-file (concat appdir "views/" cls "/" fn ".rhtml"))))

(which-function-mode t) ; required for ruby-find-view

;;; script/console

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


;;; key bindings

(define-key ruby-mode-map
  "\C-c\C-s" 'arorem-console)
(define-key ruby-mode-map
  "\C-c\C-v" 'ruby-find-view)
(define-key ruby-mode-map
  "\C-c\C-t" 'toggle-test)
(define-key ruby-mode-map
  "\C-c\C-x" 'toggle-fixture)
(define-key ruby-mode-map
  "\C-c\C-h" 'ri)
(define-key ruby-mode-map
  "\C-c\C-r" (lambda () (interactive) (dired (rails-root))))

;; rake stuff

(defun rake (task &rest args)
  (shell-command (concat "cd " (rails-root) "; rake " task)))

(define-key ruby-mode-map
  "\C-c\C-d" (lambda () (interactive) (rake "deploy")))
(define-key ruby-mode-map
  "\C-c\C-m" (lambda () (interactive) (rake "migrate")))

(define-key ruby-mode-map
  "\C-c\C-u" (lambda () (interactive) (rake "test:units")))
(define-key ruby-mode-map
  "\C-c\C-f" (lambda () (interactive) (rake "test:functionals")))
(define-key ruby-mode-map
  "\C-c\C-i" (lambda () (interactive) (rake "test:integration")))


(defun extract-partial (begin end partial-name)
  (interactive "r\nsName your partial: ")
  (kill-region begin end)
  (find-file (concat "_" partial-name ".rhtml"))
  (yank)
  (pop-to-buffer nil)
  (insert (concat "<%= render :partial => '" partial-name "' %>\n")))


(provide 'arorem)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; My .emacs file
; by Phil Hagelberg
;
; Much thanks to emacswiki.org and RMS.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq load-path (append '("~/.emacs.d") load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     loading modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PHP mode
(autoload 'php-mode "php-mode")
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;; .js (javascript) loads C mode (until I find something better)
(add-to-list 'auto-mode-alist '("\\.js$" . c-mode))

;; .rhtml loads html
(add-to-list 'auto-mode-alist '("\\.rhtml$" . html-mode))

;; html helper
(autoload 'html-helper-mode "html-helper-mode")
(add-to-list 'auto-mode-alist '("\\.html$" . html-helper-mode))

;; CSS-mode
(autoload 'css-mode "css-mode")
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))

;; syntax highlighting by default (needs to be done before ruby-electric)
(global-font-lock-mode)

;; Ruby help
(require 'ruby-electric)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))

;; Rails stuff
(defun try-complete-abbrev (old)
  (if (expand-abbrev) t nil))

(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
        try-complete-file-name
        try-expand-dabbrev))

;(require 'rails)

(defun my-ruby-mode-hook ()
  (ruby-electric-mode))
(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)

;; integrated subversion
(require 'psvn)

;; make pretty!
(require 'color-theme)
(require 'zenburn)
(color-theme-zenburn)

(require 'tabbar)
(tabbar-mode)

(iswitchb-mode 1)
(setq iswitchb-buffer-ignore '("^\\*"))

;; wait for emacs22 for these:
;; Errors to emacs
;(load 'ete)

;; Make ruby-mode usable for hs-minor-mode.
;(add-to-list 'hs-special-modes-alist
;         (list 'ruby-mode
;           (concat ruby-block-beg-re "\|{")
;           (concat ruby-block-end-re "\|}")
;           "#"
;           'ruby-forward-sexp nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key "\C-x\C-h" 'help-command)
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\M-g" 'goto-line)

; back to tabbar
(global-set-key [(control shift up)] 'tabbar-backward-group)
(global-set-key [(control shift down)] 'tabbar-forward-group)
(global-set-key [(control shift left)] 'tabbar-backward)
(global-set-key [(control shift right)] 'tabbar-forward)

(defvar ys-eshell-wins nil)
(global-set-key "\C-cs" (lambda (win-num)
			  (interactive "p")
			  (message "win-num %s" win-num)
			  (let ((assoc-buffer (cdr (assoc win-num ys-eshell-wins))))
			    (if (not (buffer-live-p assoc-buffer))
				(progn ; the requested buffer not there 
				  (setq assoc-buffer (eshell t))
				  (setq ys-eshell-wins (assq-delete-all win-num ys-eshell-wins))
				  (add-to-list 'ys-eshell-wins (cons win-num assoc-buffer))))
			    (switch-to-buffer assoc-buffer)
			    (rename-buffer (concat "*eshell-" (int-to-string win-num) "*"))
			    assoc-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     registers
; to load, C-x r j <register-name>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; local .emacs
(set-register ?l '(file . "~/.emacs"))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     misc things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; don't tell ME the file's changed cos it HASN'T!
(defun ask-user-about-supersession-threat (fn) t)

(setq font-lock-maximum-decoration t)
(blink-cursor-mode -1)
(auto-compression-mode 1) ; load .gz's automatically
(auto-image-file-mode 1) ; display images inline
(setq inhibit-startup-message t)
(setq transient-mark-mode t)
(setq show-paren-mode t)
(mouse-wheel-mode 1) ; duh! this should be default.
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; don't clutter directories!
(setq backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.baks"))))
(setq auto-save-directory (expand-file-name "~/.emacs.baks"))

;; if your file is a script, make it executable
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;(server-start)

(defun www2 ()
  "Open a tunneled connection to www2 via jacob"
  (interactive)
  (find-file "/multi:ssh:phil@leela.gotdns.com:ssh:d1103784@www2.biola.edu:"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;    Nifty things to remember and hopefully use
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; M-z zap to char
; C-u C-SPC jump to previous edit
; M-/ autocomplete word 
; M-! insert output of shell command
; M-| replace region with shell output
; M-x thumbs
; C-r-k Rectangle kill

; C-x h select all
; C-M-\ indent

; Macros
; C-m C-r to begin
; name it, and do stuff
; C-s to save

; temp macros
; C-m C-m to start recording
; C-m C-s to stop
; C-m C-p to play


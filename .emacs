
;;; My .emacs file

;; by Phil Hagelberg
;; Much thanks to emacswiki.org and RMS.

;; Note: this relies on files found in my .emacs.d:
;; http://dev.technomancy.us/phil/browser/dotfiles/.emacs.d

;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; "What you want is probably already in Emacs. If you don't use Emacs,
;; start. If do use Emacs, use it more. If you *still* can't do what you
;; want to do, you probably shouldn't do it."
;; -Shawn Betts, Ratpoison FAQ

;; I think of emacs as Shkembe Chorba. As one Bulgarian saying goes:
;; 'Shkembe chorba is best when it's hot, peppery and someone praises it'.
;; -http://programming.reddit.com/info/uw44/comments/cuze4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; server singleton

(unless (string-equal "root" (getenv "USER"))
  (when (and (> emacs-major-version 22)
	     (or (not (boundp 'server-process))
		 (not (eq (process-status server-process)
			  'listen))))
    (server-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(toggle-debug-on-error)

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/behave")
(add-to-list 'load-path "~/.emacs.d/rinari/rhtml")
(add-to-list 'load-path "~/.emacs.d/rails")
(add-to-list 'load-path "~/.emacs.d/jabber")
(add-to-list 'load-path "~/.emacs.d/w3m")
(add-to-list 'load-path "~/.emacs.d/dictionary")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; loading modes

(autoload 'php-mode "php-mode")
(autoload 'yaml-mode "yaml-mode")
(autoload 'css-mode "css-mode")
(autoload 'js-mode "js-mode" "" t)
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(autoload 'lisppaste-paste-region "lisppaste" "" t)
(autoload 'ebby "ebby" "" t)
(autoload 'htmlize-region "htmlize" "" t)
(autoload 'htmlize-buffer "htmlize" "" t)
(autoload 'color-theme-zenburn "zenburn")
(autoload 'textile-to-html-region "textilize")
(autoload 'pastie-region "pastie" "" t)
(autoload 'jabber-connect "jabber" "" t)
(autoload 'w3m "w3m" "" t)
(autoload 'tail-file "tail.el" "Tail a file." t)

(require 'psvn)
(require 'toggle)
(require 'compile)
(require 'which-func)
(require 'wrap-region)
(require 'elunit)
(require 'flog)
(require 'test-unit)
(require 'show-wspace)
(require 'esh-mode)

(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))

(add-hook 'js-mode-hook 'moz-minor-mode)
(add-hook 'java-mode-hook 'moz-minor-mode)

(load "dictionary-init")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My support files and configurations

(require 'ruby)
(require 'bindings)
(require 'defuns)
(require 'lisp)
(require 'helma)
(require 'registers)
(require 'misc)

(if (functionp 'rcirc)
    (load "rcirc-config"))

(if (functionp 'jabber-connect)
    (load "jabber-config"))

(let ((system-specific-config (concat "~/.emacs.d/" (substring (shell-command-to-string "hostname") 0 -1)".el")))
  (if (file-exists-p system-specific-config)
      (load system-specific-config)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Nifty things to remember and hopefully use

; M-z zap to char
; C-u C-SPC jump to previous edit
; M-/ autocomplete word 
; M-! insert output of shell command
; M-| replace region with shell output
; M-x thumbs
; C-r k Rectangle kill

; C-x h select all
; C-M-\ indent-region

; Macros
; C-m C-r to begin
; name it, and do stuff
; C-s to save

; temp macros
; C-m C-m to start recording
; C-m C-s to stop
; C-m C-p to play

; M-C-p, M-C-n back and forward blocks
; C-c C-s irb when in ruby-mode

; C-x n n narrow visibility of buffer to region
; C-x n w widen to full buffer

; Dired
; mark with 'm', press 'Q' for multi-file find/replace
; C-j launch dired when ido-mode is enabled

; list-colors-display
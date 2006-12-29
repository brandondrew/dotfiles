
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

;; I think of emacs as 49Shkembe Chorba. As one Bulgarian saying goes:
;; 'Shkembe chorba is best when it's hot, peppery and someone praises
;; it'.
;; -http://programming.reddit.com/info/uw44/comments/cuze4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(toggle-debug-on-error)

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/rinari")
(add-to-list 'load-path "~/.emacs.d/rinari/rhtml")
(add-to-list 'load-path "~/.emacs.d/jabber")
(add-to-list 'load-path "~/.emacs.d/w3m")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/sawfish/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; loading modes

(autoload 'php-mode "php-mode")
(autoload 'yaml-mode "yaml-mode")
(autoload 'css-mode "css-mode")
(autoload 'js-mode "js-mode" "" t)
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(autoload 'tabbar-mode "tabbar")
(autoload 'lisppaste-paste-region "lisppaste" "" t)
(autoload 'ebby "ebby" "" t)
(autoload 'htmlize-region "htmlize" "" t)
(autoload 'htmlize-buffer "htmlize" "" t)
(autoload 'color-theme-zenburn "zenburn")
(autoload 'textile-to-html-region "textilize")
(autoload 'sawfish-mode "sawfish" "" t)
(autoload 'pastie-region "pastie" "" t)
(autoload 'jabber-connect "jabber" "" t)
(autoload 'w3m "w3m" "" t)

(require 'psvn)
(require 'compile)
(require 'which-func)
(require 'elunit)

(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))

(add-hook 'js-mode-hook 'moz-minor-mode)
(add-hook 'java-mode-hook 'moz-minor-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My support files and configurations

(require 'bindings)
(require 'ruby)
(require 'lisp)
(require 'registers)
(require 'defuns)
(require 'misc)

(if (functionp 'rcirc)
    (load "rcirc-config"))

(when (functionp 'jabber-connect)
  (setq jabber-nickname "")
  (setq jabber-password nil)
  (setq jabber-resource (concat "emacs-" (shell-command-to-string "hostname")))
  (setq jabber-server "hagelb.org")
  (setq jabber-username "phil"))

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
; C-M-\ indent

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

; C-x n n narrow visibility of buffer to selection
; C-x n w widen to full buffer

; Dired
; mark with 'm', press 'Q' for multi-file find/replace
; C-j launch dired when ido-mode is enabled
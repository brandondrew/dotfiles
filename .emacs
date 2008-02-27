
;;; My .emacs file

;; by Phil Hagelberg
;; Much thanks to emacswiki.org and RMS.

;; Note: this relies on files found in my .emacs.d:
;; http://git.caboo.se/?p=technomancy.git;a=summary

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
(add-to-list 'load-path "~/.emacs.d/jabber")
(add-to-list 'load-path "~/.emacs.d/w3m")
(add-to-list 'load-path "~/.emacs.d/rinari/rhtml")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; loading modes

(autoload 'php-mode "php-mode")
(autoload 'yaml-mode "yaml-mode")
(autoload 'css-mode "css-mode")
(autoload 'javascript-mode "javascript" "" t)

(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
;;(autoload 'irbsh "irbsh" "irbsh - IRB.extend ShellUtilities" t)
;;(autoload 'irbsh-oneliner-with-completion "irbsh" "irbsh oneliner" t)

(autoload 'lisppaste-paste-region "lisppaste" "" t)
(autoload 'htmlize-region "htmlize" "" t)
(autoload 'htmlize-buffer "htmlize" "" t)
(autoload 'color-theme-zenburn "zenburn")
(autoload 'pastie-region "pastie" "" t)
(autoload 'tail-file "tail.el" "Tail a file." t)
(autoload 'column-marker-1 "column-marker.el" "" t)
(autoload 'git-blame-mode "git-blame" "" t)

(autoload 'jabber-connect "jabber" "" t)
(autoload 'w3m "w3m" "" t)

(require 'cl)
(require 'saveplace)
(require 'toggle)
(require 'compile)
(require 'which-func)
(require 'show-wspace)
(require 'esh-mode)
(require 'git)
(require 'buffer-stack)
(require 'install-elisp)
(require 'elunit)

(load "nxml/autostart.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My support files and configurations

(require 'my-calendar)
(require 'my-eshell)
(require 'my-bindings)
(require 'my-defuns)
(require 'my-registers)
(require 'my-misc)
(require 'my-hook-setup)
(require 'my-music)

(require 'my-ruby)
(require 'my-lisp)
(require 'my-js)

(if (functionp 'jabber-connect)
    (load "jabber-config"))

(load "rcirc-config")

(let ((system-specific-config (concat "~/.emacs.d/" (substring (shell-command-to-string "hostname") 0 -1)".el")))
  (if (file-exists-p system-specific-config)
      (load system-specific-config)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Nifty things to remember and hopefully use

;;; M-z zap to char
;;; C-u C-SPC jump to previous edit
;;; M-/ autocomplete word
;;; M-! insert output of shell command
;;; M-| replace region with shell output
;;; M-x thumbs
;;; C-x r k Rectangle kill

;;; temp macros
;;; C-m C-m to start recording
;;; C-m C-s to stop
;;; C-m C-p to play

;;; Macros
;;; C-m C-r to begin
;;; name it, and do stuff
;;; C-s to save


;;; M-C-p, M-C-n back and forward blocks
;;; C-c C-s irb when in ruby-mode

;;; C-x n n narrow visibility of buffer to region
;;; C-x n w widen to full buffer

;;; Dired
;;; mark with 'm', press 'Q' for multi-file find/replace
;;; C-j launch dired when ido-mode is enabled

;;; G G gnus keyword search
;;; G T show context in keyword search

;;; list-colors-display

;;; C-x v g - blame
;;; C-x v d - dired (v t to show all files)
;;; C-x v = - diff

;;; TODO:

;; publish
;;  - ebby

;; install-elisp.el
;;  - do something about metadata: description, author, version, url
;;  - make sure each installation buffer gets focus/attention

;; set up rcodetools

;; get my-red-face and friends (my-misc.el) highlighting in ruby-mode

;; fix flymake for JS and Ruby

;; fix mumamo for rhtml

;; make an emacs peepcode

;; submit pcmpl-ssh.el to emacs?

;; fix that rcirc completion
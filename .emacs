
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

;;; Fix for a bug in CVS Emacs 2 April 08; remove when fixed upstream:
(defun handle-shift-selection ())

(toggle-debug-on-error)

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/jabber")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; loading modes

;;; Regenerate the autoload file if it doesn't exist or it's are too
;;; old. (2 weeks or so)
(let ((autoload-file "~/.emacs.d/loaddefs.el"))
  (if (or (not (file-exists-p autoload-file))
	  (< (+ (car (nth 5 (file-attributes autoload-file))) 20)
	     (car (current-time))))
      (let ((generated-autoload-file autoload-file))
	(update-directory-autoloads "~/.emacs.d/")))
  (load autoload-file))

(autoload 'yaml-mode "yaml-mode")
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(autoload 'tail-file "tail.el" "Tail a file." t)
(autoload 'lisppaste-paste-region "lisppaste" "" t)
(autoload 'rafb-paste "rafb-paste" "" t)
(autoload 'pastie-region "pastie" "" t)
(autoload 'top-mode "top-mode" "" t)
(autoload 'jabber-connect "jabber" "" t)
(autoload 'scpaste "scpaste" "" t)

(require 'cl)
(require 'saveplace)
(require 'toggle)
(require 'compile)
(require 'which-func)
(require 'esh-mode)
(require 'install-elisp)
(require 'elunit)
(require 'cc-defs)
(load "nxml/autostart.el")

;; some boxes won't have this installed from CVS
;; if that is so, try roastbeef install emacs-w3m
(ignore-errors (require 'w3m-load))

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

(load "jabber-config")
(load "rcirc-config")

(let ((system-specific-config (concat "~/.emacs.d/"
				      (substring (shell-command-to-string "hostname") 0 -1)
				      ".el")))
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
;;; M-x follow-mode

;;;; Rectangles
;;; C-x r k Rectangle kill
;;; C-x r y Rectangle yank
;;; C-x r c Rectangle clear (replace with space)
;;; C-x r o Insert space rectangle
;;; C-x r t Replace rectangle with string

;;;; temp macros
;;; C-m C-m to start recording
;;; C-m C-s to stop
;;; C-m C-p to play

;;;; Macros
;;; C-m C-r to begin
;;; name it, and do stuff
;;; C-s to save

;;;; Ruby
;;; M-C-p, M-C-n back and forward blocks
;;; C-c C-s irb when in ruby-mode
;;; C-c C-r Send region to inf-ruby
;;; C-c M-r Send region to inf-ruby and switch to buffer
;;; C-c C-l Load file in ruby
;;; C-c C-x Send definition

;;;; Display
;;; C-u N C-x $ only display lines with less than N spaces of indentation
;;; C-x $ disable above
;;; C-x n n narrow visibility of buffer to region
;;; C-x n w widen to full buffer

;;;; Dired
;;; mark with 'm', press 'Q' for multi-file find/replace
;;; C-j launch dired when ido-mode is enabled

;;;; Gnus Namazu
;;; G G gnus keyword search
;;; G T show context in keyword search

;;; list-colors-display

;;;; VC
;;; C-x v g - blame (V to toggle committer data)
;;; C-x v d - dired (v t to show all files)
;;; C-x v = - diff
;;; C-x v ~ - diff against specific version

;;;; Eshell
;;; piping: ifconfig > #<buffer interfaces>
;;; sudo: cd/sudo::

;;; sudo ionice -c3 -ppid

;;; Well, we already have `C-x r w R' (window-configuration-to-register)
;;; and `C-x r f R' (frame-configuration-to-register) for saving window
;;; configurations, and `C-x r j R' for restoring them.

;;; TODO:

;; steal ZSS defadvice in setup-aliases.el for find-file-at-point
;; steal ZSS greying out lines that get too long

;; publish
;;  - ebby

;; install-elisp.el
;;  - do something about metadata: description, author, version, url
;;  - make sure each installation buffer gets focus/attention

;; set up rcodetools

;; get my-red-face and friends (my-misc.el) highlighting in ruby-mode

;; fix flymake for JS and Ruby

;; make an emacs peepcode

;; fix that rcirc completion

;; fix ssh/scp pcompletion to use the tramp functions?

;; grab music stats from /var/log/mpd/mpd.log
;; * graph them
;; * come up with a "not recently played" command

;; write a screenshot+upload combo

;; look into adding hyperlinks to *vc-change-log*

;; check out js2-mode.el: http://code.google.com/p/js2-mode/
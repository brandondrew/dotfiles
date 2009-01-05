;;; My .emacs project

;; by Phil Hagelberg
;; Much thanks to RMS and emacswiki.org

;; All files distributed under the same terms as GNU Emacs unless
;; otherwise stated.

;; Note: this relies on files found in my .emacs.d:
;; http://github.com/technomancy/dotfiles

;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; "What you want is probably already in Emacs. If you don't use Emacs,
;; start. If do use Emacs, use it more. If you *still* can't do what you
;; want to do, you probably shouldn't do it."
;; -Shawn Betts, Ratpoison FAQ

;; "I think of emacs as Shkembe Chorba. As one Bulgarian saying goes:
;; 'Shkembe chorba is best when it's hot, peppery and someone praises it'."
;; -http://programming.reddit.com/info/uw44/comments/cuze4

;; "Lisp dialects [...] are wonderful provided you don't mind spending
;; 20% of your time rejoicing in the beauty that is a dynamic language
;; with uniform syntax and a real macro system."
;; - http://ratpoison.nongnu.org/inspiration.html

;;; On with the show:

(toggle-debug-on-error)

;;; Fix for a bug in CVS Emacs 2 April 08; remove when fixed upstream:
(require 'cl)
(defun handle-shift-selection (&rest args))

;;;
;;; server singleton
;;;

(unless (string-equal "root" (getenv "USER"))
  (when (and (> emacs-major-version 22)
             (or (not (boundp 'server-process))
                 (not (eq (process-status server-process)
                          'listen))))
    (server-start)))

;;;
;;; load-path, autoloads, and requires
;;;

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/jabber")
;; (add-to-list 'load-path "~/src/nxhtml")
(add-to-list 'load-path "~/src/magit")
(add-to-list 'load-path "~/src/ert")

;; Regenerate the autoload file if it doesn't exist or it's too
;; old. (2 weeks or so)

(let ((autoload-file "~/.emacs.d/loaddefs.el"))
  (if (or (not (file-exists-p autoload-file))
          (< (+ (car (nth 5 (file-attributes autoload-file))) 20)
             (car (current-time))))
      (let ((generated-autoload-file autoload-file))
        (message "Updating autoloads...")
        (update-directory-autoloads "~/.emacs.d/")))
  (load autoload-file))

;; (autoload 'nxhtml-mode "autostart" "" t)
;; (autoload 'nxml-mode "autostart" "" t)

(autoload 'w3m "w3m-load" "" t)
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(autoload 'whitespace-mode "whitespace" "Toggle whitespace visualization." t)
(autoload 'tail-file "tail" "Tail a file." t)
(autoload 'lisppaste-paste-region "lisppaste" "" t)
(autoload 'top "top-mode" "" t)
(autoload 'jabber-connect "jabber" "" t)
(autoload 'cheat "cheat" "" t)
(autoload 'magit-status "magit" "" t)
(autoload 'vivid-chalk "vivid-chalk" "" t)
(autoload 'ri "ri-ruby" "" t)

(require 'cl)
(require 'saveplace)
(require 'toggle)
(require 'compile)
(require 'which-func)
(require 'cc-defs)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'idle-highlight)

(ignore-errors
  (load "elpa/package.el")
  (package-initialize))

;;
;; My support files and configurations
;;

(require 'my-eshell)
(require 'my-bindings)
(require 'my-defuns)
(require 'my-registers)
(require 'my-misc)
(require 'my-hook-setup)
(require 'my-lisp)
(require 'my-vc)

(eval-after-load 'ruby-mode '(require 'my-ruby))
(eval-after-load 'javascript-mode '(require 'my-js))
(eval-after-load 'jabber '(load "my-jabber"))
(eval-after-load 'rcirc '(load "my-rcirc"))

(setq system-specific-config (concat "~/.emacs.d/" system-name ".el"))

(if (file-exists-p system-specific-config)
    (load system-specific-config))

(ignore-errors
  (load "~/src/conspire/lib/conspire/support/conspire.el"))

;;;
;;;  Cheat Sheet
;;;

;;; General
;; M-z zap to char
;; C-u C-SPC jump to previous edit
;; M-! insert output of shell command
;; M-| replace region with shell output
;; M-x thumbs
;; M-x follow-mode
;; C-u C-SPC jump to mark ring
;; C-r during query-replace for recursive edit
;; C-M-c to close recursive edit

;;; Custom bindings
;; C-c \ align-regexp
;; C-c r revert buffer
;; C-x M-k kill buffer and window
;; C-c d toggle-dedicated-window
;; C-c p message point

;;; Rectangles
;; C-x r k Rectangle kill
;; C-x r y Rectangle yank
;; C-x r c Rectangle clear (replace with space)
;; C-x r o Insert space rectangle
;; C-x r t Replace rectangle with string

;;; Ruby
;; M-C-p, M-C-n back and forward blocks
;; C-c C-s irb when in ruby-mode
;; C-c C-r Send region to inf-ruby
;; C-c M-r Send region to inf-ruby and switch to buffer
;; C-c C-l Load file in ruby
;; C-c C-x Send definition

;;; Display
;; C-u N C-x $ only display lines with less than N spaces of indentation
;; C-x $ disable above
;; C-x n n narrow visibility of buffer to region
;; C-x n w widen to full buffer

;;; Dired
;; mark with 'm', press 'Q' for multi-file find/replace
;; C-j launch dired when ido-mode is enabled

;;; Gnus Namazu
;; G G gnus keyword search
;; G T show context in keyword search

;; list-colors-display

;; Handy unicode chars: ♪ ♫ ♬ ‽ ☞

;;; VC
;; C-x v g - blame (V to toggle committer data)
;; C-x v = - diff (C-u to specify revision)
;; C-x v ~ - visit specific revision
;; C-x v l - log
;; C-x g   - magit

;;; Eshell
;; piping: ifconfig > #<buffer interfaces>

;; Well, we already have `C-x r w R' (window-configuration-to-register)
;; and `C-x r f R' (frame-configuration-to-register) for saving window
;; configurations, and `C-x r j R' for restoring them.

;; Profiling: time emacs -e save-buffers-kill-terminal

;;;
;;; TODO:
;;;

;; ert.el
;; - write the manual
;; - port over mode-unit thingies

;; magit.el
;; - catch up w/ marius' changes
;; - test cases

;; ri-emacs
;; - submit patches to bug tracker

;; nxhtml-mode
;; - load into ELPA
;; - bugfix: apply ruby-mode to chunks on first load

;; ruby-mode.el
;; - finish documentation
;; - test cases
;; - write failing test cases for bugs
;; - add test runner using compile to inf-ruby.el

;; conkeror
;; - C-k should kill buffers when in the switcher
;; - shouldn't have to M-x network-go-online to view localhost

;; investigate file-name-cache

;; changing the font doesn't change the rendered frame size

;; use git submodules in dotfiles project

;; submit patched rcirc completion

;; have an auto-downloading init script you can curl and pipe to sh

;; Look into adding zeroconf-chat to jabber.el

;; DBus should allow IRC/Jabber reconnect upon resume

;;; Minor

;; fix ruby-get-old-input to not care about what the prompt looks like

;;; Long-term:

;; figure out how to get nnml under version control w/o merge conflicts

;; do something about getting a better ruby highlighter:
;; http://rubyforge.org/projects/ruby-tp-dw-gram/
;; http://cedet.cvs.sourceforge.net/cedet/cedet/contrib/

;; make an emacs peepcode

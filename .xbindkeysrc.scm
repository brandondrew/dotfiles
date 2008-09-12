;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start of xbindkeys guile configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This configuration is guile based.
;;   http://www.gnu.org/software/guile/guile.html
;; any functions that work in guile will work here.
;; see EXTRA FUNCTIONS:

;; Version: 1.8.2

;; If you edit this file, do not forget to uncomment any lines
;; that you change.
;; The semicolon(;) symbol may be used anywhere for comments.

;; List of modifier:
;;   Release, Control, Shift, Mod1 (Alt), Mod2 (NumLock),
;;   Mod3 (CapsLock), Mod4, Mod5 (Scroll).

;; The release modifier is not a standard X modifier, but you can
;; use it if you want to catch release instead of press events

;; By defaults, xbindkeys does not pay attention to modifiers
;; NumLock, CapsLock and ScrollLock.
;; Uncomment the lines below if you want to use them.
;; To dissable them, call the functions with #f

;;;;; Scheme API reference
;;;;
;; Optional modifier state:
;; (set-numlock! #f or #t)
;; (set-scrolllock! #f or #t)
;; (set-capslock! #f or #t)
;;
;; Shell command key:
;; (xbindkey key "foo-bar-command [args]")
;; (xbindkey '(modifier* key) "foo-bar-command [args]")
;;
;; Scheme function key:
;; (xbindkey-function key function-name-or-lambda-function)
;; (xbindkey-function '(modifier* key) function-name-or-lambda-function)
;;
;; Other functions:
;; (remove-xbindkey key)
;; (run-command "foo-bar-command [args]")
;; (grab-all-keys)
;; (ungrab-all-keys)
;; (remove-all-keys)
;; (debug)

;;;; Music functions

(map (lambda (binding) (xbindkey (car binding) (cdr binding)))
     (list '(F5 . "~/bin/mpc-notify")
           '((Control F5) . "~/bin/mpc-playlist")
           '(F6 . "~/bin/mpc-random")
           '(F7 . "~/bin/mpc-choose")
           '(F8 . "mpc toggle")
           '(F9 . "mpc prev")
           '(F10 . "mpc next")
           '(F12 . "vlc -f ~/documents/movies/misc/rick.flv"))) ;; tee hee

;;;; Other launchers

(xbindkey '(mod4 e) "emacs")
(xbindkey '(mod4 f) "firefox")
(xbindkey '(mod4 s) "f-spot")
(xbindkey '(mod4 c) "~/src/conkeror/conkeror -E 'default_pref(\"conkeror.rcfile\",\"/home/phil/.conkeror.d\")'")
(xbindkey '(mod4 space) "ruby ~/bin/dlaunch.rb")
(xbindkey '(mod4 x) "xournal")
(xbindkey '(mod4 t) "fetchmail")
(xbindkey '(mod4 grave) "gnome-terminal -e \"screen -xRR\"")

;;;; Tablet-specific stuff

;; TODO:
;; XF86RotateWindows - ~/bin/rotate
;; XF86Launch1 - cellwriter

;; Chording keys test: Start differents program if only one key is
;; pressed or another if two keys are pressed.
;; If key1 is pressed start cmd-k1
;; If key2 is pressed start cmd-k2
;; If both are pressed start cmd-k1-k2 or cmd-k2-k1 following the
;;   release order

(define (define-chord-keys key1 key2 cmd-k1 cmd-k2 cmd-k1-k2 cmd-k2-k1)
    "Define chording keys"
  (let ((k1 #f) (k2 #f))
    (xbindkey-function key1 (lambda () (set! k1 #t)))
    (xbindkey-function key2 (lambda () (set! k2 #t)))
    (xbindkey-function (cons 'release key1)
		       (lambda ()
			 (if (and k1 k2)
			     (run-command cmd-k1-k2)
			     (if k1 (run-command cmd-k1)))
			 (set! k1 #f) (set! k2 #f)))
    (xbindkey-function (cons 'release key2)
		       (lambda ()
			 (if (and k1 k2)
			     (run-command cmd-k2-k1)
			     (if k2 (run-command cmd-k2)))
			 (set! k1 #f) (set! k2 #f)))))


;; Example:
;;   Shift + b:1                   start an xterm
;;   Shift + b:3                   start an rxvt
;;   Shift + b:1 then Shift + b:3  start gv
;;   Shift + b:3 then Shift + b:1  start xpdf

;; (define-chord-keys '(shift "b:1") '(shift "b:3")
;;   "xterm" "rxvt" "gv" "xpdf")

;; Here the release order have no importance
;; (the same program is started in both case)
;; (define-chord-keys '(alt "b:1") '(alt "b:3")
;;   "gv" "xpdf" "xterm" "xterm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of xbindkeys guile configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

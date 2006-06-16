
;;;; elisp for managing the wmii-3 window manager

;; by Phil Hagelberg

;;; Installation

;; Launch wmii your normal way through .xinitrc, .Xsession, or
;; whatever.  Then set your wmiirc to just do "emacs -l
;; ~/.emacs.d/wmii.el" and get rid of the stuff it has in there by
;; default.

;;; Useful defuns

(defun xwrite (where what)
  (shell-command (concat "echo -n " what " | wmiir write " where)))

;;; Init

(defvar mod-key "Mod4" ; super key
  "Modifier key to use for wmii commands")

(xwrite "/def/grabmod" mod-key)

(mapc (lambda (command) (xwrite (car command) (cadr command)))
      '(("/def/border" "2")
	("/def/font" "fixed")
	("/def/selcolors" "#ffffff #285577 #4c7899")
	("/def/normcolors" "#222222 #eeeeee #666666")
	("/def/colmode" "default")
	("/def/colwidth" "0")))

;; Tagging rules

(xwrite "/def/rules" 
"/XMMS.*/ -> ~
/Gimp.*/ -> ~
/MPlayer.*/ -> ~
/.*/ -> !
/.*/ -> 1")

;;; Keyboard shortcuts
(setq wmii-map
;; Window selection
  '(("b" . ("/view/ctl" "select prev"))
    ("f" . ("/view/ctl" "select next"))
    ("n" . ("/view/sel/ctl" "select prev"))
    ("p" . ("/view/sel/ctl" "select next"))

;; Managed vs unmanaged
    ("space" . ("/view/ctl" "select toggle"))
    ("Shift-space" . ("/view/sel/sel/ctl" "sendto toggle"))

;; Moving Windows
    ("Shift-f" . ("/view/sel/sel/ctl" "sendto next"))
    ("Shift-b" . ("/view/sel/sel/ctl" "sendto prev"))

;; misc
    (":" . (shell-command "PATH=$HOME/.wmii-3:/usr/local/etc/wmii-3:$PATH `proglist /usr/local/etc/wmii-3 $HOME/.wmii-3 | wmiimenu` &;;"))
    ("!" . (shell-command "wmiisetsid `wmiimenu <$PROGS_FILE` &;;")) ; programs
;    ("`" . (shell-command "~/bin/myrxvt")) ; shell
    ("k" . ("/view/sel/sel/ctl" "kill"))

;; TODO
; Modes (default, stack, max)
; tag window
; show tags
; switch to tag
))

;; tell wmii to watch for these keys
(shell-command (concat "wmiir write /def/keys <<EOF\n" 
		       (mapconcat (lambda (key-def) (concat mod-key "-" (car key-def)))
				  wmii-map "\n")
		       "\nEOF"))

(setq process-connection-type nil) ; use a pipe

(defun wmii-filter (proc string)
  (setf string (subseq string 0 -1))
  (let* ((event (car (split-string string " ")))
	(command (assoc (cadr (split-string (cadr (split-string string " ")) "-")) wmii-map)))
    (if (equal event "Key")
	(progn
	  (if (stringp (cadr command))
;		(message (concat (cadr command) " " (caddr command)))
		(xwrite (cadr command) (caddr command))
	    (message (cadr command)))
))))
;; handle barclicks!

(set-process-filter 
 (start-process "wmii" "*wmii*" "wmiir" "read" "/event") 
 'wmii-filter)


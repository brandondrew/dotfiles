
;;;; elisp for managing the wmii-3 window manager

;; by Phil Hagelberg

;;; Installation

;; Launch wmii your normal way through .xinitrc, .Xsession, or
;; whatever.  Then set your wmiirc to just do "emacs -l
;; ~/.emacs.d/wmii.el" and get rid of the stuff it has in there by
;; default.

;;; Useful defun

(defun xwrite (where what)
  (shell-command (concat "echo -n " what " | wmiir write " where)))

;;; Init

(defvar mod-key "Control-t,"
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
    ("p" . ("/view/sel/ctl" "select prev"))
    ("n" . ("/view/sel/ctl" "select next"))

   ;; Managed vs unmanaged
    ("space" . ("/view/ctl" "select toggle"))
    ("Shift-space" . ("/view/sel/sel/ctl" "sendto toggle"))

   ;; Moving Windows
    ("e" . ("/view/sel/sel/ctl" "sendto next"))
    ("a" . ("/view/sel/sel/ctl" "sendto prev"))

   ;; Modes
    ("s" . (xwrite "/view/sel/mode" "stack"))
    ("d" . (xwrite "/view/sel/mode" "default"))
    ("m" . (xwrite "/view/sel/mode" "max"))

   ;; misc
    (":" . (shell-command "PATH=$HOME/.wmii-3:/usr/local/etc/wmii-3:$PATH `proglist /usr/local/etc/wmii-3 $HOME/.wmii-3 | wmiimenu` &;;"))
    ("!" . (shell-command "wmiisetsid `wmiimenu <$PROGS_FILE` &;;")) ; programs
;    ("\`" . (shell-command "~/bin/myrxvt")) ; shell
    ("k" . ("/view/sel/sel/ctl" "kill"))

;; TODO
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
  (setf string (subseq string 0 -1)) ; get rid of newline
  (let ((event (car (split-string string " "))) ; "Key" or "BarClick"
	(arg (cadr (split-string string " "))))
    (cond ((equal event "Key")
	   (let* ((key (cadr (split-string arg ",-"))) ; must change this to allow for non C-t prefixed things
		  (command (cdr (assoc key wmii-map))))
	     (if (stringp (car command))
;		 (message (concat (cadr command) " " (caddr command)))
		 (xwrite (car command) (cadr command))
	       (message (cdr command)))))
	  ((equal event "BarClick")
	   (xwrite "/ctl" (concat "view " command))))))

(set-process-filter 
 (start-process "wmii" "*wmii*" "wmiir" "read" "/event") 
 'wmii-filter)

;; send current window to left column
(xwrite "/view/sel/sel/ctl" "sendto prev")


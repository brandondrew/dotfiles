
;;;; elisp for managing the wmii-3 window manager

;; by Phil Hagelberg

;;; Installation

;; Launch wmii your normal way through .xinitrc, .Xsession, or
;; whatever.  Then set your wmiirc to just do "emacs -l
;; ~/.emacs.d/wmii.el" and get rid of the stuff it has in there by
;; default.

;;; Useful defun

(defun xwrite (where what)
  (shell-command (concat "echo -n " what " | wmiir write " where) "*wmii*"))

;;; Init

(defvar mod-key "Control-t,"
  "Modifier key to use for wmii commands")

(xwrite "/def/grabmod" mod-key)

;;; Keyboard shortcuts
(setq wmii-map
      ;; Stuff gets evaled by default, unless it's a string, in which case it's sent to xwrite

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

   ;; Resizing
    ("minus" . ("/view/sel/sel/geom" "+0 +0 +0 -48"))
    ("equal" . ("/view/sel/sel/geom" "+0 +0 +0 +48"))
    ("comma" . ("/view/sel/sel/geom" "+0 +0 -48 +0"))
    ("period" . ("/view/sel/sel/geom" "+0 +0 +48 +0"))

   ;; Modes
    ("s" . (xwrite "/view/sel/mode" "stack"))
    ("d" . (xwrite "/view/sel/mode" "default"))
    ("f" . (xwrite "/view/sel/mode" "max"))

   ;; MPD
    ("Next" . (shell-command "mpc next"))
    ("Prior" . (shell-command "mpc prev"))
    ("Home" . (shell-command "mpc seek -5"))
    ("End" . (shell-command "mpc seek +5"))
    ("Insert" . (shell-command "mpc toggle"))

   ;; misc
    ; do we care about actions? (not really)
;    ("shift-colon" . (shell-command "PATH=$HOME/.wmii-3:/usr/local/etc/wmii-3:$PATH `proglist /usr/local/etc/wmii-3 $HOME/.wmii-3 | wmiimenu` &;;"))

    ("Return" . (shell-command "wmiisetsid `wmiimenu < $PROGS_FILE` &")) ; programs
    ("grave" . (shell-command "wmiisetsid myrxvt"))
    ("k" . ("/view/sel/sel/ctl" "kill"))
    ("m" . (shell-command "wmiiwarp 100 1020"))

   ;; google search
    ("Shift-g" . (shell-command "epiphany --new-tab http://google.com/search?q=`echo -n \"\" | wmiimenu`"))
   ;; google define
    ("Shift-d" . (shell-command "epiphany --new-tab http://google.com/search?q=define:`echo -n \"\" | wmiimenu`"))
   ;; wikipedia search
    ("Shift-w" . (shell-command "epiphany --new-tab http://en.wikipedia.org/wiki/Special:Search?search=`echo -n \"\" | wmiimenu`"))

   ;; tagging
    ("t" . (shell-command "wmiir read /tags | wmiimenu | wmiir write /view/sel/sel/tags"))
    ("1" . (xwrite "/ctl" "view 1"))
    ("2" . (xwrite "/ctl" "view 2"))
    ("3" . (xwrite "/ctl" "view 3"))
    ("Shift-1" . (xwrite "/view/sel/sel/tags" "1"))
    ("Shift-2" . (xwrite "/view/sel/sel/tags" "2"))
    ("Shift-3" . (xwrite "/view/sel/sel/tags" "3"))


))

;; tell wmii to watch for these keys
(shell-command (concat "wmiir write /def/keys <<EOF\n" 
		       (mapconcat (lambda (key-def) (concat mod-key "-" (car key-def)))
				  wmii-map "\n")
		       "\nEOF"))

(setq process-connection-type nil) ; use a pipe


;; todo - refactor this so it groks keys not prefixed, if possible

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
	       (eval command))))
	  ((equal event "BarClick")
	   (xwrite "/ctl" (concat "view " command))))))

(set-process-filter 
 (start-process "wmii" "*wmii*" "wmiir" "read" "/event") 
 'wmii-filter)




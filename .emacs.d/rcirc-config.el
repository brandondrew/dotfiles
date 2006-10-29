(require 'rcirc)

;; Turn on spell checking.
(add-hook 'rcirc-mode-hook (lambda ()
			     (flyspell-mode 1)))

;; Keep input line at bottom.                                                                               
(add-hook 'rcirc-mode-hook
	  (lambda ()
	    (set (make-local-variable 'scroll-conservatively)
		 8192)))

(setq rcirc-default-nick "technomancy")
;(setq rcirc-fill-column 120)
(setq rcirc-unambiguous-complete t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; colors!

(defvar rcirc-colors
  (if (fboundp 'color-distance)
      (let ((min-distance (* 0.2 (color-distance "black" "white")))
	    (bg (face-background 'default))
	    (fg (face-foreground 'rcirc-my-nick))
	    candidates)
	(dolist (item color-name-rgb-alist)
	  (let ((color (car item)))
	    (when (and (not (color-gray-p color))
		       (> (color-distance color bg) min-distance)
		       (> (color-distance color fg) min-distance))
	      (setq candidates (cons color candidates)))))
	candidates)
    (delete (face-background 'default) (defined-colors)))
  "Colors to use for nicks in rcirc.
By default, all the non-grey colors that are very different from
the default background are candidates.  The minimum
color-distance is half the distance between black and red as
computed by `color-distance'.

To check out the list, evaluate (list-colors-display rcirc-colors).")

(defvar rcirc-color-mapping (make-hash-table :test 'equal)
  "Hash-map mapping nicks to color names.")

(eval-after-load 'rcirc
  '(defun rcirc-facify (string face)
     "Return a copy of STRING with FACE property added.
Also add colors to other nicks based on `rcirc-colors'."
     (when (eq face 'rcirc-other-nick)
       (let ((color (gethash string rcirc-color-mapping)))
	 (unless color
	   (setq color (elt rcirc-colors (random (length rcirc-colors))))
	   (puthash string color rcirc-color-mapping))
	 (setq face `((foreground-color . ,color)))))
     (if face
	 (propertize (or string "") 'face face 'rear-nonsticky t)
       string)))

(eval-after-load 'rcirc
  '(defun-rcirc-command color (args)
     "Change one of the nick colors."
     (interactive)
     (setq args (split-string args))
     (rcirc-do-color (car args) (cadr args) process target)))

(defun rcirc-do-color (nick color process target)
  "Implement /COLOR."
  (if (not nick)
      (let (names)
	(maphash (lambda (key value)
		   (add-text-properties
		    0 (length key)
		    `(face ((foreground-color . ,value)) help-echo ,value)
		    key)
		   (setq names (cons key names)))
		 rcirc-color-mapping)
	(rcirc-print process (rcirc-nick process) "NOTICE" target
		     (mapconcat	'identity names " ")))
    (unless color
      (error "Use what color?"))
    (puthash nick color rcirc-color-mapping)))


(defadvice rcirc-handler-NICK (before rcirc-handler-NICK-colors activate)
  "Update colors in `rcirc-color-mapping'."
  (let* ((old-nick (rcirc-user-nick sender))
	 (color (gethash old-nick rcirc-color-mapping))
         (new-nick (car args)))
    ;; don't delete the old mapping
    (puthash new-nick color rcirc-color-mapping)))

(add-hook 'rcirc-print-hooks 'my-rcirc-print-hook)
(defun my-rcirc-print-hook (a b c d e)
  (when (and
	 (string-match (rcirc-nick a) e)
	 (not (string-match (concat "<" (rcirc-nick a) ">") e))) ; but, ignore my own messages 
    (shell-command (concat "notify-send \"" (car (split-string b "!")) " said your nick\" \"" e "\""))))

(setq rcirc-unambiguous-complete t)

(setq rcirc-unambiguous-complete t)

(defun rcirc-complete-nick ()
  "Complete nick from list of nicks in channel."
  (interactive)
  (if (eq last-command this-command)
      (setq rcirc-nick-completions
            (append (cdr rcirc-nick-completions)
                    (list (car rcirc-nick-completions))))
    (setq rcirc-nick-completion-start-offset
          (- (save-excursion
               (if (re-search-backward " " rcirc-prompt-end-marker t)
                   (1+ (point))
                 rcirc-prompt-end-marker))
             rcirc-prompt-end-marker))
    (setq rcirc-nick-completions
          (let ((completion-ignore-case t))
            (all-completions
	     (buffer-substring
	      (+ rcirc-prompt-end-marker
		 rcirc-nick-completion-start-offset)
	      (point))
	     (mapcar (lambda (x) (cons x nil))
		     (rcirc-channel-nicks (rcirc-buffer-process)
					  rcirc-target))))))
  (if rcirc-unambiguous-complete
      (rcirc-unambiguous-complete-nick rcirc-nick-completions)
    (rcirc-cycle-complete-nick rcirc-nick-completions)))

(defun rcirc-cycle-complete-nick (completions)
  "Complete nick by cycling through possibilities."
  (let ((completion (car completions)))
    (when completion
      (rcirc-put-nick-channel (rcirc-buffer-process) completion rcirc-target)
      (rcirc-insert-completed-nick completion))))

(defun rcirc-unambiguous-complete-nick (completions)
  "Complete unambiguous portion of nick and prompt for more."
  (let ((unambiguous (car completions)))
    (mapc (lambda (nick)
	    (setq unambiguous (rcirc-get-unambiguous nick unambiguous)))
	  (cdr completions))
    (setq unamb unambiguous)
    (if (string= unambiguous (buffer-substring
			      (+ rcirc-prompt-end-marker
				 rcirc-nick-completion-start-offset)
			      (point)))
	(message (mapconcat 'identity completions " "))
      (if unambiguous
	  (rcirc-insert-completed-nick unambiguous (not (equal (length completions) 1)))))))

(defun rcirc-get-unambiguous (nick target)
  (if (> (length nick) (length target))
      (setq nick (substring nick 0 (length target))))
  (if (equalp nick (substring target 0 (length nick)))
      nick
    (rcirc-get-unambiguous (substring nick 0 (- (length nick) 1)) target)))

(defun rcirc-insert-completed-nick (nick &optional incomplete)
  (delete-region (+ rcirc-prompt-end-marker
		    rcirc-nick-completion-start-offset)
		 (point))
  (insert nick)
  (if (and (= (+ rcirc-prompt-end-marker
	    rcirc-nick-completion-start-offset)
	      rcirc-prompt-end-marker)
	   (not incomplete))
      (insert ": ")))

(require 'rcirc)

;; example:
;(setq rcirc-authinfo '(("freenode" nickserv "technomancy" "password")))

(setq rcirc-server-alist '(("irc.freenode.net" :channels ("#emacs" "#seattle.rb"))))
(setq rcirc-fill-column 72)
(add-hook 'rcirc-mode-hook (lambda () (rcirc-track-minor-mode 1)))

;; Turn on spell checking.
(add-hook 'rcirc-mode-hook (lambda ()
			     (flyspell-mode 1)))

;; Keep input line at bottom.                                                                               
(add-hook 'rcirc-mode-hook
	  (lambda ()
	    (set (make-local-variable 'scroll-conservatively)
		 8192)))

(setq rcirc-default-nick "technomancy")
(setq rcirc-startup-channels-alist '(("^irc.freenode.net$" "#conkeror" "#emacs" "#ruby-lang" "#helma" "#seattle.rb")))

(setq rcirc-buffer-maximum-lines 10240)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; colors!

;; (defvar rcirc-colors
;;   (if (fboundp 'color-distance)
;;       (let ((min-distance (* 0.23 (color-distance "black" "white")))
;; 	    (bg (face-background 'default))
;; 	    (fg (face-foreground 'rcirc-my-nick))
;; 	    candidates)
;; 	(dolist (item color-name-rgb-alist)
;; 	  (let ((color (car item)))
;; 	    (when (and (not (color-gray-p color))
;; 		       (> (color-distance color bg) min-distance)
;; 		       (> (color-distance color fg) min-distance))
;; 	      (setq candidates (cons color candidates)))))
;; 	candidates)
;;     (delete (face-background 'default) (defined-colors)))
;;   "Colors to use for nicks in rcirc.
;; By default, all the non-grey colors that are very different from
;; the default background are candidates.  The minimum
;; color-distance is half the distance between black and red as
;; computed by `color-distance'.

;; To check out the list, evaluate (list-colors-display rcirc-colors).")

;; (defvar rcirc-color-mapping (make-hash-table :test 'equal)
;;   "Hash-map mapping nicks to color names.")

;; (defadvice rcirc-facify (before rcirc-facify-colors activate)
;;   "Add colors to other nicks based on `rcirc-colors'."
;;   (when (and (eq face 'rcirc-other-nick)
;; 	     (not (string= string "")))
;;     (let ((color (gethash string rcirc-color-mapping)))
;;       (unless color
;; 	(setq color (elt rcirc-colors (random (length rcirc-colors))))
;; 	(puthash string color rcirc-color-mapping))
;;       (setq face `((foreground-color . ,color))))))

;; (defun rcirc-markup-nick-colors (process sender response channel-buffer)
;;   (let* ((target (with-current-buffer channel-buffer (or rcirc-target ""))))
;;     (with-syntax-table rcirc-nick-syntax-table
;;       (maphash (lambda (nick color)
;; 		 (let ((face (cons 'foreground-color color)))
;; 		   (goto-char (point-min))
;; 		   (while (re-search-forward
;; 			   (concat "\\b" (regexp-quote nick) "\\b") nil t)
;; 		     (rcirc-add-face (match-beginning 0) (match-end 0) face))))
;; 	     rcirc-color-mapping))))

;; (add-to-list 'rcirc-markup-text-functions 'rcirc-markup-nick-colors)

;; (eval-after-load 'rcirc
;;   '(defun-rcirc-command color (args)
;;      "Change one of the nick colors."
;;      (interactive)
;;      (setq args (split-string args))
;;      (rcirc-do-color (car args) (cadr args) process target)))

;; (defun rcirc-do-color (nick color process target)
;;   "Implement /COLOR."
;;   (if (not nick)
;;       (let (names)
;; 	(maphash (lambda (key value)
;; 		   (add-text-properties
;; 		    0 (length key)
;; 		    `(face ((foreground-color . ,value)) help-echo ,value)
;; 		    key)
;; 		   (setq names (cons key names)))
;; 		 rcirc-color-mapping)
;; 	(rcirc-print process (rcirc-nick process) "NOTICE" target
;; 		     (mapconcat	'identity names " ")))
;;     (unless color
;;       (error "Use what color?"))
;;     (puthash nick color rcirc-color-mapping)))

;; (defadvice rcirc-handler-NICK (before rcirc-handler-NICK-colors activate)
;;   "Update colors in `rcirc-color-mapping'."
;;   (let* ((old-nick (rcirc-user-nick sender))
;; 	 (color (gethash old-nick rcirc-color-mapping))
;;          (new-nick (car args)))
;;     ;; don't delete the old mapping
;;     (puthash new-nick color rcirc-color-mapping)))


;; (add-hook 'rcirc-print-hooks 'my-rcirc-print-hook)
;; (defun my-rcirc-print-hook (a b c d e)
;;   (when (and
;; 	 (string-match (rcirc-nick a) e)
;; 	 (not (string-match (concat "<" (rcirc-nick a) ">") e))) ; but, ignore my own messages 
;;     (shell-command (concat "notify-send \"" (car (split-string b "!")) " said
;;     your nick\" \"" e "\""))))

;; (load "rcirc-completion")

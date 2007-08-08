
;;; Part of my .emacs file

;; by Phil Hagelberg
;; Much thanks to emacswiki.org and RMS.

;; Note: this relies on files found in my dotfiles repository:
;; http://dev.technomancy.us/phil/browser/dotfiles/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defuns

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad "
          "minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

(defun display-image ()
  "display images using imagemagick"
  (interactive) 
  (shell-command (concat "display " 
			 (thing-at-point 'filename))))

(defun view-url (url) (interactive "MUrl: ")
                          (switch-to-buffer (url-retrieve-synchronously url))
                          (rename-buffer url t)
                         (eval ;set major mode
  (read
   (concat
    "("
    (completing-read "Major mode: "
                     (mapcar (lambda
                               (x)
                               (list (symbol-name x)))
                             (apropos-internal "-mode$"
                                               'commandp))
                     nil t) ")"))))

(defun flickr-grab ()
  "Display only the photo from a flickr url"
  (interactive)
  (w3m-browse-url
   (with-current-buffer (url-retrieve-synchronously (thing-at-point 'filename))
     (save-excursion
       (re-search-backward "src=\"\\(http://static\\.flickr\\.com/[[:digit:]]*/[[:digit:]]*\_[[:alnum:]]*\\.jpg\\)")
       (match-string 1)))))

(defun map-coords (lat lng)
  (interactive "BLatitude: \nBLongitude")
  (w3m-browse-url (concat "http://maps.yahoo.com/maps_result?mag=12&lat="
			  lat "&lon=" lng)))

(defun mark-string ()
  (interactive)
  (setq deactivate-mark nil)
  (push-mark (search-forward "\"") t t)
  (search-backward "\""))

(defun blog-edit ()
  (interactive)
  (require 'tramp)
  (find-file "/philisha.net:blog")
  (html-mode)
  (auto-fill-mode))

(defun fc-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun pmacro ()
  "insert the expansion of a macro"
  (interactive)
  (backward-kill-sexp)
  (undo)
  (insert (concat "\n" (pp (cl-macroexpand (read (current-kill 0)))))))

(defun jao-toggle-selective-display ()
  (interactive)
  (set-selective-display (if selective-display nil 3)))

(defun ido-imenu ()
  (interactive)
  (flet ((completing-read (&rest params)
			  (apply 'ido-completing-read params)))
    (imenu)))

(defun make-frame-on-host (host)
  (interactive (list (completing-read "Host: " '("192.168.1.47:0.0" "192.168.1.46:0.0"))))
  (make-frame-on-display host))

(defun line-count-lisp ()
  (interactive)
  (save-excursion
    (flush-lines "^$")
    (flush-lines "^;")
    (end-of-buffer)
    (let ((loc (line-number-at-pos)))
      (message (number-to-string loc) " lines of code"))))

(defmacro case-string (expr &rest choices)
  "A variation on the case macro that uses equal rather than eql, and is thus suitable for strings."
  `(cond ,@(mapcar (lambda (c)
		    `((equal ,expr ,(car c)) ,@(cdr c)))
		    choices)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; music

(defun random-music ()
  (interactive)
  (unless (boundp 'music-dirs)
    (setq music-dirs (split-string (shell-command-to-string "find /home/phil/music -type d | cut -c 18-") "\n")))
  (let ((dir (nth (random (length music-dirs)) music-dirs)))
    (shell-command (concat 
		    "mpc clear; "
		    "mpc add " (shell-quote-argument dir) " > /dev/null"))
    (message dir)))
    
(defun music-play-dir (dir)
  (interactive (list (completing-read "Play directory: " 
				      (split-string 
				       (shell-command-to-string "find /home/phil/music -type d | cut -c 18-") "\n"))))
  (shell-command (concat 
		  "mpc clear; "
		  "mpc add " (shell-quote-argument dir)
		  "; mpc play > /dev/null")))

(defun music-add-file (file)
  (interactive (list (completing-read "Add file: " 
				      (split-string 
				       (shell-command-to-string "find /home/phil/music -type f | cut -c 18-") "\n"))))
  (shell-command (shell-quote-argument (concat 
					"mpc add " file))))

(defun music-toggle () (interactive)
  (shell-command "mpc toggle"))

(defun music-next () (interactive)
  (shell-command "mpc next"))

(defun music-prev () (interactive)
  (shell-command "mpc prev"))
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     cosmetics

(defun smallish (&optional font-size)
  (interactive)
  (set-default-font (concat "terminus-" (or font-size "12")))
  (tabbar-mode -1)
  (scroll-bar-mode -1))

(defun pretty-lambdas ()
    (font-lock-add-keywords
     nil `(("(\\(lambda\\>\\)"
            (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                      ,(make-char 'greek-iso8859-7 107))
                      nil))))))

(defun window-small-and-large ()
  (interactive)
  (if (equal 1 (length (window-list)))
      (split-window))
  (set-window-text-height (first (window-list)) (- (frame-height) 20)))

(defun big-fonts ()
  (interactive)
;  (set-default-font "-b&h-lucidatypewriter-bold-r-normal-sans-34-190-100-100-m-159-iso8859-1")
  (set-default-font "-b&h-lucidatypewriter-bold-r-normal-sans-34-240-100-100-m-200-iso8859-1"))

(defun terminus () (interactive) (set-default-font "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-1"))

(defun eshell-handle-ansi-color ()
  (ansi-color-apply-on-region eshell-last-output-start
			      eshell-last-output-end))

(add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)

(defun eshell-maybe-bol ()
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
	(beginning-of-line))))

(add-hook 'eshell-mode-hook
	  '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-maybe-bol)))

(defun play-test-sounds (total fail-total)
  (if (= 0 fail-total)
      (shell-command "mpg321 ~/music/final.fantasy/ff9/1-19\\ Fanfare.mp3 &")))

(provide 'defuns)
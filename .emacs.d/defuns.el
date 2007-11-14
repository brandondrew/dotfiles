
;;; Part of my .emacs file

;; by Phil Hagelberg
;; Much thanks to emacswiki.org and RMS.

;; Note: this relies on files found in my dotfiles repository:
;; http://dev.technomancy.us/phil/browser/dotfiles/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defuns

;;; Network stuff

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

;;; Lispy stuff

(defun my-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun my-recompile-init ()
  (interactive)
  (byte-recompile-directory (expand-file-name "~/.emacs.d") 0))

(defun my-print-macro-expansion ()
  "insert the expansion of a macro"
  (interactive)
  (backward-kill-sexp)
  (undo)
  (insert (concat "\n" (pp (cl-macroexpand (read (current-kill 0)))))))

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

;;; Buffer/window stuff

(defun my-selective-display (column)
  "Rotate folding the buffer at no, 2, 4, and 6 columns."
  (interactive "P")
  (set-selective-display
   (if (< (or selective-display 0) 6)
       (or column (+ (or selective-display 0) 2))
     nil)))

(defun swap-buffers ()
  "Swap the current 2 buffers in their windows"
  (interactive)
  (if (one-window-p)
      (error "Frame doesn't have two windows")
    (let* ((cw (selected-window))
           (nw (next-window cw))
           (cb (buffer-name (window-buffer cw)))
           (nb (buffer-name (window-buffer nw))))
      (switch-to-buffer nb)
      (select-window nw)
      (switch-to-buffer cb))))

(defun current-window ()
  "Why get-buffer-window instead of buffer-window? Why isn't this defined already?"
  ;; TODO: send a patch?
  (get-buffer-window (current-buffer)))

(defun toggle-dedicated-window ()
  (interactive)
  (set-window-dedicated-p (current-window) (not (window-dedicated-p (current-window)))))

(defun window-small-and-large ()
  (interactive)
  (if (equal 1 (length (window-list)))
      (split-window))
  (set-window-text-height (first (window-list)) (- (frame-height) 20)))

;;; Cosmetic stuff

(defun smallish (&optional font-size)
  (interactive)
  (set-default-font
   (concat "-xos4-terminus-medium-r-normal--"  (or font-size "12") "-120-72-72-c-60-iso8859-1"))
  (tabbar-mode -1)
  (scroll-bar-mode -1))

(defun bigish ()                                                                                                                                                          
  (interactive)                                                                                                                                                              
  (set-default-font "-b&h-lucidatypewriter-bold-r-normal-sans-34-240-100-100-m-200-iso8859-1")) 

(defun pretty-lambdas ()
    (font-lock-add-keywords
     nil `(("(?\\(lambda\\>\\)"
            (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                      ,(make-char 'greek-iso8859-7 107))
                      nil))))))

(defun terminus () (interactive) (set-default-font "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-1"))

;;; Random stuff

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

(provide 'defuns)
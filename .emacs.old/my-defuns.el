
;;; Part of my .emacs project

;; by Phil Hagelberg
;; Much thanks to RMS and the folks at emacswiki.org.

;; Note: this relies on files found in my dotfiles repository:
;; http://github.com/technomancy/dotfiles

;;;; defuns

;;; Network stuff

(require 'thingatpt)

(defun view-url ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    ;; TODO: switch to nxml/nxhtml mode
    (cond ((search-forward "<?xml" nil t) (xml-mode))
          ((search-forward "<html" nil t) (html-mode)))))

(defun google-region (&optional flags)
  "Google the selected region."
  (interactive)
  (browse-url (concat "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
                      (buffer-substring (region-beginning) (region-end)))))

(defun map-coords (lat lng)
  "Show a Yahoo map marked with the point LAT by LNG."
  (interactive "BLatitude: \nBLongitude")
  (w3m-browse-url (concat "http://maps.yahoo.com/maps_result?mag=12&lat="
                          lat "&lon=" lng)))

(defun steersman-connect (password)
  "Make this Emacs instance become controlled by Steersman."
  (interactive "MPassword: ")
  (setq steersman-password password
        steersman-master "phil@hagelb.org"
        jabber-server "hagelb.org"
        jabber-username "agent"
        jabber-password password
        jabber-resource
        (concat "steersman-"
                (substring (shell-command-to-string "hostname") 0 -1)))
  (jabber-connect))

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

(defvar isearch-initial-string nil)

(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))

(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)))))

(defun ido-goto-symbol ()
  "Will update the imenu index and then use ido to select a symbol to navigate to"
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))
                              
                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))
                              
                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))
                             
                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

(defun current-window ()
  "Why isn't this defined already?"
  (get-buffer-window (current-buffer)))

(defun fullscreen ()
      (interactive)
      (set-frame-parameter nil 'fullscreen
                           (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(defun toggle-dedicated-window ()
  "Toggle the window-dedicated-p state of current window."
  (set-window-dedicated-p (current-window)
                          (not (window-dedicated-p (current-window)))))

(defun window-small-and-large ()
  (interactive)
  (if (equal 1 (length (window-list)))
      (split-window))
  (set-window-text-height (first (window-list)) (- (frame-height) 20)))

(defun my-coding-hook ()
  "Enable things I consider convenient across all coding buffers."
  ;; (indent-buffer)
  ;; (delete-trailing-whitespace)
  ;; (untabify-buffer)
  (if (and (> 22 emacs-major-version)
           (not (eq major-mode 'emacs-lisp-mode)))
      (whitespace-mode t))
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (make-local-variable 'column-number-mode)
  (column-number-mode t)
  (auto-fill-mode)
  (if window-system (hl-line-mode t))
  (idle-highlight))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

;;; Cosmetic stuff

(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun terminus () (interactive)
  (set-default-font
   "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-1"))

(defun terminus-large () (interactive)
  (set-default-font
   "-xos4-terminus-medium-r-normal--20-140-72-72-c-80-iso8859-1"))

(defun inconsolata (size)
  (interactive "p")
  (set-default-font
   (concat "-unknown-Inconsolata-normal-normal-normal-*-"
           (if (stringp size) size
             (if (= 1 size) "16"
               (read-from-minibuffer "Size: ")))
           "-*-*-*-m-0-*-*")))

(defun dvsm () (interactive) (set-default-font "-dejavu-dejavu sans mono-medium-r-normal--*-*-0-0--iso8859-1"))

(defun ansi-region () (interactive)
  (ansi-color-apply-on-region (min (mark) (point))
                              (max (mark) (point))))

(defadvice color-theme-zenburn (after elunit-flymake)
  (eval-after-load 'flymake
    '(progn
       (set-face-background 'flymake-errline "red4")
       (set-face-background 'flymake-warnline "dark slate blue")))

  (eval-after-load 'elunit
    '(progn
       (set-face-attribute 'elunit-pass-face nil :background "forest green")
       (set-face-attribute 'elunit-fail-face nil :background "dark red")
       (set-face-attribute 'elunit-error-face nil :background "DarkOrange4"))))

;;; Random stuff

(defadvice find-file-at-point (around goto-line compile activate)
  (let ((line (and (looking-at ".*:\\([0-9]+\\)")
                   (string-to-number (match-string 1)))))
    ad-do-it
    (and line (goto-line line))))

(defun my-recompile-init ()
  (interactive)
  (byte-recompile-directory (expand-file-name "~/.emacs.d") 0))

(defun my-generate-elisp-tags ()
  (interactive)
  (shell-command
   "find ~/.emacs.d ~/src/emacs -name \\*el | xargs etags -o ~/.emacs.d/TAGS"))

(defun my-generate-rails-tags ()
  (interactive)
  (shell-command
   (format "find %s | egrep \"rb$\" | xargs ctags-exuberant -a -e -f %s/TAGS --exclude=vendor --exclude=public --exclude=log --exclude=db"
                         (rails-root) (rails-root)))
  (visit-tags-table (concat (rails-root) "/TAGS")))

(defun my-generate-ruby-tags ()
  (interactive)
  (flet ((rails-root () (cadr (split-string (pwd) " "))))
    (my-generate-rails-tags)))

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if arg
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

(defun todo ()
  (interactive)
  (switch-to-buffer "*todo*")
  (emacs-lisp-mode)
  (insert-file-contents "~/.emacs.d/init.d")
  (goto-char (point-max))
  (search-backward ";;; TODO")
  (kill-region (point-min) (point)))

(defun display-image ()
  "display images using imagemagick"
  (interactive)
  (shell-command (concat "display "
                         (thing-at-point 'filename))))

(defun switch-or-start (function buffer)
  "If the buffer is current, bury it, otherwise invoke the function."
  (if (equal (buffer-name (current-buffer)) buffer)
      (bury-buffer)
    (if (get-buffer buffer)
        (switch-to-buffer buffer)
      (funcall function))))

(defun gd (&optional arg)
  "Help me retrain my fingers to use magit."
  (interactive)
  (magit-status "."))

(defun rot13-insertion (begin end length)
  "Probably doesn't work."
  (if (and
       (string-match "@irc\.freenode\.net" (buffer-name))
       (= (point-max) end))
      (rot13-region begin end)))

(defun texinfo-view-info ()
  (interactive)
  (shell-command (concat "makeinfo " (buffer-file-name)))
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "^@setfilename \\(.*\\)"))
  (info (concat (file-name-directory (buffer-file-name)) (match-string 1))))

; (add-hook 'after-change-functions 'rot13-insertion)

(defalias 'ss 'server-start)
(defalias 'pg 'sql-postgres)
(defalias 'pacakge 'package-list-packages)

(provide 'my-defuns)
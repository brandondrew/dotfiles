
;;; Part of my .emacs project

;; by Phil Hagelberg
;; Much thanks to RMS and the folks at emacswiki.org.

;; Note: this relies on files found in my dotfiles repository:
;; http://github.com/technomancy/dotfiles

;;; misc things

(when window-system
  (mouse-wheel-mode t)
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (blink-cursor-mode -1))

(when (not window-system)
  (keyboard-translate ?\C-h ?\C-?))

(setq visible-bell t
      font-lock-maximum-decoration t
      inhibit-startup-message t
      transient-mark-mode t
      indicate-empty-lines t
      color-theme-is-global t
      comment-auto-fill-only-comments t
      save-place t
      truncate-partial-width-windows nil
      uniquify-buffer-name-style 'forward
      indent-tabs-mode nil
      whitespace-chars '(trailing tabs lines)
      whitespace-line-column 80
      sql-server "localhost"
      save-place-file (convert-standard-filename "~/.emacs.d/places")
      browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "~/src/conkeror/conkeror")

(auto-compression-mode t)
(global-font-lock-mode t)
(menu-bar-mode -1)
(winner-mode t)
(recentf-mode 1)
(show-paren-mode 1)

(when (> emacs-major-version 21)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
	ido-use-filename-at-point t
        ido-max-prospects 10))

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'qrr 'query-replace-regexp)
(random t)

(setq emacs-wiki-name "PhilHagelberg"
      oddmuse-username "PhilHagelberg"
      oddmuse-wikis
      '(("Technomancy" "http://dev.technomancy.us" utf-8)
        ("EmacsWiki" "http://www.emacswiki.org/cgi-bin/emacs" utf-8))
      oddmuse-directory "~/.emacs.d/oddmuse")

;; Sorry, I *really* don't care.
(defvar twittering-spam-regex "\\(iphone\\|wwdc\\)"
  "Tweets that match this regex will not get displayed")

(defadvice twittering-format-status (after twittering-spam-block)
  "Drop tweets that match twittering-spam-regex"
  (if (string-match twittering-spam-regex ad-return-value)
      (setq ad-return-value "")
    ad-return-value))

(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")

;; don't clutter directories!
(setq backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups")))
      auto-save-default nil)

(add-to-list 'auto-mode-alist '("\\.ds$" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsd$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsl$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.texinfo" . texinfo-mode))

;; w3m
(setq w3m-use-cookies t
      w3m-default-save-directory "~/"
      w3m-default-display-inline-images nil)

(mapc (lambda (v) (set v nil))
      '(w3m-show-graphic-icons-in-header-line
        w3m-show-graphic-icons-in-mode-line
        w3m-track-mouse
        w3m-use-favicon
        w3m-use-toolbar))

;; nxhtml stuff
(setq mumamo-chunk-coloring 'submode-colored
      nxhtml-skip-welcome t
      indent-region-mode t
      rng-nxml-auto-validate-flag nil)

;;; Cosmetics

(defface my-red-face
  '((t (:background "red")))
  "A red face for warnings that are not quite that bad."
  :group 'my-faces)

(defface my-yellow-face
  '((t (:background "yellow")))
  "A yellow face for warnings that are not quite that bad."
  :group 'my-faces)

(eval-after-load 'flymake-mode
  '(progn
     (set-face-background 'flymake-errline "red4")
     (set-face-background 'flymake-warnline "dark slate blue")))

(eval-after-load 'diff-mode
  '(progn
     (set-face-attribute 'diff-added nil :foreground "green4")
     (set-face-attribute 'diff-removed nil :foreground "red4")))

(provide 'my-misc)
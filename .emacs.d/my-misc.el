
;;; Part of my .emacs file

;; by Phil Hagelberg
;; Much thanks to emacswiki.org and RMS.

;; Note: this relies on files found in my dotfiles repository:
;; http://git.caboo.se/?p=technomancy.git;a=summary

;;; misc things

(when window-system
  (mouse-wheel-mode t)
  (global-hl-line-mode t)
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (setq browse-url-browser-function 'browse-url-firefox)
  (setq browse-url-firefox-new-window-is-tab t)
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (blink-cursor-mode -1)
  (ignore-errors (set-default-font "terminus-16")))

(when (not window-system)
  (keyboard-translate ?\C-h ?\C-?))

(setq visible-bell t
      font-lock-maximum-decoration t
      inhibit-startup-message t
      transient-mark-mode t
      color-theme-is-global nil
      indicate-empty-lines t
      save-place t
      truncate-partial-width-windows nil
      indent-tabs-mode nil)

(auto-compression-mode t)
(global-font-lock-mode t)
(menu-bar-mode -1)
(winner-mode t)
(show-paren-mode 1)

(when (> emacs-major-version 21)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-max-prospects 10))

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'qrr 'query-replace-regexp)
(random t)

(setq emacs-wiki-name "PhilHagelberg"
      oddmuse-username "PhilHagelberg")

(setq oddmuse-wikis
      '(("Technomancy" "http://dev.technomancy.us" utf-8)
        ("EmacsWiki" "http://www.emacswiki.org/cgi-bin/emacs" utf-8)))

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

;; w3m
(setq w3m-use-cookies t
      w3m-default-save-directory "~/"
      w3m-default-display-inline-images t)

(mapc (lambda (v) (set v nil))
      '(w3m-show-graphic-icons-in-header-line
        w3m-show-graphic-icons-in-mode-line
        w3m-track-mouse
        w3m-use-favicon
        w3m-use-toolbar))

;;; Cosmetics

;; bug me
(setq yellow-tokens (delete ?\s "\\<\\(F IX\\|D OC\\|R ETIRE\\|T ODO\\|W ARN\\|F IXME\\).*\\>"))
(setq red-tokens (delete ?\s "\\<\\(H ACK\\|R EFACTOR\\).*\\>"))

(defface my-red-face
  '((t (:background "red")))
  "A red face for warnings that are not quite that bad."
 :group 'my-faces)

(defface my-yellow-face
  '((t (:background "yellow")))
  "A yellow face for warnings that are not quite that bad."
 :group 'my-faces)

(dolist ((mode '(ruby-mode lisp-mode emacs-lisp-mode)))
  (font-lock-add-keywords mode
                          (list (list yellow-tokens 0 'my-yellow-face 'prepend)
                                (list red-tokens    0 'my-red-face    'prepend))))

(provide 'my-misc)
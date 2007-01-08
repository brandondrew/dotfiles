
;;; Part of my .emacs file

;; by Phil Hagelberg
;; Much thanks to emacswiki.org and RMS.

;; Note: this relies on files found in my dotfiles repository:
;; http://dev.technomancy.us/phil/browser/dotfiles/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby help

;; syntax highlighting needs to be done before ruby-electric
(global-font-lock-mode t)

; for zenburn niceness:
(defface erb-face
  `((t (:background "grey18")))
  "Default inherited face for ERB tag body"
  :group 'rhtml-faces)

(defface erb-delim-face
  `((t (:background "grey15")))
  "Default inherited face for ERB tag delimeters"
  :group 'rhtml-faces)

(require 'ruby-electric)
(require 'rinari)
(require 'ri-ruby)

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode)) ; d'oh!

(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)

(defun my-ruby-mode-hook ()
  (ruby-electric-mode)
  (hs-minor-mode)
  (if (= emacs-major-version 22) (reveal-mode))
  (define-key ruby-mode-map "\C-\M-h" 'backward-kill-word) ; ruby-mode redefines this badly
  (local-set-key (kbd "RET") 'ruby-reindent-then-newline-and-indent))

(setq ri-ruby-script (expand-file-name "~/.emacs.d/ri-emacs.rb"))

(when (= emacs-major-version 22)
  (ido-mode t)
  (ido-toggle-prefix)
  (setq ido-enable-flex-matching t)
  (setq ido-create-new-buffer 'always)
  (file-name-shadow-mode t)
  (add-to-list 'hs-special-modes-alist
	       (list 'ruby-mode
		     (concat ruby-block-beg-re "\|{")
		     (concat ruby-block-end-re "\|}")
		     "#"
		     'ruby-forward-sexp nil)))

;; Thanks PragDave:

(defun ruby-xmp-region (reg-start reg-end)
  "Pipe the region through Ruby's xmp utility and replace
     the region with the result."
  (interactive "r")
  (shell-command-on-region reg-start reg-end
			   "ruby -r xmp -n -e 'xmp($_, \"%l\t\t# %r\n\")'" t))

(provide 'ruby)
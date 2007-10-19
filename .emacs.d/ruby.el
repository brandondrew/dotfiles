
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

(require 'ruby-mode)
(require 'ruby-electric)
(require 'inf-ruby)
;; This isn't in my repo; so don't whine when I do a fresh checkout and
;; haven't symlinked in the dev checkout.
(ignore-errors (require 'rails))
(require 'rhtml-mode)
(require 'ri-ruby)
(require 'test-unit)
(require 'rcodetools)
(require 'cheat)

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode)) ; d'oh!
(add-to-list 'auto-mode-alist '("\\.erb$" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.builder$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.mab$" . ruby-mode))

(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
 
(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)

(defun my-ruby-mode-hook ()
  (ruby-electric-mode)
  (pretty-lambdas))

(define-key ruby-mode-map "\C-\M-h" 'backward-kill-word) ; ruby-mode redefines this badly
(define-key ruby-mode-map (kbd "RET") 'ruby-reindent-then-newline-and-indent)
(define-key ruby-mode-map (kbd "C-c l") (lambda () (interactive) (insert "lambda")))
 
(setq ri-ruby-script (expand-file-name "~/.emacs.d/ri-emacs.rb"))

(when (> emacs-major-version 21)
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

(define-key ruby-mode-map
  "\C-c\C-t" 'toggle-buffer)

;; Thanks PragDave:

(defun ruby-xmp-region (reg-start reg-end)
  "Pipe the region through Ruby's xmp utility and replace
     the region with the result."
  (interactive "r")
  (shell-command-on-region reg-start reg-end
			   "ruby -r xmp -n -e 'xmp($_, \"%l\t\t# %r\n\")'" t))

;; Thanks Zenspider

(autoload 'autotest-switch "autotest" "doco" t)
(autoload 'autotest "autotest" "doco" t)
(add-hook 'ruby-mode-hook
          '(lambda ()
             (define-key ruby-mode-map (kbd "C-c C-a") 'autotest-switch)))

(provide 'ruby)
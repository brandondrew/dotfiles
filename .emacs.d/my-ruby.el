
;;; Part of my .emacs file

;; by Phil Hagelberg
;; Much thanks to emacswiki.org and RMS.

;; Note: this relies on files found in my dotfiles repository:
;; http://git.caboo.se/?p=technomancy.git;a=summary

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby help

;; syntax highlighting needs to be done before ruby-electric
(global-font-lock-mode t)

(require 'ruby-mode)
(require 'ruby-electric)
(require 'inf-ruby)
;; This isn't in my repo; so don't whine when I do a fresh checkout and
;; haven't symlinked in the dev checkout.
(ignore-errors (require 'rails))
(require 'ri-ruby)
(require 'rcodetools)
(require 'cheat)

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode)) ; d'oh!
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml$" . my-rhtml))
(add-to-list 'auto-mode-alist '("html\\.erb$" . my-rhtml))
(add-to-list 'auto-mode-alist '("\\.builder$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.mab$" . ruby-mode))

(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)
(add-hook 'ruby-mode-hook 'my-coding-hook)

(defun rr ()
  (interactive)
  (run-ruby "irb"))

(defun rails-root (&optional dir)
  (or dir (setq dir default-directory))
  (if (file-exists-p (concat dir "config/environment.rb"))
      dir
    (unless (equal dir "/")
      (rails-root (expand-file-name (concat dir "../"))))))

(defun rails-console ()
  (interactive)
  (run-ruby (concat (rails-root) "script/console")))

(setq inferior-ruby-first-prompt-pattern ">>"
      inferior-ruby-prompt-pattern ">>")

(defun my-ruby-mode-hook ()
  (ruby-electric-mode)
  (pretty-lambdas))

(define-key ruby-mode-map "\C-\M-h" 'backward-kill-word) ; ruby-mode redefines this badly
(define-key ruby-mode-map (kbd "RET") 'ruby-reindent-then-newline-and-indent)
(define-key ruby-mode-map (kbd "C-c l") (lambda () (interactive) (insert "lambda")))
(global-set-key (kbd "C-h r") 'ri)

(setq ri-ruby-script (expand-file-name "~/.emacs.d/ri-emacs.rb"))

(global-set-key "\C-c\C-t" 'toggle-buffer)

;; From http://pluskid.lifegoo.com/?p=59

;; only special background in submode
(setq mumamo-chunk-coloring 'submode-colored)
(setq nxhtml-skip-welcome t)

;; do not turn on rng-validate-mode automatically, I don't like
;; the anoying red underlines
(setq rng-nxml-auto-validate-flag nil)

;; force to load another css-mode, the css-mode in nxml package
;; seems failed to load under my Emacs 23

(defun my-rhtml ()
  (nxhtml-mode)
  (make-local-variable 'cua-inhibit-cua-keys)
  (setq mumamo-current-chunk-family '("eRuby nXhtml Family" nxhtml-mode
                                      (mumamo-chunk-eruby
                                       mumamo-chunk-inlined-style
                                       mumamo-chunk-inlined-script
                                       mumamo-chunk-style=
                                       mumamo-chunk-onjs=)))
  (mumamo-mode))

;; Flymake - http://www.emacswiki.org/cgi-bin/emacs-en/FlymakeRuby

(require 'flymake)

;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))

(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

;;(add-hook 'ruby-mode-hook
;;          '(lambda ()
;;           ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
;;           (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
;;               (flymake-mode))))

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

(provide 'my-ruby)
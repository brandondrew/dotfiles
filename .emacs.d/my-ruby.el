
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
(require 'ri-ruby)
(require 'rcodetools)

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode)) ; d'oh!
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.builder$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.mab$" . ruby-mode))

(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby1.9" . ruby-mode))

(add-to-list 'completion-ignored-extensions ".rbc")
 
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
(add-hook 'ruby-mode-hook 'my-coding-hook)

;;;###autoload
(defun rr ()
  (interactive)
  (run-ruby "irb"))

;;;###autoload
(defun rr1.9 ()
  (interactive)
  (run-ruby "irb1.9"))

;;;###autoload
(defun rbx ()
  (interactive)
  (run-ruby "~/src/rubinius/shotgun/rubinius"))

(defun rails-root (&optional dir)
  (or dir (setq dir default-directory))
  (if (file-exists-p (concat dir "config/environment.rb"))
      dir
    (unless (equal dir "/")
      (rails-root (expand-file-name (concat dir "../"))))))

;;;###autoload
(defun rails-console ()
  (interactive)
  (run-ruby (concat (rails-root) "script/console")))

;; TODO: autodetect this?
(setq inferior-ruby-first-prompt-pattern ">>"
      inferior-ruby-prompt-pattern ">>")

(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
          '(lambda () (inf-ruby-keys)))
             
;;;###autoload
(defun rake (task)
  (interactive (list (completing-read "Rake (default: default): "
				      (pcmpl-rake-tasks))))
  (shell-command-to-string (concat "rake " (if (= 0 (length task)) "default" task))))

(define-key ruby-mode-map "\C-\M-h" 'backward-kill-word) ; ruby-mode redefines this badly
(define-key ruby-mode-map (kbd "RET") 'ruby-reindent-then-newline-and-indent)
(define-key ruby-mode-map (kbd "C-c l") (lambda () (interactive) (insert "lambda")))

;; rcodetools stuff
(define-key ruby-mode-map (kbd "C-\\") 'rct-complete-symbol)

(global-set-key (kbd "C-h r") 'ri)

(setq ri-ruby-script (expand-file-name "~/.emacs.d/ri-emacs.rb"))

(global-set-key "\C-c\C-t" 'toggle-buffer)
(setq toggle-mapping-style 'ruby)

;; From http://pluskid.lifegoo.com/?p=59

;; only special background in submode
(setq mumamo-chunk-coloring 'submode-colored)
(setq nxhtml-skip-welcome t)

;; do not turn on rng-validate-mode automatically, I don't like
;; the anoying red underlines
(setq rng-nxml-auto-validate-flag nil)


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

;;; ;; Thanks Zenspider

(autoload 'autotest-switch "autotest" "doco" t)
(autoload 'autotest "autotest" "doco" t)
(add-hook 'ruby-mode-hook
          '(lambda ()
             (define-key ruby-mode-map (kbd "C-c C-a") 'autotest-switch)))

(defun rdoc-browse-gems (gem)
  (interactive "MGem: ")
  (if (equal (shell-command-to-string "ps awx | grep \"gem [s]erver\"")
	     "")
      (shell-command "gem server &"))
  (w3m-browse-url "http://localhost:8808")
  (ignore-errors
    (search-forward-regexp (concat "^" gem ".*\[rdoc\]"))))


(provide 'my-ruby)
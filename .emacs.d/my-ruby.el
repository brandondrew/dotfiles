
;;; Part of my .emacs project

;; by Phil Hagelberg
;; Much thanks to RMS and the folks at emacswiki.org.

;; Note: this relies on files found in my dotfiles repository:
;; http://github.com/technomancy/dotfiles

(require 'ruby-mode)
(require 'ruby-electric)
(require 'inf-ruby)
(require 'ri-ruby)
(require 'rcodetools)
(require 'pcmpl-rake)

(ignore-errors
  (add-to-list 'load-path "~/src/rinari")
  (require 'rinari))

(defvar ruby-test-program "ruby"
  "Program to use to run tests")

;;
;; Defuns
;;

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

;;;###autoload
(defun rjr ()
  (interactive)
  (run-ruby "jruby -S irb")
  (set (make-local-variable 'inferior-ruby-first-prompt-pattern)
       "^irb(.*)[0-9:]+0> *")
  (set (make-local-variable 'inferior-ruby-prompt-pattern)
       "^\\(irb(.*)[0-9:]+[>*\"'] *\\)+"))

;;;###autoload
(defun rake (task)
  (interactive (list (completing-read "Rake (default: default): "
                                      (pcmpl-rake-tasks))))
  (shell-command-to-string (concat "rake " (if (= 0 (length task)) "default" task))))

;;;###autoload
(defun rdoc-browse-gems (gem)
  (interactive "MGem: ")
  (if (equal (shell-command-to-string "ps awx | grep \"gem [s]erver\"")
             "")
      (shell-command "gem server &"))
  (w3m-browse-url "http://localhost:8808")
  (ignore-errors
    (search-forward-regexp (concat "^" gem ".*\[rdoc\]"))))

(defun ruby-test-one ()
  "Test the current ruby test (must be runable via ruby `buffer' --name `test')."
  (interactive)
  (let* ((funname (which-function))
         (fn (and (string-match "#\\(.*\\)" funname) (match-string 1 funname))))
    (compile (concat ruby-test-program " -I:../lib " buffer-file-name " --name " fn))))

(defun ruby-test-file ()
  (interactive)
  (if (string-match "_test.rb$" buffer-file-name)
      (compile (concat "ruby " buffer-file-name))
    (toggle-buffer)
    (compile (concat ruby-test-program " -I:../lib " buffer-file-name))
    (toggle-buffer)))

;; find-file-at-point help

(defun ruby-module-path (module)
    (shell-command-to-string
     (concat
      "ruby -e "
      "\"ret='()';$LOAD_PATH.each{|p| "
      "x=p+'/'+ARGV[0].gsub('.rb', '')+'.rb';"
      "ret=File.expand_path(x)"
      "if(File.exist?(x))};printf ret\" "
      module)))

(eval-after-load "ffap"
  '(push '(ruby-mode . ruby-module-path) ffap-alist))

;;
;; Bindings
;;

(define-key ruby-mode-map "\C-\M-h" 'backward-kill-word)
(define-key ruby-mode-map (kbd "RET") 'ruby-reindent-then-newline-and-indent)
(define-key ruby-mode-map (kbd "C-c l") (lambda ()
                                          (interactive) (insert "lambda")))
(define-key ruby-mode-map (kbd "C-\\") 'rct-complete-symbol)
(define-key ruby-mode-map (kbd "C-c M-t") 'ruby-test-file)
(define-key ruby-mode-map (kbd "C-c C-M-t") 'ruby-test-one)
(define-key ruby-mode-map (kbd "M-;") (lambda () (interactive)
                                        (comment-dwim nil)
                                        (indent-buffer)))

(global-set-key (kbd "C-h r") 'ri)

;;
;; Misc
;;

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode)) ; d'oh!
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.builder$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.mab$" . ruby-mode))

(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("jruby" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby1.9" . ruby-mode))

(add-to-list 'completion-ignored-extensions ".rbc")

(setq toggle-mapping-style 'ruby)
(setq inferior-ruby-first-prompt-pattern ">>"
      inferior-ruby-prompt-pattern ">>")

(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
          (lambda () (inf-ruby-keys)))
(add-hook 'inferior-ruby-mode-hook
          (lambda () (toggle-truncate-lines nil)
            (font-lock-mode -1)))

(add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode t)))
(add-hook 'ruby-mode-hook 'my-coding-hook)
(add-hook 'ruby-mode-hook 'pretty-lambdas)

(setq ri-ruby-script (expand-file-name "~/.emacs.d/ri-emacs.rb"))

(font-lock-add-keywords
 'ruby-mode
 '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
    1 font-lock-warning-face t)))

;;; Flymake

(require 'flymake)

;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))

(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3)
      flymake-err-line-patterns)

(add-hook 'ruby-mode-hook
          (lambda ()
            (when (and buffer-file-name (file-writable-p buffer-file-name))
              (local-set-key (kbd "C-c d")
                             'flymake-display-err-menu-for-current-line)
              (flymake-mode t))))

(provide 'my-ruby)
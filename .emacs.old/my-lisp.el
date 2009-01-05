
;;; Part of my .emacs project

;; by Phil Hagelberg
;; Much thanks to RMS and the folks at emacswiki.org.

;; Note: this relies on files found in my dotfiles repository:
;; http://github.com/technomancy/dotfiles

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp
;;

;; (add-to-list 'load-path "~/.emacs.d/slime-2.0/")
;; (setq inferior-lisp-program "/usr/bin/sbcl")
;; (require 'slime)

(add-hook 'emacs-lisp-mode-hook 'pretty-lambdas)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-mode-hook 'pretty-lambdas)
(add-hook 'scheme-mode-hook 'pretty-lambdas)
(add-hook 'emacs-lisp-mode-hook
	  ;; If you're saving an elisp file, likely the .elc is no longer valid.
	  (lambda ()
	    (make-local-variable 'after-save-hook)
	    (add-hook 'after-save-hook
		      (lambda ()
			(if (file-exists-p (concat buffer-file-name "c"))
			    (delete-file (concat buffer-file-name "c")))))))

(add-to-list 'auto-mode-alist '("\\.emacs-project" . emacs-lisp-mode))

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)
(define-key lisp-mode-shared-map (kbd "C-\\") 'lisp-complete-symbol)

(font-lock-add-keywords 'emacs-lisp-mode
			'(("(\\|)" . 'paren-face)))

(font-lock-add-keywords 'scheme-mode
			'(("(\\|)" . 'paren-face)))

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
    1 font-lock-warning-face t)))

(add-hook 'lisp-mode-hook 'my-coding-hook)
(add-hook 'emacs-lisp-mode-hook 'my-coding-hook)
(add-hook 'scheme-mode-hook 'my-coding-hook)

(defface paren-face
   '((((class color) (background dark))
      (:foreground "grey20"))
     (((class color) (background light))
      (:foreground "grey55")))
   "Face used to dim parentheses."
   :group 'my-faces)

(defun my-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

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
    (goto-char (point-max))
    (let ((loc (line-number-at-pos)))
      (message (number-to-string loc) " lines of code. Be sure to undo now."))))

(defmacro case-string (expr &rest choices)
  "A variation on the case macro that uses equal rather than eql, and is thus suitable for strings."
  `(cond ,@(mapcar (lambda (c)
                    `((equal ,expr ,(car c)) ,@(cdr c)))
                    choices)))

(require 'bus-scheme)
(provide 'my-lisp)
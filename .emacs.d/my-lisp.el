
;;; Part of my .emacs file

;; by Phil Hagelberg
;; Much thanks to emacswiki.org and RMS.

;; Note: this relies on files found in my dotfiles repository:
;; http://dev.technomancy.us/phil/browser/dotfiles/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp
;;

;; (add-to-list 'load-path "~/.emacs.d/slime-2.0/")
;; (setq inferior-lisp-program "/usr/bin/sbcl")
;; (require 'slime)

(add-hook 'emacs-lisp-mode-hook 'pretty-lambdas)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-mode-hook 'pretty-lambdas)

(defun my-lisp-hook ()
  (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)
  (font-lock-add-keywords nil
                          '(("(\\|)" . 'paren-face))))

(add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)
(add-hook 'lisp-mode-hook 'my-lisp-hook)
(add-hook 'lisp-mode-hook 'my-coding-hook)

(defface paren-face
   '((((class color) (background dark))
      (:foreground "grey20"))
     (((class color) (background light))
      (:foreground "grey55")))
   "Face used to dim parentheses.")

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
    (end-of-buffer)
    (let ((loc (line-number-at-pos)))
      (message (number-to-string loc) " lines of code"))))

(defmacro case-string (expr &rest choices)
  "A variation on the case macro that uses equal rather than eql, and is thus suitable for strings."
  `(cond ,@(mapcar (lambda (c)
                    `((equal ,expr ,(car c)) ,@(cdr c)))
                    choices)))

(provide 'my-lisp)
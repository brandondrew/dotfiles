
;;; Part of my .emacs project

;; by Phil Hagelberg
;; Much thanks to RMS and the folks at emacswiki.org.

;; Note: this relies on files found in my dotfiles repository:
;; http://git.caboo.se/?p=technomancy.git;a=summary

(require 'javascript)

(defun js-lambda () (interactive) (insert "function () {\n};")
  (backward-char 6))

(font-lock-add-keywords
 'javascript-mode `(("\\(function *\\)("
		     (0 (progn (compose-region (match-beginning 1) (match-end 1)
					       ,(make-char 'greek-iso8859-7 107))
			       nil)))))

(font-lock-add-keywords
 'ruby-mode
 '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
    1 font-lock-warning-face t)))

(define-key javascript-mode-map (kbd "C-c l") 'js-lambda)

(define-key javascript-mode-map "\C-\M-h" 'backward-kill-word)
(define-key javascript-mode-map (kbd "RET") 'newline-and-indent)
(add-hook 'javascript-mode-hook 'moz-minor-mode)
(add-hook 'javascript-mode-hook 'my-coding-hook)

(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . yaml-mode))

;; for helma
(add-to-list 'auto-mode-alist '("\\.skin$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.hac$" . javascript-mode))

(setq javascript-indent-level 2)

(require 'flymake-js)
;; getting an error 100% of the time now.
;;(add-hook 'javascript-mode-hook 'flymake-js-load)

(provide 'my-js)
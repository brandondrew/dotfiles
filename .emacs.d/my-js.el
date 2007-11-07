
;;; Part of my .emacs file

;; by Phil Hagelberg
;; Much thanks to emacswiki.org and RMS.

;; Note: this relies on files found in my dotfiles repository:
;; http://dev.technomancy.us/phil/browser/dotfiles/

(require 'javascript)

(defun js-lambda () (interactive) (insert "function () {\n};")
  (backward-char 6))

(defun js-pretty-lambdas ()
    (font-lock-add-keywords
     nil `(("\\(function *\\)("
            (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                      ,(make-char 'greek-iso8859-7 107))
                      nil))))))

(add-hook 'javascript-mode-hook 'js-pretty-lambdas)
(add-hook 'inferior-moz-mode-hook 'js-pretty-lambdas)
(define-key javascript-mode-map (kbd "C-c l") 'js-lambda)

(define-key javascript-mode-map "\C-\M-h" 'backward-kill-word)
(define-key javascript-mode-map (kbd "RET") 'newline-and-indent)
(add-hook 'javascript-mode-hook 'moz-minor-mode)

(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

;; for helma
(add-to-list 'auto-mode-alist '("\\.skin$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.hac$" . js-mode))

(require 'flymake-js)
(add-hook 'javascript-mode-hook 'flymake-js-load)

(provide 'my-js)
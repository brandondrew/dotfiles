;;; rhtml-mode

;; Part of arorem - Another Ruby on Rails Emacs Mode
;; Sets up an rhtml mode
;; (C) 2006 Phil Hagelberg

(add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))

(defconst rhtml-font-lock-keywords
  (append
   '(("<%[=]?" . font-lock-preprocessor-face)
     ("%>" . font-lock-preprocessor-face)
     ("link_to" . font-lock-keyword-face)
     ("<\\(/?[[:alnum:]][-_.:[:alnum:]]*\\)" 1 font-lock-function-name-face))
   ruby-font-lock-keywords))

(define-derived-mode rhtml-mode
  html-mode "RHTML"
  "Another Ruby on Rails Emacs Mode (RHTML)"
  (interactive)
  (abbrev-mode)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((rhtml-font-lock-keywords))))

(define-key rhtml-mode-map
  "\C-c\C-v" 'ruby-find-view)

(provide 'rhtml-mode)
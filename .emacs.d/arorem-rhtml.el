;;; arorem-rhtml

;; Part of arorem - Another Ruby on Rails Emacs Mode
;; Sets up an rhtml mode
;; (C) 2006 Phil Hagelberg

(add-to-list 'auto-mode-alist '("\\.rhtml$" . arorem-rhtml))

(define-derived-mode arorem-rhtml
  html-mode "RHTML"
  "Another Ruby on Rails Emacs Mode (RHTML)"
  (abbrev-mode))

(define-key arorem-rhtml-map
  "\C-x\C-v" 'arorem-switch-view)

;; TODO: add ruby keywords to syntax table!

(provide 'arorem-rhtml)
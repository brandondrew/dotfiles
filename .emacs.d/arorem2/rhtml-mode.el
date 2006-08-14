;;; rhtml-mode

;; Sets up an rhtml mode for embedded Ruby (ERB)
;; (C) 2006 Phil Hagelberg

(add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))

(defconst rhtml-font-lock-keywords
  (append
   '(
     ("<%[=]?" . font-lock-preprocessor-face)
     ("%>" . font-lock-preprocessor-face)
     ("<\\(/?[[:alnum:]][-_.:[:alnum:]]*\\)" 1 font-lock-function-name-face)
     ("\\([a-zA-Z0-9]*[ ]?\\)=" 1 font-lock-variable-name-face)
     ("\\(@[a-zA-Z0-9]*\\)" 1 font-lock-variable-name-face)
     ("<%[=]?\\([^%]*\\)%>" 1 font-lock-preprocessor-face)

     sgml-font-lock-keywords
     sgml-font-lock-keywords-1
     sgml-font-lock-keywords-2
     ruby-font-lock-keywords)))

(defface erb-face
  `((t (:background "lightblue")))
  "Face for Ruby embedded into HTML"
  :group 'basic-faces)

(define-derived-mode rhtml-mode
  html-mode "RHTML"
  "Embedded Ruby Mode (RHTML)"
  (interactive)
  (abbrev-mode)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(rhtml-font-lock-keywords)))

(defun rhtml-controller-name-from-view (view)
  (concat (rails-root) 
	  "app/controllers/"
	   (file-name-nondirectory 
	    (expand-file-name (concat view "/..")))
	  "_controller.rb"))

(defun rhtml-find-action ()
  (interactive)
  (let ((action (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
    (find-file (rhtml-controller-name-from-view (buffer-file-name)))
    (beginning-of-buffer)
    (search-forward (concat "def " action))
    (recenter)))

(defun extract-partial (begin end partial-name)
  (interactive "r\nsName your partial: ")
  (kill-region begin end)
  (find-file (concat "_" partial-name ".rhtml"))
  (yank)
  (pop-to-buffer nil)
  (insert (concat "<%= render :partial => '" partial-name "' %>\n")))


(define-key rhtml-mode-map
  "\C-c\C-v" 'rhtml-find-action)

(provide 'rhtml-mode)

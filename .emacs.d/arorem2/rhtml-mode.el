;;; rhtml-mode

;; Sets up an rhtml mode for embedded Ruby (ERB)
;; (C) 2006 Phil Hagelberg

(add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))

(defconst rhtml-font-lock-keywords
  '(
    ;; erb-specific
    ("<%[=]?" . font-lock-preprocessor-face)
    ("%>" . font-lock-preprocessor-face)

;    ("<%[=]?\\([^%]*\\)\\(['\"][^\"']*[\"']\\)\\([^%]*\\)%>"
;     . (2 'font-lock-string-face-erb t nil)) teh broken
    
    ("<%[=]?\\([^%]*\\)\\([A-Z][0-9a-zA-Z_]*\\)\\([^%]*\\)%>"
     2 'font-lock-type-face-erb)

    ("<%[=]?\\([^%]*\\)\\(@[0-9a-zA-Z_]*\\)\\([^%]*\\)%>"
     . (2 'font-lock-variable-name-face-erb t t))

    ("<%[=]?\\([^%]*\\)\\(:[0-9a-zA-Z_]*\\)\\([^%]*\\)%>"
     2 'font-lock-constant-face-erb)

    ("<%[=]?\\([^%]*\\)\\<\\(alias\\|and\\|begin\\|break\\|case\\|catch\\|class\\|def\\|do\\|elsif\\|else\\|fail\\|ensure\\|for\\|end\\|if\\|in\\|module\\|next\\|not\\|or\\|raise\\|redo\\|rescue\\|retry\\|return\\|then\\|throw\\|super\\|unless\\|undef\\|until\\|when\\|while\\|yield\\|render\\)\\>\\([^%]*\\)%>"
     2 'font-lock-keyword-face-erb)

    ("<%[=]?\\([^%]*\\)%>" . (1 'erb-face keep t))
    
    ;; html-specific
    ("<\\(/?[[:alnum:]][-_.:[:alnum:]]*\\)" 1 font-lock-function-name-face) ; tags
    ("\\([a-zA-Z0-9]*[ ]?\\)=" 1 font-lock-variable-name-face) ; attributes

    ("\\(\"[^\"]*\"\\)" .(1 font-lock-string-face prepend nil))
    ("\\('[^']*'\\)" . (1 font-lock-string-face prepend nil))
    ("\\(<!--.*?-->\\)" . (1 font-lock-comment-face t nil))
))


;; Set up ERB faces with proper background

(defcustom erb-background "grey18"
  "Background for embedded Ruby")

(defface erb-face
  `((t (:background "grey18")))
  "Basic face for Ruby embedded into HTML"
  :group 'basic-faces)

(mapc (lambda (faces)
	(copy-face (car faces) (cdr faces))
	(set-face-background (cdr faces) erb-background))
      '((font-lock-keyword-face . font-lock-keyword-face-erb)
	(font-lock-variable-name-face . font-lock-variable-name-face-erb)
	(font-lock-string-face . font-lock-string-face-erb)
	(font-lock-type-face . font-lock-type-face-erb)
	(font-lock-constant-face . font-lock-constant-face-erb)))


;; Handy RHTML functions

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


;; Defining the mode

(define-derived-mode rhtml-mode
  html-mode "RHTML"
  "Embedded Ruby Mode (RHTML)"
  (interactive)
  (abbrev-mode)
  (setq font-lock-defaults '(rhtml-font-lock-keywords t)))

(define-key rhtml-mode-map
  "\C-c\C-v" 'rhtml-find-action)

; for debugging font-lock
(global-set-key "\C-c\C-r" 'rhtml-mode)
(global-set-key "\C-x\C-\M-e" 'eval-defun)

(provide 'rhtml-mode)
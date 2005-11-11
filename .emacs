;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; My .emacs file
; by Phil Hagelberg
;
; Much thanks to emacswiki.org and RMS.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; some stuff I install through apt-get rather than
; manually: tramp, ruby-elisp, ecb, mmm-mode

; Load Path
(setq load-path (append (list 
			 "~/.emacs.d")
			 load-path))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     loading modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; PHP mode
(autoload 'php-mode "php-mode")
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

; .js (javascript) loads C mode (until I find something better)
(add-to-list 'auto-mode-alist '("\\.js$" . c-mode))

; .rhtml loads html
(add-to-list 'auto-mode-alist '("\\.rhtml$" . html-mode))

; html helper
(autoload 'html-helper-mode "html-helper-mode")
(add-to-list 'auto-mode-alist '("\\.html$" . html-helper-mode))

; CSS-mode
(autoload 'css-mode "css-mode")
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))

; Ruby help
(require 'ruby-electric)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-electric))

; integrated subversion
(require 'psvn)

; predictive abbreviation
(require 'pabbrev)

(require 'ido)
(ido-mode)
(setq ido-enable-tramp-completion nil)



; tabbar
(load "tabbar")
(tabbar-mode)

; all buffer tabs on main or misc groups
(defun tabbar-buffer-groups (buffer)
  "Return the list of group names BUFFER belongs to.
Return only one group for each buffer."
  (with-current-buffer (get-buffer buffer)
    (cond
     ((or (get-buffer-process (current-buffer))
          (memq major-mode
                '(comint-mode compilation-mode)))
      '("Misc"))
     ((member (buffer-name)
              '("*scratch*"))
      '("Misc"))
     ((member (buffer-name)
              '("*Completions*"))
      '("Misc"))
     ((member (buffer-name)
              '("*tramp output*"))
      '("Misc"))
     ((member (buffer-name)
              '("*Messages*"))
      '("Misc"))
     ((member (buffer-name)
              '("*Compile-Log*"))
      '("Misc"))
     ((eq major-mode 'dired-mode)
      '("Dired"))
     ((memq major-mode
            '(help-mode apropos-mode Info-mode Man-mode))
      '("Misc"))
     ((memq major-mode
            '(tex-mode latex-mode text-mode xml-mode php-mode ruby-mode term))
      '("Main"))
     (t
      '("Main"))
     )))

; mmm-mode
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
(setq mmm-submode-decoration-level 2)
(mmm-add-classes
 '((embedded-ruby
    :submode ruby-mode
    :front "<%[=#]?"
    :back "%>"
    :insert ((?r eruby-directive nil @ "<%" @ " " _ " " @ "%>" @)
             (?= eruby-directive nil @ "<%=" @ " " _ " " @ "%>" @)))))
(mmm-add-classes
 '((embedded-css
    :submode css-mode
    :face mmm-declaration-submode-face
    :front "style=\""
    :back "\"")))
(mmm-add-classes
 '((embedded-javascript
    :submode javascript-mode ;; javascript-generic-mode
    :face mmm-declaration-submode-face
    :front "<script\[^>\]*>"
    :back "</script>")))
(mmm-add-classes
 '((embedded-javascript-attribute
    :submode javascript-mode ;; javascript-generic-mode
    :face mmm-declaration-submode-face
    :front "\\bon\\w+=\\s-*\""
    :back "\"")))


;; What features should be turned on in this html-mode?
(add-to-list 'mmm-mode-ext-classes-alist
         '(html-mode nil embedded-css))
(add-to-list 'mmm-mode-ext-classes-alist
         '(html-mode nil embedded-ruby))
(add-to-list 'mmm-mode-ext-classes-alist
         '(html-mode nil embedded-javascript))
(add-to-list 'mmm-mode-ext-classes-alist
         '(html-mode nil embedded-javascript-attribute))

(global-set-key [f8] 'mmm-parse-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; help (so C-h is delete)
   (global-set-key "\C-x\C-h" 'help-command)
; delete
   (global-set-key "\C-h" 'backward-delete-char)
; go to line
   (global-set-key "\M-g" 'goto-line)
; C-x C-m is compile
  (global-set-key "\C-x\C-m" 'compile)
; tabbar switching
  (global-set-key [(control shift up)] 'tabbar-backward-group)
  (global-set-key [(control shift down)] 'tabbar-forward-group)
  (global-set-key [(control shift left)] 'tabbar-backward)
  (global-set-key [(control shift right)] 'tabbar-forward)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     registers
; to load, C-x r j <register-name>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; local .emacs
(set-register ?l '(file . "~/.emacs"))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     misc things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; quit yer yappin'
  (setq sgml-warn-about-undefined-entities nil)
; load .gz's automatically
  (auto-compression-mode 1)
; display images inline
  (auto-image-file-mode 1)
; syntax highlighting by default
  (global-font-lock-mode)

  (setq inhibit-startup-message t)
  (setq transient-mark-mode t)
  (setq show-paren-mode t)
; duh! this should be default.
  (mouse-wheel-mode 1)

; hide toolbar/menubar by default
(tool-bar-mode -1)
(menu-bar-mode -1)

; use a real clipboard!
;(setq x-select-enable-clipboard t)
; this disables copy-on-select--boo!

; window title
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

; don't autosave on tramp
     (add-to-list 'backup-directory-alist
                  (cons tramp-file-name-regexp nil))

; don't clutter directories!
(setq backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.baks"))))
(setq auto-save-directory (expand-file-name "~/.emacs.baks"))
(setq semanticdb-default-save-directory (expand-file-name "~/.emacs.baks"))




; cursor at the beginning of searches instead of the end!
    (add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
    (defun my-goto-match-beginning ()
      (when isearch-forward (goto-char isearch-other-end)))

; compile works w/o a Makefile!
 (require 'compile)
 (add-hook 'c-mode-hook
   (lambda ()
     (unless (file-exists-p "Makefile")
       (set (make-local-variable 'compile-command)
	    (let ((file (file-name-nondirectory buffer-file-name)))
	      (concat "gcc -O2 -Wall -o " (file-name-sans-extension file)
		      " " file))))))


(server-start)

(defun paxtel ()
  "Open a connection to paxtel via tramp"
  (interactive)
  (when t
    (find-file "/phil@paxtel.com:/var/www/app/app")
    (find-file "/phil@paxtel.com:/var/www/app/app/controllers")
    (find-file "/phil@paxtel.com:/var/www/app/app/models")
    (find-file "/phil@paxtel.com:/var/www/app/app/views")))


(defun www2 ()
  "Open a tunneled connection to www2 via jacob"
  (interactive)
  (find-file "/multi:ssh:rozinant@jacob.biola.edu:ssh:d1103784@www2.biola.edu:"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;    Nifty things to remember and hopefully use
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; M-z zap to char
; C-u C-SPC jump to previous edit
; M-/ autocomplete word 
; M-! insert output of shell command
; M-| replace region with shell output
; M-x thumbs
; C-r-k Rectangle kill

; Macros
; C-m C-r to begin
; name it, and do stuff
; C-s to save

; temp macros
; C-m C-m to start recording
; C-m C-s to stop
; C-m C-p to play


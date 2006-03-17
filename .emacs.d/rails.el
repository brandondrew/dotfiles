;;; rails.el --- minor mode for editing RubyOnRails code

;; Copyright (C) 2006 Galinsky Dmitry <dima dot exe at gmail dot com>

;;; Code:

(eval-when-compile
  (require 'speedbar)
  (require 'ruby-mode))

(require 'ansi-color)
(require 'snippet)
(require 'etags)
(require 'find-recursive)

(require 'rails-snippets)
(require 'rails-menu)
(require 'rails-switch)
(require 'rails-webrick)
(require 'rails-misc)

(defvar rails-version "0.3")
(defvar rails-ruby-command "ruby")
(defvar rails-webrick-buffer-name "*WEBrick*")
(defvar rails-webrick-port "3000")
(defvar rails-webrick-default-env "development")
(defvar rails-webrick-url (concat "http://localhost:" rails-webrick-port))
(defvar rails-templates-list '("rhtml" "rxml" "rjs"))
(defvar rails-chm-file nil "Path CHM file or nil")
(defvar rails-use-another-define-key nil )
(defvar rails-use-mongrel nil "Non nil using Mongrel, else WEBrick")

(defun try-complete-abbrev (old)
  (if (expand-abbrev) t nil))

(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
	try-complete-file-name
	try-expand-dabbrev))


(define-minor-mode rails-minor-mode
  "RubyOnRails"
  nil
  " RoR"
  (list
   (cons [menu-bar] rails-minor-mode-menu-bar-map)
   (cons "\C-t"  'rails-switch-view-action)
   (cons [f1]  'rails-search-doc))

  (abbrev-mode -1)
  (rails-abbrev-init)

  ;; Tags
  (make-local-variable 'tags-file-name)

  (setq tags-file-name (concat (rails-root) "TAGS")))

(add-hook 'ruby-mode-hook
          (lambda()
            (local-set-key (kbd "C-.") 'complete-tag)
	    (if (rails-root)
		(rails-minor-mode t))
            (if rails-use-another-define-key
                (progn
                  (local-set-key (kbd "TAB") 'ruby-indent-or-complete)
                  (local-set-key (kbd "RET") 'ruby-newline-and-indent))
              (progn
                (local-set-key (kbd "<tab>") 'ruby-indent-or-complete)
                (local-set-key (kbd "<return>") 'ruby-newline-and-indent)))))

(add-hook 'speedbar-mode-hook
          (lambda()
            (speedbar-add-supported-extension "\\.rb")))

(add-hook 'find-file-hooks
          (lambda()
            (if (and (string-match (rails-make-template-regex) buffer-file-name)
                     (rails-root))
                (progn
                  (add-hook 'local-write-file-hooks
                            '(lambda()
                               (save-excursion
                                 (untabify (point-min) (point-max))
                                 (delete-trailing-whitespace))))

                  (rails-minor-mode t)
                  (rails-erb-abbrev-init)
                  (if rails-use-another-define-key
                      (local-set-key "TAB"
                                     '(lambda() (interactive)
                                        (if snippet
                                            (snippet-next-field)
                                          (if (looking-at "\\>")
                                              (hippie-expand nil)
                                            (indent-for-tab-command)))))
                    (local-set-key (kbd "<tab>")
                                   '(lambda() (interactive)
                                      (if snippet
                                          (snippet-next-field)
                                        (if (looking-at "\\>")
                                            (hippie-expand nil)
                                          (indent-for-tab-command))))))))))

(provide 'rails)
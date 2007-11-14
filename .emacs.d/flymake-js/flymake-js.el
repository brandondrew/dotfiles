;; Flymake JS mode

(require 'flymake)

(defconst flymake-allowed-js-file-name-masks '(
                                                ("\\.json$" flymake-js-init)
                                                ("\\.js$" flymake-js-init))
  "Filename extensions that switch on flymake-js mode syntax checks")

(defconst flymake-js-err-line-pattern-re '(
					   ("^js: \"\\(.+\\)\", line \\([0-9]+\\): \\(.+\\)$" 1 2 nil 3)
					   ("^js: uncaught JavaScript \\(.+\\)$" nil nil nil 1)
)
  "Regexp matching JavaScript error messages")

(defun flymake-js-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace))
         (local-file (file-relative-name
		      temp-file
		      (file-name-directory buffer-file-name))))
    (list "java" (list "-jar" "/home/gabor/src/yuicompressor-1.0/lib/rhino-1.6R6.jar" "/home/gabor/dev/slink/js/rhino.js" local-file))))

(defun flymake-js-load ()
  (setq flymake-allowed-file-name-masks (append flymake-allowed-file-name-masks flymake-allowed-js-file-name-masks))
  (setq flymake-err-line-patterns (append flymake-err-line-patterns flymake-js-err-line-pattern-re))
  (flymake-mode t)
  (local-set-key (kbd "C-c d") 'flymake-display-err-menu-for-current-line))

(provide 'flymake-js)

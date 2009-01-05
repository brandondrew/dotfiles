(toggle-debug-on-error)
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/elpa/ert-0.1")
(require 'ruby-mode)
(require 'ert)

(deftest indentation
  (assert-correct-indentation "ruby_mode_sample.rb"))

(deftest keyword-font-lock
  (assert-font-lock font-lock-keyword-face
		    '("class" "def" "end")))

(deftest string-font-lock
  (assert-font-lock font-lock-string-face
		    '("double-quoted" "single-quoted"
		      ;; TODO: "q-style"
		      "heredoc")))

(deftest symbol-font-lock
  (assert-font-lock font-lock-constant-face
		    '("symbol")))

(deftest major-mode
  (assert-equal 'ruby-mode major-mode))
  
(elunit "ruby-mode-suite")

;; (global-set-key (kbd "C-c C-m") (lambda () (interactive) (shell-command "emacs -Q -l ruby-mode-test.el")))


(toggle-debug-on-error)
(add-to-list 'load-path "~/.emacs.d")
(require 'ruby-mode)
(require 'mode-unit)
(require 'elunit-augment)

(global-set-key (kbd "C-c v") 'eval-buffer)

(mode-unit-suite ruby-mode-suite default-suite
		 "ruby_mode_sample.rb" ruby-mode)

(deftest indentation ruby-mode-suite
  (assert-correct-indentation "ruby_mode_sample.rb"))

(deftest keyword-font-lock ruby-mode-suite
  (assert-font-lock font-lock-keyword-face
		    '("class" "def" "end")))

(deftest string-font-lock ruby-mode-suite
  (assert-font-lock font-lock-string-face
		    '("double-quoted" "single-quoted"
		      ;; TODO: "q-style"
		      "heredoc")))

(deftest symbol-font-lock ruby-mode-suite
  (assert-font-lock font-lock-constant-face
		    '("symbol")))

(deftest major-mode ruby-mode-suite
  (assert-equal 'ruby-mode major-mode))

;; (elunit "ruby-mode-suite")

;; (global-set-key (kbd "C-c C-m") (lambda () (interactive) (shell-command "emacs -Q ruby-mode-test.el")))

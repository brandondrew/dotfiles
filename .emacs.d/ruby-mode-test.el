(toggle-debug-on-error)
(add-to-list 'load-path "~/.emacs.d")
(require 'ruby-mode)
(require 'mode-unit)

(mode-unit-suite ruby-mode-suite default-suite
		 "ruby_mode_sample.rb" ruby-mode)

(deftest indentation ruby-mode-suite
  (assert-correct-indentation "ruby_mode_sample.rb"))

(elunit "ruby-mode-suite")
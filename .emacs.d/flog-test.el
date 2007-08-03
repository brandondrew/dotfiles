(require 'elunit)
(load "flog")

(make-local-variable 'after-save-hook)
(add-hook 'after-save-hook (lambda () (eval-buffer)))

(elunit-clear-suites)

(defvar flog-sample-file "flog-sample.rb")

(defsuite flog nil)

(deftest flog-color flog
  (assert-equal "green4" (flog-color 34))
  (assert-equal "green1" (flog-color 3))
  (assert-equal "orange2" (flog-color 55))
  (assert-equal "red1" (flog-color 127)))

(deftest flog-filter-line flog
  (save-window-excursion
    (find-file flog-sample-file)
    (with-current-buffer flog-sample-file
      (flog-clear)
      (setq flogging-current-buffer flog-sample-file)
      (flog-filter-line nil "Flogger#minorly_complex: (16)")
      (goto-char (point-min))
      (search-forward "minorly_complex" nil t)
      (assert-equal "green2"
		    (cdr (overlay-get (car (overlays-at (point))) 'face))))))

(elunit "flog")
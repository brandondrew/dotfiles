(require 'elunit)

(elunit-clear-suites)

;; Meta suite

(defsuite elunit-suite nil ; nil means it's not part of a larger suite. suites can be nested.
  ;; put something here if needed...
  ;; :setup-hook (lambda () )
  :teardown-hook (lambda () (message "done testing")))

(deftest passing-tests elunit-suite
  ;; optional docstring
  "passing tests should leave fail counter, open elunit buffer, and display results"
  (save-buffer-excursion ; execute body and return to the current buffer configuration
   (load "passing-sample-tests")
   (elunit "passing-tests")
   (assert (equal 0 elunit-fail-count))
   (assert (string= (buffer-name (current-buffer)) "*elunit*"))
   (assert (search-forward-regexp "0 failures"))))

(deftest failing-tests elunit-suite
  (save-buffer-excursion
   (load "failing-sample-tests")
   (elunit "failing-tests")
   (assert (equal 4 elunit-fail-count))
   (assert (string= (buffer-name (current-buffer)) "*elunit*"))
   (search-forward-regexp "(cl-assertion-failed (equal 5 (+ 2 2)))")
   (search-forward-regexp "4 failures")))
   

;; Sample suite

(defsuite arithmetic-suite nil ; nil means it's not part of a larger suite
  ;; put something here if needed...
  ;; :setup-hook (lambda () )
  :teardown-hook (lambda () (message "done testing")))

(deftest two-plus-two arithmetic-suite
  "Test that 2 + 2 = 4"
  (assert (equal 4 (+ 2 2))))

(deftest two-minus-two arithmetic-suite
  (assert (equal 0 (- 2 2))))

(deftest bad-math arithmetic-suite
  "This test should fail!"
  (assert (equal 5 (+ 1 2))))

(deftest two-minus-two arithmetic-suite
  "duplicate should overwrite original"
  (assert (equal 3 (length (elunit-suite 'my-test-suite)))))

(add-hook 'arithmetic-suite-setup-hook (lambda () (message "setup hook ran")))

(elunit "my-test-suite")


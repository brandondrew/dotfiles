(require 'cl)
(require 'elunit)

(elunit-clear-suites)

;; Meta suite

(defsuite meta-suite nil ; nil means it's not part of a larger suite. suites can be nested.
  ;; put something here if needed...
  :setup-hook (lambda () (message "started tests"))
  :teardown-hook (lambda () (message "done testing")))

(assert (elunit-get-suite 'meta-suite))
(assert (test-suite-setup-hook (elunit-get-suite 'meta-suite)))
(assert (test-suite-teardown-hook (elunit-get-suite 'meta-suite)))
(assert (equal 2 (length elunit-suites)))

(elunit-delete-suite 'meta-suite)
(assert (equal 1 (length elunit-suites)))

(defsuite meta-suite nil
  :teardown-hook (lambda () (message "done testing")))
;; Should replace existing suite
(defsuite meta-suite nil
  :teardown-hook (lambda () (message "done testing")))

;; should still be a single suite
(assert (equal 2 (length elunit-suites)))
;; make sure it's the right one; shouldn't have a setup-hook
(assert (not (test-suite-setup-hook (elunit-get-suite 'meta-suite))))

(deftest empty-test meta-suite
  "This is just here to increment the size of meta-suite's test list."
  (assert t))

(assert (equal 1 (length (test-suite-tests (elunit-get-suite 'meta-suite)))))

;; should delete test
(elunit-delete-test 'empty-test 'meta-suite)

(assert (equal 0 (length (test-suite-tests (elunit-get-suite 'meta-suite))
			 )))

(deftest empty-test meta-suite
  "This is just here to increment the size of meta-suite's test list."
  (assert t))
(deftest empty-test meta-suite
  "This is just here to increment the size of meta-suite's test list."
  (assert t))

;; should rewrite existing test of the same name
(assert (equal 1 (length (test-suite-tests (elunit-get-suite 'meta-suite)))))

;; should store file and line number in test
(assert (equal buffer-file-name
	       (test-file (elunit-get-test 'empty-test 'meta-suite))))

(assert (equal 47
	       (test-line (elunit-get-test 'empty-test 'meta-suite))))

;; should run a suite

(save-excursion
  (delete-other-windows)
  (elunit "meta-suite")
  (other-window 1)
  ;; Will signal an error if not found.
  (search-forward "meta-suite")
  (kill-buffer (current-buffer))
  (other-window 1))

;; should run a suite's tests plus a suite's children

(defsuite child-suite meta-suite)

(deftest child-test child-suite
  "put some crap in the elunit buffer"
  ;; must use princ to take advantage of with-output-to-temp-buffer
  (princ "some crap"))

(save-excursion
  (delete-other-windows)
  (elunit "meta-suite")
  (other-window 1)
  ;; Will signal an error if not found.
  (search-forward "some crap")
  (kill-buffer (current-buffer))
  (other-window 1))

(message "looks like it works. what do you want, a gold star?")

;; (deftest passing-tests meta-suite
;;   ;; optional docstring
;;   "passing tests should leave fail counter, open elunit buffer, and display results"
;;   (save-buffer-excursion ; execute body and return to the current buffer configuration
;;    (load "passing-sample-tests")
;;    (elunit "passing-tests")
;;    (assert (equal 0 elunit-fail-count))
;;    (assert (string= (buffer-name (current-buffer)) "*elunit*"))
;;    (assert (search-forward-regexp "0 failures"))))

;; (deftest failing-tests meta-suite
;;   (save-buffer-excursion
;;    (load "failing-sample-tests")
;;    (elunit "failing-tests")
;;    (assert (equal 4 elunit-fail-count))
;;    (assert (string= (buffer-name (current-buffer)) "*elunit*"))
;;    (search-forward-regexp "(cl-assertion-failed (equal 5 (+ 2 2)))")
;;    (search-forward-regexp "4 failures")))
   
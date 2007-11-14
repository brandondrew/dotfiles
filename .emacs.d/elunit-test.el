(load "elunit")

(add-hook (make-local-variable 'after-save-hook) (lambda () (elunit "meta-suite")))

(elunit-clear-suites)

;; Meta suite

(defsuite meta-suite nil)

(deftest assertions meta-suite
  "Let's make sure our assertions work."
  (assert-equal 2 (+ 1 1))
  (assert-not-equal 2 1)
  (assert-member 1 '(2 3 1))
  (assert-nil (not t))
  (assert-error (assert nil))
  (setq foo "foo")
  (assert-changed foo
		  (setq foo "bar"))
  (assert-not-changed foo
 		      (setq foo "bar")))

(deftest sample-suite-setup meta-suite
  "Ensure that defsuite creates new suites properly."
  (defsuite sample-suite nil ; nil for no parent suite
    :setup-hook (lambda () (message "setting up test"))
    :teardown-hook (lambda () (message "done with test")))
  
  (assert-that (elunit-get-suite 'sample-suite))
  (assert-that (test-suite-setup-hook (elunit-get-suite 'sample-suite)))
  (assert-that (test-suite-teardown-hook (elunit-get-suite 'sample-suite)))
  (assert-equal 4 (length elunit-suites))

  (elunit-delete-suite 'sample-suite)
  ;; should just have default suite now
  (assert-equal 3 (length elunit-suites)))

(deftest duplicate-suite meta-suite
  (defsuite sample-suite nil
    :setup-hook (lambda () (message "start testing")))
  ;; Should replace existing suite
  (assert-not-changed (length elunit-suites)
		      (defsuite sample-suite nil
			:teardown-hook (lambda () (message "done testing")))

		      ;; make sure it really got replaced.
		      (assert-nil (test-suite-setup-hook
				    (elunit-get-suite 'sample-suite)))))

(deftest duplicate-test meta-suite
  (defsuite sample-suite nil)
  (assert-changed (length (test-suite-tests (elunit-get-suite 'sample-suite)))
		  (deftest empty-test sample-suite
		    (assert-that t))))

(deftest deleting-and-redefining-tests meta-suite
  (defsuite sample-suite nil)
  (deftest empty-test sample-suite)
  ;; should not define twice
  (assert-not-changed (length (test-suite-tests (elunit-get-suite 'sample-suite)))
		      (deftest empty-test sample-suite))

    ;; should store file and line number in test
  (assert-equal buffer-file-name
		(test-file (elunit-get-test 'empty-test 'sample-suite)))

  (let ((test-count (length (test-suite-tests (elunit-get-suite 'sample-suite)))))
    ;; should delete test
    (elunit-delete-test 'empty-test 'sample-suite)
    (assert-equal (- test-count 1)
		  (length (test-suite-tests (elunit-get-suite 'sample-suite))))))


(defsuite sample-suite nil)

(deftest test-fail sample-suite
  (assert-that nil))

(deftest test-error sample-suite
  (assert nil))

(deftest test-success sample-suite
  (assert-that t))

(elunit-quiet "sample-suite")

;; have to write raw assertions as suites can't run inside suites.
;; probably should fix this, allowing multiple *elunit* buffers, but
;; doesn't seem worth it.

(save-window-excursion
  (delete-other-windows)
  (elunit "sample-suite")
  (other-window 1)
  ;; Will signal an error if not found.
  (search-forward "sample-suite")
  (search-forward "Error:")
  (search-forward "Failure:"))

;; should run a suite's tests plus a suite's children

(defsuite child-suite sample-suite)

(deftest child-test child-suite
   "put some crap in the elunit buffer"
   ;; must use princ to take advantage of with-output-to-temp-buffer
   (princ "some crap"))

(save-window-excursion
 (elunit "sample-suite")
 (assert-in-buffer "some crap" "*elunit*"))

;; report successes with dots

(save-window-excursion
  (elunit "meta-suite")
  (assert-in-buffer ".." "*elunit*"))

(elunit-quiet "meta-suite")
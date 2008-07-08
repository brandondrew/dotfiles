(load "elunit")

(add-hook (make-local-variable 'after-save-hook)
          (lambda () (elunit "meta-suite")))

(elunit-clear)

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
    :setup-hooks (lambda () (message "setting up test"))
    :teardown-hooks (lambda () (message "done with test")))

  (assert-that sample-suite)
  (assert-that (test-suite-setup-hooks sample-suite))
  (assert-that (test-suite-teardown-hooks sample-suite)))

(deftest duplicate-suite meta-suite
  (defsuite sample-suite nil
    :setup-hooks (lambda () (message "start testing")))
  ;; Should replace existing suite
  (assert-not-changed (length elunit-suites)
                      (defsuite sample-suite nil
                        :teardown-hooks (lambda () (message "done testing")))

                      ;; make sure it really got replaced.
                      (assert-nil (test-suite-setup-hooks
                                    sample-suite))))

(deftest duplicate-test meta-suite
  (defsuite sample-suite nil)
  (assert-changed (length (test-suite-tests sample-suite))
                  (deftest empty-test sample-suite
                    (assert-that t))))

(deftest deleting-and-redefining-tests meta-suite
  (defsuite sample-suite nil)
  (deftest empty-test sample-suite)
  ;; should not define twice
  (assert-not-changed (length (test-suite-tests sample-suite))
                      (deftest empty-test sample-suite))

  ;; should store file and line number in test
  (assert-equal buffer-file-name
                (test-file (elunit-get-test 'empty-test sample-suite)))

  (let ((test-count (length (test-suite-tests sample-suite))))
    ;; should delete test
    (elunit-delete-test 'empty-test sample-suite)
    (assert-equal (- test-count 1)
                  (length (test-suite-tests sample-suite)))))


(defsuite sample-suite nil)

(deftest test-fail sample-suite
  (assert-that nil))

(deftest test-error sample-suite
  (/ 3 0))

(deftest test-success sample-suite
  (assert-that t))

;; (elunit "sample-suite")

;; should run a suite's tests plus a suite's children

(defsuite child-suite sample-suite)

(deftest child-test child-suite
   "put some crap in the elunit buffer"
   ;; must use princ to take advantage of with-output-to-temp-buffer
   (assert-that nil))

;;; Moved to mode-unit.

;; (save-window-excursion
;;  (elunit "sample-suite")
;;  (assert-in-buffer "some crap" "*elunit*"))

;; ;; report successes with dots

;; (save-window-excursion
;;   (elunit "meta-suite")
;;   (assert-in-buffer ".." "*elunit*"))
;; (elunit-get-test (intern ms1)
;;               (intern ms2))
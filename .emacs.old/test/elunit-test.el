(load "elunit")

(add-hook (make-local-variable 'after-save-hook)
          (lambda () (eval-buffer) (elunit "meta-suite")))

(elunit-clear)

;; Meta suite

(define-elunit-suite meta-suite nil)

(define-elunit-test assertions meta-suite
  "Let's make sure our assertions work."
  (assert-equal 2 (+ 1 1))
  (assert-not-equal 2 1)
  (assert-member 1 '(2 3 1))
  (assert-nil t)
  (assert-error (assert nil))
  (setq foo "foo")
  (assert-changed foo
                  (setq foo "bar"))
  (assert-not-changed foo
                      (setq foo "bar")))

(define-elunit-test sample-suite-setup meta-suite
  "Ensure that define-elunit-suite creates new suites properly."
  (define-elunit-suite sample-suite nil ; nil for no parent suite
    :setup-hooks (lambda () (message "setting up test"))
    :teardown-hooks (lambda () (message "done with test")))

  (assert-that sample-suite)
  (assert-that (test-suite-setup-hooks sample-suite))
  (assert-that (test-suite-teardown-hooks sample-suite)))

(define-elunit-test duplicate-test meta-suite
  (define-elunit-suite sample-suite nil)
  (assert-changed (length (test-suite-tests sample-suite))
                  (define-elunit-test empty-test sample-suite
                    (assert-that t))))

(define-elunit-test deleting-and-redefining-tests meta-suite
  (define-elunit-suite sample-suite nil)
  (define-elunit-test empty-test sample-suite)
  ;; should not define twice
  (assert-not-changed (length (test-suite-tests sample-suite))
                      (define-elunit-test empty-test sample-suite))

  ;; should store file and line number in test
  (assert-equal buffer-file-name
                (test-file (elunit-get-test 'empty-test sample-suite)))

  (let ((test-count (length (test-suite-tests sample-suite))))
    ;; should delete test
    (elunit-delete-test 'empty-test sample-suite)
    (assert-equal (- test-count 1)
                  (length (test-suite-tests sample-suite)))))


(define-elunit-suite sample-suite nil)

(define-elunit-test test-fail sample-suite
  (assert-that nil))

(define-elunit-test test-error sample-suite
  (/ 3 0))

(define-elunit-test test-success sample-suite
  (assert-that t))

;; (elunit "sample-suite")

;; should run a suite's tests plus a suite's children

(define-elunit-suite child-suite sample-suite)

(define-elunit-test child-test child-suite
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

(defun my-trace (n)
  (backtrace-frame n))

(defun my-trr (n)
  (my-trace n))

(my-trr 6)
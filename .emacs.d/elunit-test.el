(elunit-clear-suites)

;; Sample tests

(defsuite my-test-suite
  (two-plus-two
    "Test that 2 + 2 = 4"
    (assert (equal 4 (+ 2 2))))

  (two-minus-two
    (assert (equal 0 (- 2 2))))

  (bad-math
    "This test should fail!"
    (assert (equal 5 (+ 1 2))))

  (two-minus-two
    "duplicate should overwrite original"
    (assert (equal 3 (length (elunit-suite 'my-test-suite))))))

(add-hook 'my-test-suite-teardown-hook (lambda () (message "teardown hook")))

(elunit "my-test-suite")


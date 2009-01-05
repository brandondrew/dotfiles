(require 'elunit)

(defsuite tmpl nil)

(deftest string-template tmpl
  (assert-equal "hello world" (tmpl "hello #{\"world\"}")))

(deftest var-template tmpl
  (assert-equal "hello world"
                (let ((world "world"))
                  (tmpl "hello #{world}"))))

(deftest expression-template tmpl
  (assert-equal "hello world"
                (let ((world "world"))
                  (tmpl "hello #{(concat \"wor\" \"ld\")}"))))

(deftest double-template tmpl
  (assert-equal "hello emacs 23 world"
                (let ((editor "emacs")
                      (version "23"))
                  (tmpl "hello #{editor} #{version} world"))))
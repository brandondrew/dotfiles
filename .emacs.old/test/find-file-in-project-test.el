(setq ffip-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path ffip-dir)

(load "ert")
(load "../find-file-in-project")

(ert-deftest ffip-test ()
  (save-excursion
    (let ((ffip-regexp ".+"))
      (find-file (concat ffip-dir "/fixtures/ffip/one"))
      (should (equal (file-name-directory (buffer-file-name))
                     (ffip-project-root)))
      (should (equal '(".emacs-project"
                       "a: three" "a: two" "b" "c" "one")
                     (sort (mapcar 'car (ffip-project-files)) 'string<))))))


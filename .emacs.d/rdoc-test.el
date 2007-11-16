(defsuite rdoc-tests
  (test-login
   (flet ((url-http-parse-headers () nil)) ; we DO NOT want to follow redirects in this case.
     (switch-to-buffer (rdoc-login))
     (kill-buffer (current-buffer))
     (assert (equal (url-http-parse-response) 302))))

  (test-file-list
   (let ((files (rdoc-get-file-list)))
     (assert (member "files/README" files))
     (assert (member "files/vendor/railties/MIT-LICENSE" files)))))

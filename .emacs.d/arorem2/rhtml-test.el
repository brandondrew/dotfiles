;;; rhtml-mode test suite

;; (C) 2006 Phil Hagelberg

(defun face-at-string (string)
  (beginning-of-buffer)
  (search-forward string)
  (get-text-property (- (point) 1) 'face)) ;' to close the string

;; Test material

;; <body id="st1"> <!-- html comment -->
;; <!-- html comment
;;  spanning lines -->
;; <%= link_to "somewhere", :action => "blah" -%>
;; <% do end rescue 'blah' @word Const @var %>
;; <%# a comment %>

(add-hook 'rhtml-test-setup-hook 'rhtml-setup-buffer)
(add-hook 'rhtml-test-teardown-hook 'emacs-lisp-mode)

(defun rhtml-setup-buffer ()
  (rhtml-mode)
  (sit-for 0)) ; fontification won't happen unless we wait

(defsuite rhtml-test
;;   Bah; how to test this without a real rails project?
;;   (rhtml-find-action-test
;;    "Make sure we can switch buffers right"
;;    (assert (equal (rhtml-controller-name-from-view)))
;;    (rhtml-find-action)
;;    (assert (equal ())))
  (rhtml-extract-partial-test ; TODO - clean up this test by using a real project
   (switch-to-buffer "*rhtml-test-partial*")
   (insert "blah blah blah PARTIAL")
   (set-mark 16)
   (forward-word)
   (extract-partial (point) (mark) "blah")
   (assert (equal (buffer-string) "blah blah blah <%= render :partial => 'blah' %>\n"))
   (switch-to-buffer "_blah.rhtml")
   (assert (equal (buffer-string) "PARTIAL"))
   (kill-buffer "*rhtml-test-partial*")
   (kill-buffer "_blah.rhtml"))

  (rhtml-font-lock-erb-test
   "test font locking"
   (assert (equal (face-at-string "<%") 'font-lock-preprocessor-face))
   (assert (equal (face-at-string "link_to") 'erb-face))
   (assert (equal (face-at-string "Const") 'font-lock-type-face-erb))
   (assert (equal (face-at-string "@var") 'font-lock-variable-name-face-erb))
;   (assert (equal (face-at-string "@word") 'font-lock-variable-name-face-erb)) ; second variable on a line
   (assert (equal (face-at-string ":action") 'font-lock-constant-face-erb))
   (assert (equal (face-at-string "rescue") 'font-lock-keyword-face-erb))
;   (assert (equal (face-at-string "end") 'font-lock-keyword-face-erb)) ; second keyword on a line
   (assert (equal (face-at-string "html comment") 'font-lock-comment-face))
   (assert (equal (face-at-string "a comment") 'font-lock-comment-face)) ; erb comments not enabled yet
   (assert (equal (face-at-string "body") 'font-lock-function-name-face))
   (assert (equal (face-at-string "id") 'font-lock-variable-name-face))
;   (assert (equal (face-at-string "spanning") 'font-lock-comment-face)) ; html comments spanning lines don't work
   (assert (equal (face-at-string "st1") '(font-lock-string-face)))
   (assert (equal (face-at-string "\"blah") '(font-lock-string-face erb-face)))))

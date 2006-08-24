
<body id="st1"> <!-- html comment -->
<!-- html comment
 spanning lines -->
<%= link_to "somewhere", :action => "blah" -%>
<% do end rescue 'blah' @word Const @var %>
<%# a comment %>

;;; rhtml-mode test suite

;; (C) 2006 Phil Hagelberg

(add-hook 'rhtml-test-setup-hook 'rhtml-setup-buffer)

(defun rhtml-setup-buffer ())
  (switch-to-buffer "rhtml-test-buffer"))
;  (rhtml-mode)
;  (mark-whole-buffer)
;  (kill-region (mark) (point))
  (insert "<body id=\"st1\"> <!-- html comment -->
<!-- html comment
 spanning lines -->
<%= link_to \"somewhere\", :action => \"blah\" -%>
<% do end rescue 'blah' @word Const @var %>
<%# a comment %>\n"))



(defun face-at-string (string)
  (beginning-of-buffer)
  (search-forward string)
  (get-text-property (- (point) 1) 'face))

(defsuite rhtml-test
  (rhtml-font-lock-erb
;   "test font locking"
   (assert (equal (face-at-string "<%") 'font-lock-preprocessor-face))
   (assert (equal (face-at-string "link_to") 'erb-face))
   (assert (equal (face-at-string "Const") 'font-lock-type-face-erb))
   (assert (equal (face-at-string "@var") 'font-lock-variable-name-face-erb))
   (assert (equal (face-at-string "@word") 'font-lock-variable-name-face-erb))
   (assert (equal (face-at-string ":action") 'font-lock-constant-face-erb))
   (assert (equal (face-at-string "rescue") 'font-lock-keyword-face-erb))
;   (assert (equal (face-at-string "end") 'font-lock-keyword-face-erb))
   (assert (equal (face-at-string "html comment") 'font-lock-comment-face))
;   (assert (equal (face-at-string "a comment") 'font-lock-comment-face))
   (assert (equal (face-at-string "body") 'font-lock-function-name-face))
   (assert (equal (face-at-string "id") 'font-lock-variable-name-face))
;   (assert (equal (face-at-string "spanning") 'font-lock-comment-face))
   (assert (equal (face-at-string "st1") '(font-lock-string-face)))
   (assert (equal (face-at-string "\"blah") '(font-lock-string-face erb-face)))))

(elunit-clear-suites)

(insert (pp bb))

(setq body (cons 'progn bb))

(dolist (assertion bb) (eval assertion))

(setq max-specpdl-size 1000)

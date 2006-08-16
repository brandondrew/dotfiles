;;; rhtml-mode test suite

;; (C) 2006 Phil Hagelberg

(defun setup-rhtml-test ()
  "You need to run this method, then run 'M-x regress
 rhtml-test-regress' on the buffer it creates."

  (interactive)
  (switch-to-buffer "rhtml-test-buffer")
  (insert "<body id=\"st1\"> <!-- html comment -->
<!-- html comment
 spanning lines -->
<%= link_to \"somewhere\", :action => \"blah\" -%>
<% do end rescue 'blah' @word Const @var %>
<%# a comment %>")
  (rhtml-mode))

(defun face-at-string (string)
  (beginning-of-buffer)
  (search-forward string)
  (get-text-property (- (point) 1) 'face))

(put 'rhtml-test-regress 'regression-suite t)
(setq rhtml-test-regress
      '("rhtml-test-regress"
	("Highlight the <% %> marks."
	 (face-at-string "<%") 'font-lock-preprocessor-face)
	("Highlight all ERB."
	 (face-at-string "link_to") 'erb-face)
	("Highlight Constants."
	 (face-at-string "Const") 'font-lock-type-face-erb)
	("Highlight @variables."
	 (face-at-string "@var") 'font-lock-variable-name-face-erb)
	("Highlight both @variables."
	 (face-at-string "@word") 'font-lock-variable-name-face-erb)
	("Highlight :symbols."
	 (face-at-string ":action") 'font-lock-constant-face-erb)
	("Highlight keywords."
	 (face-at-string "rescue") 'font-lock-keyword-face-erb)
	("Highlight multiple keywords."
	 (face-at-string "end") 'font-lock-keyword-face-erb)
	("Highlight ruby comments."
	 (face-at-string "a comment") 'font-lock-comment-face)
	("Highlight HTML tags."
	 (face-at-string "body") 'font-lock-function-name-face)
	("Highlight HTML attributes."
	 (face-at-string "id") 'font-lock-variable-name-face)
	("Highlight HTML comments."
	 (face-at-string "html comment") 'font-lock-comment-face)
	("Highlight multiline HTML comments."
	 (face-at-string "spanning") 'font-lock-comment-face)
	("Highlight HTML strings."
	 (face-at-string "st1") '(font-lock-string-face))
	("Highlight strings."
	 (face-at-string "\"blah") '(font-lock-string-face erb-face))
	))


;;; Part of behave.el --- Emacs Lisp Behaviour-Driven Development framework

;; Copyright (C) 2007 Phil Hagelberg

;; Meta-specifications for how behave.el should work

(context "A context with multiple specs"
	 (for meta)
	 (setup (lambda (c)
		  (let ((context (context-find "A context with multiple specs")))
		    (funcall c))))
	 (specify "should have multiple specs"
		  (expect (length (context-specs context)) equal 2))
	 (specify "should be tagged meta"
		  (expect (context-tags context) equal '(meta)))
	 (specify "should fail this spec"
		 (expect (execute-spec (last (context-specs context))) equal t)))

(context "The expect macro"
	 (for meta expect)
	 (setup (lambda (c)
		  (let ((context (context-find "The expect macro")))
		    (funcall c))))
	 (specify "should expand to assert actual"
		  (expect (cl-macroexpand '(expect (+ 2 2) equal 4)) equal 
			  (cl-macroexpand '(assert (equal (+ 2 2) 4)))))
	 (specify "should fail by asserting nil"
		  (assert nil)))

;(behave-clear-contexts)
;(insert (pp *behave-contexts*))

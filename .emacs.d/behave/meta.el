;;; Part of behave.el --- Emacs Lisp Behaviour-Driven Development framework

;; Copyright (C) 2007 Phil Hagelberg

;; Meta-specifications for how behave.el should work

(local-set-key (kbd "C-x y") 'behave-clear-contexts)

(context "context macro"
	 (tag meta)
	 (message "should execute context"))

(context "specify macro"
	 (tag meta)
	 (specify "should add specifications to context"
		  (assert (= (length (context-specs context)) 2)))
	 (specify "dummy spec" (message "ran the dummy spec!")))

(context "a context that sets up variables"
	 (tag meta)
	 (lexical-let ((var "variable"))
	   (specify "should be able to use variable in spec"
		    (assert var))))

(context "a multi-line spec"
	 (tag meta)
	 (specify "should execute all of the body"
		  (setq some-random-variable 22)
		  (assert (= 22 some-random-variable))))

(context "setting up a context twice"
	 (tag meta)
	 (specify "should not create a duplicate context"
		  (context "dupe")
		  (lexical-let ((context-count (length *behave-contexts*)))
		    (context "dupe")
		    (assert (= context-count (length *behave-contexts*))))))

(context "helper functions"
	 (tag meta)
	 (specify "should find this context"
		  (assert (context-find "specify macro")))
	 (specify "should find many contexts tagged meta"
		  (assert (> (length (context-find-by-tag 'meta)) 4))))

(context "failing"
	 (tag meta fail)
	 (specify "should cause failure"
		  (assert nil)))


;; Writing a spec for execute-context would cause an infinite loop.
;; So we have to test it by hand.
; (execute-context (context-find "helper functions"))


;; Should fail:
; (execute-context (context-find "failing"))

(context "The expect macro"
	 (tag meta expect)
	 (let ((context (context-find "The expect macro")))
	   (specify "should expand to assert actual"
		    (expect (cl-macroexpand '(expect (+ 2 2) equal 4)) equal 
			    (cl-macroexpand '(assert (equal (+ 2 2) 4)))))
	   (specify "should fail by asserting nil"
		    (assert nil))))



(context "A context with multiple specs"
	 (tag meta foo bar)
	 (specify "should have multiple specs"
		  (expect (length (context-specs context)) equal 2))
	 (specify "should be tagged meta"
		  (expect (context-tags context) equal '(meta)))
	 (specify "should fail this spec"
		  (expect (execute-spec (last (context-specs context))) equal t))))



;; (setf c (make-context))

;; (context-specs c)

;; (setf (context-specs c) (cons '(1 2 3) (context-specs c)))
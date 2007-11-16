;;; Part of behave.el --- Emacs Lisp Behaviour-Driven Development framework

;; Copyright (C) 2007 Phil Hagelberg

;; Meta-specifications for how behave.el should work

(require 'behave)

(local-set-key (kbd "C-x y") 'behave-clear-contexts)

(context "context macro"
	 (tag meta)
	 (message "should execute context"))

(context "specify macro"
	 (tag meta)
	 (specify "should add specifications to context"
		  (assert (= (length (context-specs context)) 2)))
	 (specify "should execute specification including message when run" (message "ran the message spec!")))

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
	 (specify "should find this context with context-find"
		  (assert (context-find "helper functions")))
	 (specify "should find many contexts tagged meta with context-find-by-tag"
		  (assert (> (length (context-find-by-tag 'meta)) 4))))

(context "failing"
	 (tag fail)
	 (specify "should cause failure"
		  (assert nil)))

(context "failing context"
	 (tag meta)
	 (specify "should signal failure"
		  (assert (condition-case err
			      (mapcar #'execute-spec (context-specs (context-find "failing")))
			    (error t)))))

(context "The expect macro"
	 (tag meta expect)
	 (specify "should fail by expecting nil"
		    (expect nil))
	 (specify "should expect 1 to equal 1"
		  (expect 1 equal 1)))


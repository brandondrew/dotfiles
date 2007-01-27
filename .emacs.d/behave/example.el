;;; Part of behave.el --- Emacs Lisp Behaviour-Driven Development framework

;; Copyright (C) 2007 Phil Hagelberg

; Example: let's copy http://rspec.rubyforge.org/examples.html

(require 'behave)

(context "A stack which is neither empty nor full"
	 (for stack)

	 (lexical-let ((stack ()))
	   (mapc (lambda (x) (stack-push 'stack x)) (list "a" "b" "c"))

	   (specify "should add to the top when pushed"
		    (stack-push 'stack "d")
		    (expect (stack-peek 'stack) equal "d"))

	   (specify "should return the top item when peeked"
		    (expect (stack-peek 'stack) equal "c"))

	   (specify "should NOT remove the top item when peeked"
		    (expect (stack-peek 'stack) equal "c")
		    (expect (stack-peek 'stack) equal "c"))

	   (specify "should return the top item when popped"
		    (expect (stack-pop 'stack) equal "c"))

	   (specify "should remove the top item when popped" 
		    (expect (stack-pop 'stack) equal "c")
		    (expect (stack-pop 'stack) equal "b"))))

(context "An empty stack"
	 (for stack)
	 (setup (c)
		(let ((stack ()))
		  (callf c)))
  
	 (specify "should be empty"
		  (expect (length stack) equal 0))
	 
	 (specify "should no longer be empty after push"
		  (stack-push 'stack "anything")
		  (expect (length stack) not-equal 0))

	 (specify "should complain when sent peek"
		  (expect ((lambda (stack) (stack-peek stack)) stack) raises 'stackunderflow))

	 (specify "should complain when sent pop"
		  (expect ((lambda (stack) (stack-pop stack)) stack) raises 'stackunderflow)))

(context "An almost empty stack (with one item)"
	 (for stack)
	 (setup (c)
		(let ((stack ()))
		  (stack-push 'stack 3)))

	 (specify "should not be empty"
		  (expect (length stack) not-equal 0))
  
	 (specify "should remain not empty after peek"
		  (stack-peek 'stack)
		  (expect (length stack) not-equal 0))
	 (specify "should become empty after pop"
		  (stack-pop 'stack)
		  (expect (length stack) equal 0)))

;; context "An almost full stack (with one item less than capacity)" do
;;   setup do
;;     @stack = Stack.new
;;     (1..9).each { |i| @stack.push i }
;;   end
  
;;   specify "should not be full" do
;;     @stack.should_not_be_full
;;   end
  
;;   specify "should become full when sent 'push'" do
;;     @stack.push Object.new
;;     @stack.should_be_full
;;   end
;; end

;; context "A full stack" do
;;   setup do
;;     @stack = Stack.new
;;     (1..10).each { |i| @stack.push i }
;;   end
  
;;   specify "should be full" do
;;     @stack.should_be_full
;;   end
  
;;   specify "should remain full after 'peek'" do
;;     @stack.peek
;;     @stack.should_be_full
;;   end
  
;;   specify "should no longer be full after 'pop'" do
;;     @stack.pop
;;     @stack.should_not_be_full
;;   end

;;   specify "should complain on 'push'" do
;;     lambda { @stack.push Object.new }.should_raise StackOverflowError
;;   end
;; end
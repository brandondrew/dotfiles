;;; Part of behave.el --- Emacs Lisp Behaviour-Driven Development framework

;; Copyright (C) 2007 Phil Hagelberg

;; Note: behave.el usage has changed; haven't gotten around to updating this. See meta.el instead.
;; Example: let's copy http://rspec.rubyforge.org/examples.html

(require 'behave)

(context "A list with three items"
	 (tag list example)
	 (lexical-let ((list (list "a" "b" "c")))

	   (specify "should contain the first item"
		    (expect (first list) equal "a"))

	   (specify "should push new values onto the front"
		    (push "d" list)
		    (expect (first list) equal "d"))

	   (specify "should NOT remove the top item when reading the car"
		    (expect (first list) equal "d")
		    (expect (first list) equal "d"))

	   (specify "should return the top item when popped"
		    (expect (pop list) equal "d"))

	   (specify "should remove the top item when popped" 
		    (expect (pop list) equal "a")
		    (expect (pop list) equal "b"))))

(context "An empty stack"
	 (tag stack example)
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
	 (tag stack)
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
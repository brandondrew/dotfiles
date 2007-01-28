;;; behave.el --- Emacs Lisp Behaviour-Driven Development framework

;; Copyright (C) 2007 Phil Hagelberg

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License can be obtained from the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; For usage see example.el or meta.el

(require 'cl)

(defvar *behave-contexts* '()
  "A list of contexts and their specs.")

(defvar *behave-default-tags* "")

(defstruct context description tags (specs '()))

(defmacro context (description &rest body)
  "Defines a context for specifications to run in. Variable capture warning: sets CONTEXT to the current context."
  (setq *behave-contexts* (delete (context-find description) *behave-contexts*))
  `(lexical-let ((context (make-context)))
     (setf (context-description context) ,description)
     (add-to-list '*behave-contexts* context)
     ,@body))

(defmacro specify (description &rest body)
  "Add a specification and its description to the current context."
  `(setf (context-specs context) ; or in Ruby: context.specs << lambda { description; body }
	 (cons (lambda () ,description ,@body) (context-specs context))))

(defmacro expect (actual &optional predicate expect)
  (case predicate
    ;; TODO: add predicates
    (nil
     `(assert ,actual))
    (equal
     `(assert (equal ,actual ,expect)))
    (error ; no idea if this will work. =)
     (assert (condition-case err
		 (,@actual)
	       (error t))))))

(defmacro tag (&rest tags)
  "Give a context tags for easy reference. (Must be used within a context.)"
  `(setf (context-tags context) (append '(,@tags) (context-tags context))))

;; Context-management

(defun behave-clear-contexts ()
  (interactive)
  (setq *behave-contexts* '())
  (message "Behave: contexts cleared"))

(defun context-find (description)
  "Find a context by its description."
  (find description *behave-contexts* :test (lambda (description context) (equal description (context-description context)))))

(defun context-find-by-tag (tag)
  (remove-if (lambda (context) (not (find tag (context-tags context))))
	     *behave-contexts*))

(defun context-find-by-tags (tags)
  (delete nil (remove-duplicates (mapcan 'context-find-by-tag tags))))

;; Execute

(defun behave (&optional tags)
  "Execute all contexts that match given tags"
  (interactive)
  (let ((tags-string (or tags (read-string (concat "Execute specs matching these tags (default " *behave-default-tags* "): ")
					   nil nil *behave-default-tags*))))
    (setq *behave-default-tags* tags-string) ; update default for next time
    (mapc 'execute-context (context-find-by-tags (mapcar 'intern (split-string tags-string " "))))))

(defun execute-context (context)
  (mapcar #'funcall (context-specs context)))

(provide 'behave)

;(setq max-specpdl-size 5000)

; todo:
; expect macro
; report results in a pretty fashion
;  * error-catching
;  * pass/fail count (lift from elunit)
;  * differentiate errors from expectation failures

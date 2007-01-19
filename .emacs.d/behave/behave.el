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

(defstruct context description tags (setup 'funcall) (specs '()))

(defmacro context (description &rest body)
  `(let ((context (make-context)))
     (setf (context-description context) ,description)
     (dolist (item ',body)
	(case (car item)
	  (for (setf (context-tags context) (cdr item)))
	  (setup (setf (context-setup context) (cadr item)))
	  (specify (setf (context-specs context) (append (list (cdr item)) (context-specs context))))))
     (add-to-list '*behave-contexts* context)))

(defun behave-clear-contexts () 
  (setq *behave-contexts* '()))

(defun context-find (name)
  (find name *behave-contexts* :test (lambda (name context) (equal name (context-description context)))))

(defun context-find-by-tag (tag)
  (find tag *behave-contexts* :test 
	(lambda (tag context) (find tag (context-tags context)))))

(defun context-find-by-tags (tags)
  (mapcan 'context-find-by-tag tags))

(defun behave (&optional tags)
  "Execute all contexts that match given tags"
  (interactive)
  (let ((tags (or tags (read-string "Execute specs matching these tags: " nil nil *behave-default-tags*)))))
    (setq *behave-default-tags* tags) ; update default for next time
    (mapc 'execute-context (delete nil (context-find-by-tags (mapcar 'intern (split-string tags " "))))))

(defun execute-context (context)
  (let ((setup (context-setup context)))
    (dolist (spec (context-specs context))
      (setq spc (cdar spec)))))
;      (funcall setup (cadr spec)))))

(defmacro expect (actual &optional predicate expect)
  (case predicate
    ;; TODO: add predicates
    (nil
     `(assert ,actual))
    (equal
     `(assert (equal ,actual ,expect)))))

;; TODO: pretty reports
(defun spec-report (results)
  )

(provide 'behave)

;(setq max-specpdl-size 5000)


(setq context (context-find "The expect macro"))
(setq spec (last (context-specs context)))
(setq setup (context-setup context))
(funcall setup (lambda () (eval (cadr spec))))

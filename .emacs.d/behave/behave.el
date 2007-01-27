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
  (setq *behave-contexts* (delete (context-find description) *behave-contexts*))
  `(lexical-let ((context (make-context)))
     (setf (context-description context) ,description)
     (add-to-list '*behave-contexts* context)
     ,@body))

(defmacro specify (description body)
  `(setf (context-specs context) ; or in Ruby: context.specs << lambda { description; body }
	 (cons (lambda () ,description (,@body)) (context-specs context))))

(defmacro expect (actual &optional predicate expect)
  (case predicate
    ;; TODO: add predicates
    (nil
     `(assert ,actual))
    (equal
     `(assert (equal ,actual ,expect)))))

(defmacro tag (&rest tags)
  `(setf (context-tags context) (append '(,@tags) (context-tags context))))

;; Context-management

(defun behave-clear-contexts () 
  (interactive)
  (setq *behave-contexts* '())
  (message "Behave: contexts cleared"))

(defun context-find (name)
  (find name *behave-contexts* :test (lambda (name context) (equal name (context-description context)))))

(defun context-find-by-tag (tag)
  (remove-if (lambda (context) (not (find tag (context-tags context))))
	     *behave-contexts*))

(defun context-find-by-tags (tags)
  (remove-duplicates (mapcan 'context-find-by-tag tags)))

;; Execute

(defun behave (&optional tags)
  "Execute all contexts that match given tags"
  (interactive)
  (let ((tags-string (or tags (read-string "Execute specs matching these tags: " nil nil *behave-default-tags*))))
    (setq *behave-default-tags* tags-string) ; update default for next time
    (mapc 'execute-context (delete nil (context-find-by-tags (mapcar 'intern (split-string tags-string " ")))))))

(defun execute-context (context)
  (mapcar #'funcall (context-specs context)))

;; TODO: pretty reports
(defun spec-report (results)
  )

(provide 'behave)

;(setq max-specpdl-size 5000)

; todo:
; multi-form specs (suspect need progn)
; expect macro
; report results in a pretty fashion
; error-catching


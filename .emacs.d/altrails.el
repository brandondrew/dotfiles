;;; altrails.el --- minor mode for editing RubyOnRails code

;; Copyright (C) 2006 Galinsky Dmitry <dima dot exe at gmail dot com>,
;; Phil Hagelberg <technomancy at gmail>

;; Keywords: ruby rails languages oop emacs
;; X-URL:    http://dev.technomancy.us/phil/browser/dotfiles/.emacs.d/altrails.el

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Motivation

;; The original rails.el had a bunch of stuff that I really didn't
;; like. This is a pared-down version that has all the really useful
;; stuff but leaves out what I consider to be the junk. It doesn't use
;; TAGS, it uses snippets that auto-expand, it leaves out webrick
;; management and documentation lookup. (This probably won't work with
;; mmm-mode, which I don't like anyway.)

;;; Features

;; * Custom Rails snippets
;; * Quick switch between action and view
;; * Show coloured logs


;;; Required libraries

(require 'ruby-mode)
(require 'ansi-color)
(require 'snippet)

;;; globals

(defvar rails-templates-list '("rhtml" "rxml" "rjs"))

;;; Set up snippets

(defun rails-snip-ra () (interactive)
  (snippet-insert "render :action => \"$${action}\""))

(defun rails-snip-ral () (interactive)
  (snippet-insert "render :action => \"$${action}\", :layout => \"$${layoutname}\""))

(defun rails-snip-rf () (interactive)
  (snippet-insert "render :file => \"$${filepath}\""))

(defun rails-snip-rfu () (interactive)
  (snippet-insert
   "render :file => \"$${filepath}\", :use_full_path => $${false}"))

(defun rails-snip-ri () (interactive)
  (snippet-insert
   "render :inline => \"$${<%= 'hello' %>}\""))

(defun rails-snip-ril () (interactive)
  (snippet-insert
   "render :inline => \"$${<%= 'hello' %>}\", :locals => { $${name} => \"$${value}\" }"))

(defun rails-snip-rit () (interactive)
  (snippet-insert
   "render :inline => \"$${<%= 'hello' %>}\", :type => :$${rxml})"))

(defun rails-snip-rl () (interactive)
  (snippet-insert
   "render :layout => \"$${layoutname}\""))

(defun rails-snip-rn () (interactive)
  (snippet-insert
   "render :nothing => $${true}"))

(defun rails-snip-rns () (interactive)
  (snippet-insert
   "render :nothing => $${true}, :status => $${401}" ))

(defun rails-snip-rp () (interactive)
  (snippet-insert
   "render :partial => \"$${item}\""))

(defun rails-snip-rpc () (interactive)
  (snippet-insert
   "render :partial => \"$${item}\", :collection => $${items}"))

(defun rails-snip-rpl () (interactive)
  (snippet-insert
   "render :partial => \"$${item}\", :locals => { :$${name} => \"$${value}\"}"))

(defun rails-snip-rpo () (interactive)
  (snippet-insert
   "render :partial => \"$${item}\", :object => $${object}"))

(defun rails-snip-rps () (interactive)
  (snippet-insert
   "render :partial => \"$${item}\", :status => $${500}"))

(defun rails-snip-rt () (interactive)
  (snippet-insert
   "render :text => \"$${Text here...}\""))

(defun rails-snip-rtl () (interactive)
  (snippet-insert
   "render :text => \"$${Text here...}\", :layout => \"$${layoutname}\""))

(defun rails-snip-rtlt () (interactive)
  (snippet-insert
   "render :text => \"$${Text here...}\", :layout => $${true}"))

(defun rails-snip-rts () (interactive)
  (snippet-insert
   "render :text => \"$${Text here...}\", :status => $${401}"))

(defun rails-snip-rcea () (interactive)
  (snippet-insert
   "render_component :action => \"$${index}\""))

(defun rails-snip-rcec () (interactive)
  (snippet-insert
   "render_component :controller => \"$${items}\""))

(defun rails-snip-rceca () (interactive)
  (snippet-insert
   "render_component :controller => \"$${items}\", :action => \"$${index}\""))

(defun rails-snip-rea () (interactive)
  (snippet-insert
   "redirect_to :action => \"$${index}\""))

(defun rails-snip-reai () (interactive)
  (snippet-insert
   "redirect_to :action => \"$${show}\", :id => $${@item}"))

(defun rails-snip-rec () (interactive)
  (snippet-insert
   "redirect_to :controller => \"$${items}\""))

(defun rails-snip-reca () (interactive)
  (snippet-insert
   "redirect_to :controller => \"$${items}\", :action => \"$${list}\""))

(defun rails-snip-recai () (interactive)
  (snippet-insert
   "redirect_to :controller => \"$${items}\", :action => \"$${show}\", :id => $${@item}"))

(defun rails-snip-flash () (interactive)
  (snippet-insert
   "flash[:$${notice}] = \"$${Text here...}\""))

(defun rails-snip-logi () (interactive)
  (snippet-insert
   "logger.info \"$${Text here...}\""))

(defun rails-snip-params () (interactive)
  (snippet-insert
   "params[:$${id}]"))

(defun rails-snip-session () (interactive)
  (snippet-insert
   "session[:$${user}]"))

(defun rails-snip-ar-belongs_to () (interactive)
  (snippet-insert
   "belongs_to :$${model}, :class_name => \"$${class}\", :foreign_key => \"$${key}\""))

(defun rails-snip-ar-has_many () (interactive)
  (snippet-insert
   "has_many :$${model}, :class_name => \"$${class}\", :foreign_key => \"$${key}\", :dependent => :$${destroy}"))

(defun rails-snip-ar-has_one () (interactive)
  (snippet-insert
   "has_one :$${model}, :class_name => \"$${class}\", :foreign_key => \"$${key}\", :dependent => :$${destroy}"))

(defun rails-snip-ar-val_pres () (interactive)
  (snippet-insert
   "validates_presence_of :$${attr}"))

(defun rails-snip-ar-val_uniq () (interactive)
  (snippet-insert
   "validates_uniqueness_of :$${attr}"))

(defun rails-snip-ar-val_num () (interactive)
  (snippet-insert
   "validates_numericality_of :$${attr}"))

(defun rails-snip-erb-ft () (interactive)
  (snippet-insert
   "<%= form_tag :action => \"$${update}\" %>\n$.\n<%= end_form_tag %>"))

(defun rails-snip-erb-lia () (interactive)
  (snippet-insert
   "<%= link_to \"$${title}\", :action => \"$${index}\" %>"))

(defun rails-snip-erb-liai () (interactive)
  (snippet-insert
   "<%= link_to \"$${title}\", :action => \"$${edit}\", :id => $${@item} %>"))

(defun rails-snip-erb-lic () (interactive)
  (snippet-insert
   "<%= link_to \"$${title}\", :controller => \"$${items}\" %>"))

(defun rails-snip-erb-lica () (interactive)
  (snippet-insert
   "<%= link_to \"$${title}\", :controller => \"$${items}\", :action => \"$${index}\" %>"))

(defun rails-snip-erb-licai () (interactive)
  (snippet-insert
   "<%= link_to \"$${title}\", :controller => \"$${items}\", :action => \"$${edit}\", :id => $${@item} %>"))

(defun rails-snip-erb-if () (interactive)
  (snippet-insert "<% if $${cond} -%>\n$.\n<% end -%>"))

(defun rails-snip-erb-ifel () (interactive)
  (snippet-insert "<% if $${cond} -%>\n$.\n<% else -%>\n<% end -%>"))

(defun rails-snip-erb-unless () (interactive)
  (snippet-insert "<% unless $${cond} -%>\n$.\n<% end -%>"))

(defun rails-snip-erb-block () (interactive)
  (snippet-insert "<% $. -%>"))

(defun rails-snip-erb-echo-block () (interactive)
  (snippet-insert "<%= $. %>"))

(defun rails-abbrev-init ()
  "Initialize ruby abbrev table"
  (define-abbrev ruby-mode-abbrev-table "ra"  "" 'rails-snip-ra)
  (define-abbrev ruby-mode-abbrev-table "ral" "" 'rails-snip-ral)
  (define-abbrev ruby-mode-abbrev-table "rf"  "" 'rails-snip-rf)
  (define-abbrev ruby-mode-abbrev-table "rfu" "" 'rails-snip-rfu)
  (define-abbrev ruby-mode-abbrev-table "ri"  "" 'rails-snip-ri)
  (define-abbrev ruby-mode-abbrev-table "ril" "" 'rails-snip-ril)
  (define-abbrev ruby-mode-abbrev-table "rit" "" 'rails-snip-rit)
  (define-abbrev ruby-mode-abbrev-table "rl"  "" 'rails-snip-rl)
  (define-abbrev ruby-mode-abbrev-table "rn"  "" 'rails-snip-rn)
  (define-abbrev ruby-mode-abbrev-table "rns"  "" 'rails-snip-rns)
  (define-abbrev ruby-mode-abbrev-table "rp"  "" 'rails-snip-rp)
  (define-abbrev ruby-mode-abbrev-table "rpc"  "" 'rails-snip-rpc)
  (define-abbrev ruby-mode-abbrev-table "rpl"  "" 'rails-snip-rpl)
  (define-abbrev ruby-mode-abbrev-table "rpo"  "" 'rails-snip-rpo)
  (define-abbrev ruby-mode-abbrev-table "rps"  "" 'rails-snip-rps)
  (define-abbrev ruby-mode-abbrev-table "rt"  "" 'rails-snip-rt)
  (define-abbrev ruby-mode-abbrev-table "rtl"  "" 'rails-snip-rtl)
  (define-abbrev ruby-mode-abbrev-table "rtlt"  "" 'rails-snip-rtlt)
  (define-abbrev ruby-mode-abbrev-table "rts"  "" 'rails-snip-rts)
  (define-abbrev ruby-mode-abbrev-table "rcea"  "" 'rails-snip-rcea)
  (define-abbrev ruby-mode-abbrev-table "rcec"  "" 'rails-snip-rcec)
  (define-abbrev ruby-mode-abbrev-table "rceca"  "" 'rails-snip-rceca)
  (define-abbrev ruby-mode-abbrev-table "rea"  "" 'rails-snip-rea)
  (define-abbrev ruby-mode-abbrev-table "reai"  "" 'rails-snip-reai)
  (define-abbrev ruby-mode-abbrev-table "rec"  "" 'rails-snip-rec)
  (define-abbrev ruby-mode-abbrev-table "reca"  "" 'rails-snip-reca)
  (define-abbrev ruby-mode-abbrev-table "recai"  "" 'rails-snip-recai)
  (define-abbrev ruby-mode-abbrev-table "flash"  "" 'rails-snip-flash)
  (define-abbrev ruby-mode-abbrev-table "logi"  "" 'rails-snip-logi)
  (define-abbrev ruby-mode-abbrev-table "ses"  "" 'rails-snip-session)
  (define-abbrev ruby-mode-abbrev-table "par"  "" 'rails-snip-params)

  (define-abbrev ruby-mode-abbrev-table "belongs"  "" 'rails-snip-ar-belongs_to)
  (define-abbrev ruby-mode-abbrev-table "many"  "" 'rails-snip-ar-has_many)
  (define-abbrev ruby-mode-abbrev-table "one"  "" 'rails-snip-ar-has_one)
  (define-abbrev ruby-mode-abbrev-table "valpres"  "" 'rails-snip-ar-val_pres)
  (define-abbrev ruby-mode-abbrev-table "valuniq"  "" 'rails-snip-ar-val_uniq)
  (define-abbrev ruby-mode-abbrev-table "valnum"  "" 'rails-snip-ar-val_num))


(defun rails-erb-abbrev-init()
  ;; fix undefuned variable html-mode-abbrev-table
  (unless (boundp 'html-mode-abbrev-table)
    (setq html-mode-abbrev-table (make-abbrev-table)))
  (define-abbrev html-mode-abbrev-table "ft"  "" 'rails-snip-erb-ft)
  (define-abbrev html-mode-abbrev-table "lia"  "" 'rails-snip-erb-lia)
  (define-abbrev html-mode-abbrev-table "liai"  "" 'rails-snip-erb-liai)
  (define-abbrev html-mode-abbrev-table "lic"  "" 'rails-snip-erb-lic)
  (define-abbrev html-mode-abbrev-table "lica"  "" 'rails-snip-erb-lica)
  (define-abbrev html-mode-abbrev-table "licai"  "" 'rails-snip-erb-licai)
  (define-abbrev html-mode-abbrev-table "%if"  "" 'rails-snip-erb-if)
  (define-abbrev html-mode-abbrev-table "%unless"  "" 'rails-snip-erb-unless)
  (define-abbrev html-mode-abbrev-table "%ifel"  "" 'rails-snip-erb-ifel)
  (define-abbrev html-mode-abbrev-table "%"  "" 'rails-snip-erb-block)
  (define-abbrev html-mode-abbrev-table "%%"  "" 'rails-snip-erb-echo-block))



;;; Switch between action and view

(defun rails-switch-to-view()
  (let ((pos (nth 2 (posn-at-point)))) ; mouse position at point
    (save-excursion
      (let (action path files)
        (search-backward-regexp "^[ ]*def \\([a-z_]+\\)")
        (setq action (match-string 1))
        (search-backward-regexp "^[ ]*class \\([a-zA-Z0-9_]+\\(::\\([a-zA-Z0-9_]+\\)\\)?\\)Controller[ ]+<")
        (setq path (rails-inflector-underscore (match-string 1)))
        (setq path (concat "app/views/" path "/"))

        (setq files (directory-files
                     (concat (rails-root) path)
                     nil
                     (concat "^" action (rails-make-template-regex))))

        (if (= 1 (list-length files))
            (progn
              (find-file (concat (rails-root) path (car files)))
              (message (concat path action))))

        (if (< 1 (list-length files))
            (let (items tmp file)
              (setq tmp files)
              (setq items (list))
              (while (car tmp)
                (add-to-list 'items (cons (car tmp) (car tmp)))
                (setq tmp (cdr tmp)))

              (setq file
                    (x-popup-menu
                     (list (list (car pos) (cdr pos))
                           (selected-window))
                     (list "Please select.." (cons "Please select.." items ))))
              (if file
                  (progn
                    (find-file (concat (rails-root) path file))
                    (message (concat path action))))))

        (if (> 1 (list-length files))
            (message (concat path action " not found")))))))


(defun rails-switch-to-action()
  (let (file path action root)
    (setq file buffer-file-name)
    (string-match "views/\\([^/]+\\)/\\([^/\.]+\\)\\(/\\([^/\.]+\\)\\)?" file)
    (if (match-beginning 4)
        (progn
          (setq path
                (concat (substring file (match-beginning 1) (match-end 1))
                        "/"
                        (substring file (match-beginning 2) (match-end 2)) ))
          (setq path (concat path "_controller.rb"))
          (setq action (substring file (match-beginning 4) (match-end 4))))
      (progn
        (setq path (concat
                    (substring file (match-beginning 1) (match-end 1))
                    "_controller.rb" ))
        (setq action (substring file (match-beginning 2) (match-end 2))))
      )
    (setq root (rails-root))
    (setq path (concat "app/controllers/" path))
    (if (file-exists-p (concat root path))
        (progn
          (find-file (concat root path))
          (goto-char (point-min))
          (message (concat path "#" action))
          (if (search-forward-regexp (concat "^[ ]*def[ ]*" action))
              (recenter))))))


(defun rails-switch-view-action()
  (interactive)
  (if (string-match "\\.rb$" buffer-file-name)
      (rails-switch-to-view)
    (rails-switch-to-action)))


(defun rails-inflector-underscore (camel-cased-word)
  (let* ((case-fold-search nil)
         (path (replace-regexp-in-string "::" "/" camel-cased-word))
         (path (replace-regexp-in-string "\\([A-Z]+\\)\\([A-Z][a-z]\\)" "\\1_\\2" path))
         (path (replace-regexp-in-string "\\([a-z\\d]\\)\\([A-Z]\\)" "\\1_\\2" path)))
    (downcase path)))


(defun rails-make-template-regex ()
  "Return regex to match rails view templates"
  (let (reg tmp it)
    (setq reg "\\.\\(")
    (setq tmp rails-templates-list)
    (while (setq it (car tmp))
      (progn
        (setq reg (concat reg it))
        (setq tmp (cdr tmp))
        (if (car tmp)
            (setq reg (concat reg "\\|"))
          (setq reg (concat reg "\\)$")))))
    (if reg reg)))


(defun rails-root ()
  "Return RAILS_ROOT"
  (let (curdir max found)
    (setq curdir default-directory)
    (setq max 10)
    (setq found nil)
    (while (and (not found) (> max 0))
      (progn
        (if (file-exists-p (concat curdir "config/environment.rb"))
            (progn
              (setq found t))
          (progn
            (setq curdir (concat curdir "../"))
            (setq max (- max 1))))))
    (if found curdir)))


;;; Logs

;; replace in autorevert.el
(defun auto-revert-tail-handler ()
  (let ((size (nth 7 (file-attributes buffer-file-name)))
        (modified (buffer-modified-p))
        buffer-read-only    ; ignore
        (file buffer-file-name)
        buffer-file-name)   ; ignore that file has changed
    (when (> size auto-revert-tail-pos)
      (undo-boundary)
      (save-restriction
        (widen)
        (save-excursion
          (let ((cur-point (point-max)))
            (goto-char (point-max))
            (insert-file-contents file nil auto-revert-tail-pos size)
            (ansi-color-apply-on-region cur-point (point-max)))))
      (undo-boundary)
      (setq auto-revert-tail-pos size)
      (set-buffer-modified-p modified)))
  (set-visited-file-modtime))

(defun rails-open-log (env)
  (let ((root (rails-root)))
    (if root
        (progn
          (if (file-exists-p (concat root "/log/" env ".log"))
              (progn
                (find-file (concat root "/log/" env ".log"))
                (set-buffer-file-coding-system 'utf-8)
                (ansi-color-apply-on-region (point-min) (point-max))
                (set-buffer-modified-p nil)
                (rails-minor-mode t)
                (goto-char (point-max))
                (setq auto-revert-interval 1)
                (setq auto-window-vscroll t)
                (auto-revert-tail-mode t)))))))

(define-minor-mode alt-rails-minor-mode
  "AltRubyOnRails"
  nil
  " aRoR"
  (list
   (cons "\C-t"  'rails-switch-view-action))

  (rails-abbrev-init)

  (add-hook 'find-file-hooks
	    (lambda()
	      (if (and (string-match (rails-make-template-regex) buffer-file-name)
		       (rails-root))
		  (progn
		    (alt-rails-minor-mode t)
		    (rails-erb-abbrev-init))))))

(provide 'altrails)
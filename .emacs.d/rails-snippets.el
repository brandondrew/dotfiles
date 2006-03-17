(defmacro* def-snip (name &key snip abbrev abbrev-table )
  "  This macro create function with name ``name'', witch generate a snip.
   Also macro sets symbol property ``no-self-insert'' to avoid bug, where
<space>
   replace first default snip value.
   If abbrev and abbrev-table is not nil, macro generate abbrev for this
snip."
  `(progn
     (defun ,name () (interactive)
       (snippet-insert ,snip))
     (put ',name 'no-self-insert t)
     ,@(if (and abbrev abbrev-table)
           (let ((abbrev-tables (if (not (listp abbrev-table))
                                    (list abbrev-table)
                                  abbrev-table)))
             (mapcar #'(lambda (table)
                         `(define-abbrev ,table ,abbrev  "" ',name))
                     abbrev-tables)))))

(defun rails-abbrev-init ()
  "Initialize ruby abbrev table"

  (def-snip rails-snip-ra
    :snip "render :action => \"$${action}\""
    :abbrev "ra"
    :abbrev-table ruby-mode-abbrev-table)


  (def-snip rails-snip-ral
    :snip "render :action => \"$${action}\", :layout => \"$${layoutname}\""
    :abbrev "ral"
    :abbrev-table ruby-mode-abbrev-table)


  (def-snip rails-snip-rf
    :snip "render :file => \"$${filepath}\""
    :abbrev "rf"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-rfu
    :snip "render :file => \"$${filepath}\", :use_full_path => $${false}"
    :abbrev "rfu"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-ri
    :snip "render :inline => \"$${<%= 'hello' %>}\""
    :abbrev "ri"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-ril
    :snip "render :inline => \"$${<%= 'hello' %>}\", :locals => { $${name} => \"$${value}\" }"
    :abbrev "ril"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-rit
    :snip "render :inline => \"$${<%= 'hello' %>}\", :type => :$${rxml})"
    :abbrev "rit"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-rl
    :snip "render :layout => \"$${layoutname}\""
    :abbrev "rl"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-rn
    :snip "render :nothing => $${true}"
    :abbrev "rn"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-rns
    :snip "render :nothing => $${true}, :status => $${401}"
    :abbrev "rns"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-rp
    :snip "render :partial => \"$${item}\""
    :abbrev "rp"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-rpc
    :snip "render :partial => \"$${item}\", :collection => $${items}"
    :abbrev "rpc"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-rpl
    :snip "render :partial => \"$${item}\", :locals => { :$${name} => \"$${value}\"}"
    :abbrev "rpl"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-rpo
    :snip "render :partial => \"$${item}\", :object => $${object}"
    :abbrev "rpo"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-rps
    :snip "render :partial => \"$${item}\", :status => $${500}"
    :abbrev "rps"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-rt
    :snip "render :text => \"$${Text here...}\""
    :abbrev "rt"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-rtl
    :snip "render :text => \"$${Text here...}\", :layout => \"$${layoutname}\""
    :abbrev "rtl"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-rtlt
    :snip "render :text => \"$${Text here...}\", :layout => $${true}"
    :abbrev "rtlt"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-rts
    :snip "render :text => \"$${Text here...}\", :status => $${401}"
    :abbrev "rts"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-rcea
    :snip "render_component :action => \"$${index}\""
    :abbrev "rcea"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-rcec
    :snip "render_component :controller => \"$${items}\""
    :abbrev "rcec"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-rceca
    :snip "render_component :controller => \"$${items}\", :action => \"$${index}\""
    :abbrev "rceca"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-rea
    :snip "redirect_to :action => \"$${index}\""
    :abbrev "rea"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-reai
    :snip "redirect_to :action => \"$${show}\", :id => $${@item}"
    :abbrev "reai"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-rec
    :snip "redirect_to :controller => \"$${items}\""
    :abbrev "rec"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-reca
    :snip "redirect_to :controller => \"$${items}\", :action => \"$${list}\""
    :abbrev "reca"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-recai
    :snip "redirect_to :controller => \"$${items}\", :action => \"$${show}\", :id => $${@item}"
    :abbrev "recai"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-flash
    :snip "flash[:$${notice}] = \"$${Text here...}\""
    :abbrev "flash"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-logi
    :snip "logger.info \"$${Text here...}\""
    :abbrev "logi"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-params
    :snip "params[:$${id}]"
    :abbrev "par"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-session
    :snip "session[:$${user}]"
    :abbrev "ses"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-ar-belongs_to
    :snip "belongs_to :$${model}, :class_name => \"$${class}\", :foreign_key => \"$${key}\""
    :abbrev "belongs"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-ar-has_many
    :snip "has_many :$${model}, :class_name => \"$${class}\", :foreign_key => \"$${key}\", :dependent => :$${destroy}"
    :abbrev "many"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-ar-has_one
    :snip "has_one :$${model}, :class_name => \"$${class}\", :foreign_key => \"$${key}\", :dependent => :$${destroy}"
    :abbrev "one"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-ar-val_pres
    :snip "validates_presence_of :$${attr}"
    :abbrev "valpres"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-ar-val_uniq
    :snip "validates_uniqueness_of :$${attr}"
    :abbrev "valuniq"
    :abbrev-table ruby-mode-abbrev-table)

  (def-snip rails-snip-ar-val_num
    :snip "validates_numericality_of :$${attr}"
    :abbrev "valnum"
    :abbrev-table ruby-mode-abbrev-table))

(defun rails-erb-abbrev-init()
  ;; fix undefuned variable html-mode-abbrev-table
  (unless (boundp 'html-mode-abbrev-table)
    (setq html-mode-abbrev-table (make-abbrev-table)))
  (unless (boundp 'html-helper-mode-abbrev-table)
    (setq html-helper-mode-abbrev-table (make-abbrev-table)))

  (def-snip rails-snip-erb-ft
    :snip "<%= form_tag :action => \"$${update}\" %>\n$.\n<%= end_form_tag %>"
    :abbrev "ft"
    :abbrev-table (html-mode-abbrev-table html-helper-mode-abbrev-table))

  (def-snip rails-snip-erb-lia
    :snip "<%= link_to \"$${title}\", :action => \"$${index}\" %>"
    :abbrev "lia"
    :abbrev-table (html-mode-abbrev-table html-helper-mode-abbrev-table))

  (def-snip rails-snip-erb-liai
    :snip "<%= link_to \"$${title}\", :action => \"$${edit}\", :id => $${@item} %>"
    :abbrev "liai"
    :abbrev-table (html-mode-abbrev-table html-helper-mode-abbrev-table))

  (def-snip rails-snip-erb-lic
    :snip "<%= link_to \"$${title}\", :controller => \"$${items}\" %>"
    :abbrev "lic"
    :abbrev-table (html-mode-abbrev-table html-helper-mode-abbrev-table))

  (def-snip rails-snip-erb-lica
    :snip "<%= link_to \"$${title}\", :controller => \"$${items}\", :action => \"$${index}\" %>"
    :abbrev "lica"
    :abbrev-table (html-mode-abbrev-table html-helper-mode-abbrev-table))

  (def-snip rails-snip-erb-licai
    :snip "<%= link_to \"$${title}\", :controller => \"$${items}\", :action => \"$${edit}\", :id => $${@item} %>"
    :abbrev "licai"
    :abbrev-table (html-mode-abbrev-table html-helper-mode-abbrev-table))

  (def-snip rails-snip-erb-h
    :snip "<% h $${@item} %>"
    :abbrev "%h"
    :abbrev-table (html-mode-abbrev-table html-helper-mode-abbrev-table))

  (def-snip rails-snip-erb-if
    :snip "<% if $${cond} -%>\n$.\n<% end -%>"
    :abbrev "%if"
    :abbrev-table (html-mode-abbrev-table html-helper-mode-abbrev-table))

  (def-snip rails-snip-erb-ifel
    :snip "<% if $${cond} -%>\n$.\n<% else -%>\n<% end -%>"
    :abbrev "%ifel"
    :abbrev-table (html-mode-abbrev-table html-helper-mode-abbrev-table))

  (def-snip rails-snip-erb-unless
    :snip "<% unless $${cond} -%>\n$.\n<% end -%>"
    :abbrev "%unless"
    :abbrev-table (html-mode-abbrev-table html-helper-mode-abbrev-table))

  (def-snip rails-snip-erb-block
    :snip "<% $. -%>"
    :abbrev "%"
    :abbrev-table (html-mode-abbrev-table html-helper-mode-abbrev-table))

  (def-snip rails-snip-erb-echo-block
    :snip "<%= $. %>"
    :abbrev "%%"
    :abbrev-table (html-mode-abbrev-table html-helper-mode-abbrev-table)))

(provide 'rails-snippets)
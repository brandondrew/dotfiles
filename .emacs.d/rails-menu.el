(defvar rails-minor-mode-menu-bar-map
  (let ((map (make-sparse-keymap)))
    (define-key map [rails] (cons "RubyOnRails" (make-sparse-keymap "RubyOnRails")))
    (define-key map [rails svn-status]
      '(menu-item "SVN status"
                  (lambda()
                    (interactive)
                    (svn-status (rails-root))
                    :enable (rails-root))))
    (define-key map [rails tag] '("Update TAGS file" . rails-create-tags))
    (define-key map [rails ri] '("Search documentation" . rails-search-doc))
    (define-key map [rails separator] '("--"))
    (define-key map [rails snip] (cons "Snippets" (make-sparse-keymap "Snippets")))
    (define-key map [rails snip render] (cons "render" (make-sparse-keymap "render")))
    (define-key map [rails snip render sk-ra]  '("render (action)\t(ra)" . rails-snip-ra))
    (define-key map [rails snip render sk-ral] '("render (action,layout)\t(ral)" . rails-snip-ral))
    (define-key map [rails snip render sk-rf]  '("render (file)\t(rf)" . rails-snip-rf))
    (define-key map [rails snip render sk-rfu] '("render (file,use_full_path)\t(rfu)" . rails-snip-rfu))
    (define-key map [rails snip render sk-ri]  '("render (inline)\t(ri)" . rails-snip-ri))
    (define-key map [rails snip render sk-ril] '("render (inline,locals)\t(ril)" . rails-snip-ril))
    (define-key map [rails snip render sk-rit] '("render (inline,type)\t(rit)" . rails-snip-rit))
    (define-key map [rails snip render sk-rl]  '("render (layout)\t(rl)" . rails-snip-rl))
    (define-key map [rails snip render sk-rn]  '("render (nothing)\t(rn)" . rails-snip-rn))
    (define-key map [rails snip render sk-rns] '("render (nothing,status)\t(rns)" . rails-snip-rns))
    (define-key map [rails snip render sk-rp]  '("render (partial)\t(rp)" . rails-snip-rp))
    (define-key map [rails snip render sk-rpc] '("render (partial,collection)\t(rpc)" . rails-snip-rpc))
    (define-key map [rails snip render sk-rpl] '("render (partial,locals)\t(rpl)" . rails-snip-rpl))
    (define-key map [rails snip render sk-rpo] '("render (partial,object)\t(rpo)" . rails-snip-rpo))
    (define-key map [rails snip render sk-rps] '("render (partial,status)\t(rps)" . rails-snip-rps))
    (define-key map [rails snip render sk-rt] '("render (text)\t(rt)" . rails-snip-rt))
    (define-key map [rails snip render sk-rtl] '("render (text,layout)\t(rtl)" . rails-snip-rtl))
    (define-key map [rails snip render sk-rtlt] '("render (text,layout => true)\t(rtlt)" . rails-snip-rtlt))
    (define-key map [rails snip render sk-rcea] '("render_component (action)\t(rcea)" . rails-snip-rcea))
    (define-key map [rails snip render sk-rcec] '("render_component (controller)\t(rcec)" . rails-snip-rcec))
    (define-key map [rails snip render sk-rceca] '("render_component (controller, action)\t(rceca)" . rails-snip-rceca))

    (define-key map [rails snip redirect_to] (cons "redirect_to" (make-sparse-keymap "redirect_to")))
    (define-key map [rails snip redirect_to sk-rea] '("redirect_to (action)\t(rea)" . rails-snip-rea))
    (define-key map [rails snip redirect_to sk-reai] '("redirect_to (action, id)\t(reai)" . rails-snip-reai))
    (define-key map [rails snip redirect_to sk-rec] '("redirect_to (controller)\t(rec)" . rails-snip-rec))
    (define-key map [rails snip redirect_to sk-reca] '("redirect_to (controller, action)\t(reca)" . rails-snip-reca))
    (define-key map [rails snip redirect_to sk-recai] '("redirect_to (controller, action, id)\t(recai)" . rails-snip-recai))

    (define-key map [rails snip controller] (cons "controller" (make-sparse-keymap "controller")))
    (define-key map [rails snip controller sk-flash] '("flash[...]\t(flash)" . rails-snip-flash))
    (define-key map [rails snip controller sk-logi] '("logger.info\t(logi)" . rails-snip-logi))
    (define-key map [rails snip controller sk-params] '("params[...]\t(par)" . rails-snip-params))
    (define-key map [rails snip controller sk-session] '("session[...]\t(ses)" . rails-snip-session))

    (define-key map [rails snip model] (cons "model" (make-sparse-keymap "model")))
    (define-key map [rails snip model sk-belongs_to] '("belongs_to (class_name,foreign_key)\t(belongs)" . rails-snip-ar-belongs_to))
    (define-key map [rails snip model sk-has_many] '("has_many (class_name,foreign_key,dependent)\t(many)" . rails-snip-ar-has_many))
    (define-key map [rails snip model sk-has_one] '("has_one (class_name,foreign_key,dependent)\t(one)" . rails-snip-ar-has_one))
    (define-key map [rails snip model sk-val_pres] '("validates_presence_of\t(valpres)" . rails-snip-ar-val_pres))
    (define-key map [rails snip model sk-val_uniq] '("validates_uniqueness_of\t(valuniq)" . rails-snip-ar-val_uniq))
    (define-key map [rails snip model sk-val_num] '("validates_numericality_of\t(valnum)" . rails-snip-ar-val_num))

    (define-key map [rails snip rhtml] (cons "rhtml" (make-sparse-keymap "rhtml")))
    (define-key map [rails snip rhtml sk-erb-ft] '("form_tag\t(ft)" . rails-snip-erb-ft))
    (define-key map [rails snip rhtml sk-erb-lia] '("link_to (action)\t(lia)" . rails-snip-erb-lia))
    (define-key map [rails snip rhtml sk-erb-liai] '("link_to (action, id)\t(liai)" . rails-snip-erb-liai))
    (define-key map [rails snip rhtml sk-erb-lic] '("link_to (controller)\t(lic)" . rails-snip-erb-lic))
    (define-key map [rails snip rhtml sk-erb-lica] '("link_to (controller, action)\t(lica)" . rails-snip-erb-lica))
    (define-key map [rails snip rhtml sk-erb-licai] '("link_to (controller, action, id)\t(licai)" . rails-snip-erb-licai))
    (define-key map [rails snip rhtml sk-erb-ft] '("form_tag\t(ft)" . rails-snip-erb-ft))
    (define-key map [rails snip rhtml sk-erb-h] '("<% h ... %>\t(%h)" . rails-snip-erb-h))
    (define-key map [rails snip rhtml sk-erb-if] '("<% if/end %>\t(%if)" . rails-snip-erb-if))
    (define-key map [rails snip rhtml sk-erb-unless] '("<% unless/end %>\t(%unless)" . rails-snip-erb-unless))
    (define-key map [rails snip rhtml sk-erb-ifel] '("<% if/else/end %>\t(%ifel)" . rails-snip-erb-ifel))
    (define-key map [rails snip rhtml sk-erb-block] '("<% ... %>\t(%)" . rails-snip-erb-block))
    (define-key map [rails snip rhtml sk-erb-echo-block] '("<%= ... %>\t(%%)" . rails-snip-erb-echo-block))

    (define-key map [rails log] (cons "Open log" (make-sparse-keymap "Open log")))
    (define-key map [rails log test]
      '("test.log" . (lambda() (interactive) (rails-open-log "test"))))
    (define-key map [rails log pro]
      '("production.log" . (lambda() (interactive) (rails-open-log "production"))))
    (define-key map [rails log dev]
      '("development.log" . (lambda() (interactive) (rails-open-log "development"))))

    (define-key map [rails config] (cons "Configuration" (make-sparse-keymap "Configuration")))
    (define-key map [rails config routes]
      '("routes.rb" .
        (lambda()
          (interactive)
          (let ((root (rails-root)))
            (if root (find-file (concat root "config/routes.rb")))))))
    (define-key map [rails config environment]
      '("environment.rb" .
        (lambda()
          (interactive)
          (let ((root (rails-root)))
            (if root (find-file (concat root "config/environment.rb")))))))
    (define-key map [rails config database]
      '("database.yml" .
        (lambda()
          (interactive)
          (let ((root (rails-root)))
            (if root (find-file (concat root "config/database.yml")))))))
    (define-key map [rails config boot]
      '("boot.rb" .
        (lambda()
          (interactive)
          (let ((root (rails-root)))
            (if root (find-file (concat root "config/boot.rb")))))))

    (define-key map [rails config env] (cons "environments" (make-sparse-keymap "environments")))
    (define-key map [rails config env test]
      '("test.rb" .
        (lambda()
          (interactive)
          (let ((root (rails-root)))
            (if root (find-file (concat root "config/environments/test.rb")))))))
    (define-key map [rails config env production]
      '("production.rb" .
        (lambda()
          (interactive)
          (let ((root (rails-root)))
            (if root (find-file (concat root "config/environments/production.rb")))))))
    (define-key map [rails config env development]
      '("development.rb" .
        (lambda()
          (interactive)
          (let ((root (rails-root)))
            (if root (find-file (concat root "config/environments/development.rb")))))))

    (define-key map [rails webrick] (cons "WEBrick" (make-sparse-keymap "WEBrick")))

    (define-key map [rails webrick mongrel]
      '(menu-item "Use Mongrel" rails-toggle-use-mongrel
                  :enable (not (rails-webrick-process-status))
                  :button (:toggle
                           . (and (boundp 'rails-use-mongrel)
                                   rails-use-mongrel))))

    (define-key map [rails webrick separator] '("--"))

    (define-key map [rails webrick buffer]
      '(menu-item "Show buffer"
                  rails-webrick-open-buffer
                  :enable (rails-webrick-process-status)))
    (define-key map [rails webrick url]
      '(menu-item "Open browser"
                  rails-webrick-open-browser
                  :enable (rails-webrick-process-status)))
    (define-key map [rails webrick stop]
      '(menu-item "Stop"
                  rails-webrick-process-stop
                  :enable (rails-webrick-process-status)))
    (define-key map [rails webrick test]
      '(menu-item "Start test"
                  (lambda() (interactive)
                    (rails-webrick-process "test"))
                  :enable (not (rails-webrick-process-status))))
    (define-key map [rails webrick production]
      '(menu-item "Start production"
                  (lambda() (interactive)
                    (rails-webrick-process "production"))
                  :enable (not (rails-webrick-process-status))))
    (define-key map [rails webrick development]
      '(menu-item "Start development"
                  (lambda() (interactive)
                    (rails-webrick-process "development"))
                  :enable (not (rails-webrick-process-status))))

    (define-key map [rails switch-va] '("Switch Action/View" . rails-switch-view-action))

    map))

(provide 'rails-menu)
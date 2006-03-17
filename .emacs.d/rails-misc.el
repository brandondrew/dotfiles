
(defun ruby-indent-or-complete ()
  "Complete if point is at end of a word, otherwise indent line."
  (interactive)
  (if snippet
      (snippet-next-field)
    (if (looking-at "\\>")
        (hippie-expand nil)
      (ruby-indent-command))))


(defun ruby-newline-and-indent ()
  (interactive)
  (newline)
  (ruby-indent-command))



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



(defun rails-search-doc (&rest item)
  (interactive)
  (if (or (not (boundp item))
          (not item))
      (setq item (thing-at-point 'sexp)))
  (unless item
    (setq item (read-string "Search symbol? ")))
  (if item
      (let ((buf (buffer-name)))
        (if (and rails-chm-file
                 (file-exists-p rails-chm-file))
            (progn
              (start-process "keyhh" "*keyhh*" "keyhh.exe" "-#klink"
                             (format "'%s'" item)  rails-chm-file))
            (progn
              (unless (string= buf "*ri*")
                (switch-to-buffer-other-window "*ri*"))
              (setq buffer-read-only nil)
              (kill-region (point-min) (point-max))
              (message (concat "Please wait..."))
              (call-process "ri" nil "*ri*" t item)
              (setq buffer-read-only t)
              (local-set-key [return] 'rails-search-doc)
              (goto-char (point-min)))))))


(defun rails-create-tags()
  "Create tags file"
  (interactive)
  (let ((root (rails-root)) dir cmd)
    (message "Creating TAGS, please wait...")
    (setq dir default-directory)
    (setq default-directory root)
    (setq cmd "ctags -e -a --Ruby-kinds=-f -o %s -R %s %s")

    (shell-command (format cmd tags-file-name (concat root "app") (concat root "lib")))

    (setq default-directory dir)
    (visit-tags-table tags-file-name)))


(defun rails-toggle-use-mongrel()
  (interactive)
  (let ((toggle (boundp 'rails-use-mongrel)))
    (setq rails-use-mongrel (not rails-use-mongrel))))

(provide 'rails-misc)
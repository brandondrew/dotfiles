
(defun rails-webrick-open-browser()
  (interactive)
  (browse-url rails-webrick-url))


(defun rails-webrick-open-buffer()
  (interactive)
  (switch-to-buffer rails-webrick-buffer-name))


(defun rails-webrick-sentinel (proc msg)
  (if (memq (process-status proc) '(exit signal))
        (message
         (concat
          (if rails-use-mongrel "Mongrel" "WEBrick") " stopped"))))


(defun rails-webrick-process-status()
  (let (st)
    (setq st (get-buffer-process rails-webrick-buffer-name))
    (if st t nil)))

(defun rails-webrick-process-stop()
  (interactive)
  (let (proc)
    (setq proc (get-buffer-process rails-webrick-buffer-name))
    (if proc
        (kill-process proc))))


(defun rails-webrick-process(env)
  (let (proc dir root)
    (setq proc (get-buffer-process rails-webrick-buffer-name))
    (unless proc
      (progn
        (setq root (rails-root))
        (if root
            (progn
              (setq dir default-directory)
              (setq default-directory root)
              (if rails-use-mongrel
                  (setq proc
                        (apply 'start-process-shell-command
                               "mongrel_rails"
                               rails-webrick-buffer-name
                               "mongrel_rails"
                               (list "start" "0.0.0.0" rails-webrick-port)))
                (setq proc
                      (apply 'start-process-shell-command
                             rails-ruby-command
                             rails-webrick-buffer-name
                             rails-ruby-command
                             (list (concat root "script/server")
                                   (concat " -e " env)
                                   (concat " -p " rails-webrick-port)))))
              (set-process-filter proc 'rails-webrick-filter)
              (set-process-sentinel proc 'rails-webrick-sentinel)
              (setq default-directory dir)

              (message (concat (if rails-use-mongrel
                                   "Mongrel" "Webrick")
                               "(" env  ") started with port " rails-webrick-port)))
          (progn
            (message "RAILS_ROOT not found")))))))


(defun rails-webrick-filter (process line)
  (let ((buffer (current-buffer)))
    (switch-to-buffer rails-webrick-buffer-name)
    (goto-char(point-min))
    (insert line)
    (switch-to-buffer buffer)))

(provide 'rails-webrick)
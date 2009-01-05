(setq jabber-resource (concat "emacs-" (shell-command-to-string "hostname"))
      jabber-server "hagelb.org"
      jabber-username "phil"
      jabber-roster-show-bindings nil)

(eval-after-load 'jabber
  '(set-face-attribute 'jabber-title-large nil :height 1.5))

(defun jabber-notify-message (from buffer text propsed-alert)
  (shell-command (concat "notify-send \"" propsed-alert "\"")))
;;;   (shell-command "ruby ~/bin/morselight.rb %s"
;;;                  (shell-quote-argument proposed-alert)))

(add-hook 'jabber-alert-message-hooks 'jabber-notify-message)

;; TODO: look into using same-window-regexps to make a jabber-only window.
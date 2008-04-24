(setq jabber-nickname ""
      jabber-password nil
      jabber-resource (concat "emacs-" (shell-command-to-string "hostname"))
      jabber-server "hagelb.org"
      jabber-username "phil"
      jabber-roster-show-bindings nil)

(defun jabber-notify-message (from buffer text propsed-alert)
  (shell-command (concat "notify-send \"" "Jabber message from "
			 from "\" \""
			 text "\""))
  (shell-command "ruby ~/bin/morselight.rb %s" (shell-quote-argument text)))

(add-hook 'jabber-alert-message-hooks 'jabber-notify-message)

(setq gnus-select-method '(nnnil ""))
(setq gnus-secondary-select-methods '((nnml ""
					    (nnml-directory "~/Mail"))))

(setq user-mail-address "phil@hagelb.org")
(setq user-full-name "Phil Hagelberg")
(setq gnus-ignored-from-addresses "Phil Hagelberg")

(setq send-mail-function 'smtpmail-send-it)
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-default-smtp-server "mail.hagelb.org")
(setq smtpmail-auth-credentials '(("mail.hagelb.org" 25 "m7139145" "testyy")))
(setq message-kill-buffer-on-exit t)

(setq gnus-message-archive-group "sent")

(setq nnmail-split-methods 'nnmail-split-fancy)
(setq nnmail-crosspost nil)

(setq nnmail-split-fancy
      '(| (to "ruby-talk@ruby-lang\\.org" "ruby-talk")
	  (to "ruby_emacs_dev@yahoogroups\\.com" "emacs-rails")
	  (to "rails" "rails")
	  (to "obby-users@list.0x539.de" "obby-users")
	  (any "conkeror" "conkeror")
	  (any "cartographer" "cartographer")
	  (any "ocruby" "ocruby")

	  (any "planetargon" "argon")

	  (any "Jed Levin" "ujive")
	  (any "Marshall Pierce" "ujive")
	  (any "brent.hargrave" "ujive")

	  (any "Paxtel Notifier" junk)
	  (any "phil@dev.\\.paxtel\\.com" junk)
	  (any ".*@dev\\.paxtel\\.com" "checkins")
	  (any "tmornini@.*" "paxtel")
	  (any "paxgrid@rogers\\.com" "paxtel")
	  (any "dallas\\.reedy@gmail\\.com" "paxtel")
	  (any ".*@paxtel\\.com" "paxtel")

	  (any "cron" junk)
	  ("subject" "EQSE News" junk)
	  (to "phil@localhost" "feeds")

	  (any "zacchaeus.*" "friends")

	  (any ".*kleist.*" "grace-group")
	  (any "alisha\\.e\\.hagelberg@biola\\.edu" "alisha")
	  (from "Hagelberg" - "Alisha" "family")

	  (any ".*hackelford.*" "friends")
	  (any ".*peckham.*" "friends")
	  (any ".*carroll.*" "friends")
	  (any ".*guenther.*" "friends")
	  (any ".*rowley.*" "friends")
	  (any ".*malabuyo.*" "friends")
	  (any ".*holloway.*" "friends")

	  (any "Benjamin Bryan" "friends")
	  (any "Arko" "friends")
	  (any "Joel Watson" "friends")
	  "inbox"))

(setq mail-sources '((imap 
		      :server "mail.hagelb.org"
		      :user "philhag"
		      :stream ssl)
		     (file
		      :path "/var/mail/phil")
		     (imap
		      :server "mail.hagelb.org"
		      :user "m2643488"
		      :stream ssl)
))

(setq imap-ssl-program "/usr/bin/openssl s_client -ssl3 -connect %s:%p")
(setq gnus-agent-expire-days 0)
(setq gnus-agent-enable-expiration 'DISABLE)

(gnus-demon-add-handler 'gnus-group-get-new-news 10 t)
(gnus-demon-init)

(add-hook 'message-mode-hook 'auto-fill-mode)
(add-hook 'message-mode-hook 'flyspell-mode)

;; (setq gnus-buffer-configuration 
;;       '((group (vertical 1.0 (group 1.0 point)))
;; 	(summary (vertical 1.0 (summary 1.0 point)))
;; 	(article (horizontal 1.0 (summary 1.0 point)
;; 			   (article 80)))))


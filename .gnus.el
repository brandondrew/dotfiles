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
      '(| (any "tmornini@.*" "work")
	  (any "paxgrid@rogers\\.com" "work")
	  (any "dallas\\.reedy@gmail\\.com" "work")
	  (any ".*@dev\\.paxtel\\.com" "checkins")
	  (any ".*@paxtel\\.com" "work")

	  (to "ruby-talk@ruby-lang\\.org" "ruby-talk")
	  (to "obby-users@list.0x539.de" "obby-users")
	  (to "feeds@hagelb.org" "feeds")

	  (any "zacchaeus.*" "friends")

	  (any ".*kleist.*" "grace-group")
	  (any "alisha\\.e\\.hagelberg@biola\\.edu" "alisha")
	  (from "Hagelberg" - "Alisha" "family")

	  (any "conkeror" "conkeror")

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
		      :stream ssl
		      :predicate "UNSEEN"
		      :fetchflag "\\Seen")
		     (imap 
		      :server "mail.hagelb.org"
		      :user "m1824678"
		      :stream ssl
		      :predicate "UNSEEN"
		      :fetchflag "\\Seen")
))

(setq imap-ssl-program "/usr/bin/openssl s_client -ssl3 -connect %s:%p")
(setq gnus-agent-expire-days 0)

(gnus-demon-add-handler 'gnus-group-get-new-news 10 t)
(gnus-demon-init)

(add-hook 'message-mode-hook 'auto-fill-mode)
(add-hook 'message-mode-hook 'flyspell-mode)

(setq gnus-sum-thread-tree-leaf-with-other "+-> ")
(setq gnus-sum-thread-tree-vertical "|")
(setq gnus-sum-thread-tree-single-leaf "`-> ")

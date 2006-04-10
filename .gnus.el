(setq gnus-select-method '(nnnil ""))
(setq gnus-secondary-select-methods '((nnml ""
					    (nnml-directory "~/mjolnir/Mail"))))

(setq user-mail-address "phil@hagelb.org")
(setq user-full-name "Phil Hagelberg")

(setq send-mail-function 'smtpmail-send-it)
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-default-smtp-server "mail.hagelb.org")
(setq smtpmail-auth-credentials '(("mail.hagelb.org" 25 "m7139145" "testyy")))

(setq nnmail-split-methods 'nnmail-split-fancy)
(setq nnmail-crosspost nil)

(setq nnmail-split-fancy
      '(| (any "tmornini@.*" "work")
	  (any "paxgrid@rogers\\.com" "work")
	  (from "dev\\.paxtel\\.com" "checkins")
	  (any ".*@paxtel\\.com" "work")

	  (any "alisha\\.e\\.hagelberg@biola\\.edu" "alisha")
	  (any "Hagelberg" - "Alisha" "family")

	  (any "conkeror" "conkeror")
	  (to "ruby-talk@ruby-lang\\.org" "ruby-talk")

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
		      :fetchflag "\\Seen")))

(setq imap-ssl-program "/usr/bin/openssl s_client -ssl3 -connect %s:%p")
(setq gnus-agent-expire-days 0)

(gnus-demon-add-handler 'gnus-group-get-new-news 10 t)
(gnus-demon-init)

(add-hook 'message-mode-hook 'auto-fill-mode)

 (setq gnus-topic-line-format "%i[ %u&topic-line; ] %v\n")
    

(defun gnus-user-format-function-topic-line (dummy)
  (let ((topic-face (if (zerop total-number-of-articles)
			'gnus-group-mail-2-empty
		      'gnus-group-mail-2)))
    (propertize
     (format "%s %d" name total-number-of-articles)
     'face topic-face)))
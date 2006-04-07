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

(setq nnmail-split-fancy
      '(| ("any" "tmornini" "work")
	  ("any" "Martin Nathanson" "work")
	  ("from" "dev.paxtel.com" "checkins")
	  ("any" "paxtel.com" "work")

	  ("any" "Alisha Hagelberg" "alisha")
	  ("any" "Hagelberg" - "Alisha" "family")

	  ("any" "conkeror" "conkeror")

	  ("any" "hackelford" "friends")
	  ("any" "Peckham" "friends")
	  ("any" "Carroll" "friends")
	  ("any" "Dustin Guenther" "friends")
	  ("any" "Rowley" "friends")
	  ("any" "Malabuyo" "friends")
	  ("any" "Eric Holloway" "friends")

	  ("any" "Benjamin Bryan" "friends")
	  ("any" "Arko" "friends")
	  ("any" "Joel Watson" "friends")
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


;; formatting stuff below:

(dolist (c '((?\207 . ?q) (?\216 . ?x) (?\212 . ?t) (?\203 . ?m)))
  (aset standard-display-table (car c)
	(vector (create-glyph (concat "\e(0" "\e[35m" ; magenta
				      (char-to-string (cdr c))
				      "\e[0m" "\e(B")))))

(setq gnus-sum-thread-tree-vertical "\216"
      gnus-sum-thread-tree-root ""
      gnus-sum-thread-tree-false-root ""
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-single-indent ""
      gnus-sum-thread-tree-leaf-with-other "\212\207>"
      gnus-sum-thread-tree-single-leaf "\203\207>")

(defun gnus-user-format-function-thread (dummy)
  (propertize gnus-tmp-thread-tree-header-string 'gnus-face t))

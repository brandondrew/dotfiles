(setq gnus-select-method '(nnnil ""))
(setq gnus-secondary-select-methods '((nnml ""
					    (nnml-directory "~/mjolnir/Mail"))))

(setq user-mail-address "phil@hagelb.org")
(setq user-full-name "Phil Hagelberg")

(setq send-mail-function 'smtpmail-send-it)
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-default-smtp-server "mail.hagelb.org")
(setq smtpmail-auth-credentials '(("mail.hagelb.org" "587" "m7139145" "testyy")))

(setq nnmail-split-methods
      '(("work" "From:.*paxtel|From:.*tmornini|From:.*paxgrid")
	("alisha" "From:.*lisha.*agelberg")
	("family" "From:.*agelberg")
	("trash" "From:.*Cron Daemon")
	("other" "")))

(setq mail-sources
      '((maildir :path "~/mjolnir/Maildir"
         :subdirs ("cur" "new"))))


;; General config
(setq gnus-select-method '(nnnil ""))
(setq gnus-secondary-select-methods '((nnml ""
					    (nnml-directory "~/Mail"))))
(setq gnus-home-directory "~/.emacs.d")

;; Sending mail details
(setq user-mail-address "phil@evri.com")
(setq user-full-name "Phil Hagelberg")
(setq gnus-ignored-from-addresses "Phil Hagelberg")
(setq gnus-message-archive-group "sent")

;; SMTP
(setq smtpmail-starttls-credentials '(("mail.hagelb.org" 587 nil nil))
      smtpmail-smtp-server "mail.hagelb.org"
      smtpmail-default-smtp-server "mail.hagelb.org"
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-service 587
      smtpmail-auth-credentials '(("mail.hagelb.org"
				   587
				   "m7139145"
				   "testyy")))

;; Incoming mail
(setq mail-sources '((file :path "/var/mail/phil")))

;; Sorting
(setq nnmail-split-methods 'nnmail-split-fancy)
(setq nnmail-crosspost nil)

(setq nnmail-split-fancy
      '(| (from "repoman" "checkins")
	  (from "JIRA" "jira")
	  (from "zabbix" "zabbix")
	  (from "infact" "infact")
	  (any "articles@" "articles")
	  (to "phil@localhost" "feeds")
	  (from "xplanner" junk)
	  (from "Apple" junk)
	  "inbox"))

;; Misc settings
(setq message-kill-buffer-on-exit t
      gnus-fetch-old-headers 'some
      gnus-agent-expire-days 0
      gnus-agent-enable-expiration 'DISABLE
      gnus-use-full-window nil
      mm-text-html-renderer 'w3m)

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(add-hook 'message-mode-hook 'auto-fill-mode)
(add-hook 'message-mode-hook 'flyspell-mode)

(mailcap-add "image/jpeg" "display")

;; Cosmetics
(setq gnus-summary-line-format "%O%U%R%z%d %B%(%[%4L: %-22,22f%]%) %s\n")
(setq gnus-summary-mode-line-format "Gnus: %p [%A / Sc:%4z] %Z")

(setq gnus-summary-same-subject "")
(setq gnus-sum-thread-tree-root "")
(setq gnus-sum-thread-tree-single-indent "")
(setq gnus-sum-thread-tree-leaf-with-other "+-> ")
(setq gnus-sum-thread-tree-vertical "|")
(setq gnus-sum-thread-tree-single-leaf "`-> ")


;; Searching
(require 'gnus-namazu)
(gnus-namazu-insinuate)
(setq gnus-namazu-index-directories
      (list (expand-file-name "~/.cache/namazu")))


;; auto-check
(gnus-demon-add-handler 'gnus-group-get-new-news 10 t)
(gnus-demon-init)

;; TODO:
;; - ical integration: http://bc.tech.coop/blog/070306.html and https://zimbra.evri.com/home/phil/Calendar/
;; - what is gnu-registry?
;; - bbdb?
;; - sort group by
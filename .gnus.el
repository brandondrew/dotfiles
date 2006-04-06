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
	("inbox" "")))

(setq mail-sources '((imap 
		      :server "mail.hagelb.org"
		      :user "philhag"
		      :stream ssl
		      :predicate "UNSEEN"
		      :fetchflag "\\Seen")))

(setq imap-ssl-program "/usr/bin/openssl s_client -ssl3 -connect %s:%p")
(setq gnus-agent-expire-days 0)

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


(defun color-theme-dark-gnus ()
  "Color theme for gnus and message faces only.
This is intended for other color themes to use
\(eg. `color-theme-late-night')."
  (interactive)
  (color-theme-install
   '(color-theme-blue-gnus
     nil
     (gnus-cite-attribution-face ((t (:foreground "#bbb"))))
     (gnus-cite-face-1 ((t (:foreground "#aaa"))))
     (gnus-cite-face-2 ((t (:foreground "#aaa"))))
     (gnus-cite-face-3 ((t (:foreground "#aaa"))))
     (gnus-cite-face-4 ((t (:foreground "#aaa"))))
     (gnus-cite-face-5 ((t (:foreground "#aaa"))))
     (gnus-cite-face-6 ((t (:foreground "#aaa"))))
     (gnus-cite-face-7 ((t (:foreground "#aaa"))))
     (gnus-cite-face-8 ((t (:foreground "#aaa"))))
     (gnus-cite-face-9 ((t (:foreground "#aaa"))))
     (gnus-emphasis-bold ((t (:bold t))))
     (gnus-emphasis-bold-italic ((t (:italic t :bold t))))
     (gnus-emphasis-highlight-words ((t (:foreground "#ccc"))))
     (gnus-emphasis-italic ((t (:italic t))))
     (gnus-emphasis-underline ((t (:underline t))))
     (gnus-emphasis-underline-bold ((t (:bold t :underline t))))
     (gnus-emphasis-underline-bold-italic ((t (:italic t :bold t :underline t))))
     (gnus-emphasis-underline-italic ((t (:italic t :underline t))))
     (gnus-group-mail-1-empty-face ((t (:foreground "#999"))))
     (gnus-group-mail-1-face ((t (:bold t :foreground "#999"))))
     (gnus-group-mail-2-empty-face ((t (:foreground "#999"))))
     (gnus-group-mail-2-face ((t (:bold t :foreground "#999"))))
     (gnus-group-mail-3-empty-face ((t (:foreground "#888"))))
     (gnus-group-mail-3-face ((t (:bold t :foreground "#888"))))
     (gnus-group-mail-low-empty-face ((t (:foreground "#777"))))
     (gnus-group-mail-low-face ((t (:bold t :foreground "#777"))))
     (gnus-group-news-1-empty-face ((t (:foreground "#999"))))
     (gnus-group-news-1-face ((t (:bold t :foreground "#999"))))
     (gnus-group-news-2-empty-face ((t (:foreground "#888"))))
     (gnus-group-news-2-face ((t (:bold t :foreground "#888"))))
     (gnus-group-news-3-empty-face ((t (:foreground "#777"))))
     (gnus-group-news-3-face ((t (:bold t :foreground "#777"))))
     (gnus-group-news-4-empty-face ((t (:foreground "#666"))))
     (gnus-group-news-4-face ((t (:bold t :foreground "#666"))))
     (gnus-group-news-5-empty-face ((t (:foreground "#666"))))
     (gnus-group-news-5-face ((t (:bold t :foreground "#666"))))
     (gnus-group-news-6-empty-face ((t (:foreground "#666"))))
     (gnus-group-news-6-face ((t (:bold t :foreground "#666"))))
     (gnus-group-news-low-empty-face ((t (:foreground "#666"))))
     (gnus-group-news-low-face ((t (:bold t :foreground "#666"))))
     (gnus-header-content-face ((t (:foreground "#888"))))
     (gnus-header-from-face ((t (:bold t :foreground "#888"))))
     (gnus-header-name-face ((t (:bold t :foreground "#777"))))
     (gnus-header-newsgroups-face ((t (:bold t :foreground "#777"))))
     (gnus-header-subject-face ((t (:bold t :foreground "#999"))))
     (gnus-signature-face ((t (:foreground "#444"))))
     (gnus-splash-face ((t (:foreground "#ccc"))))
     (gnus-summary-cancelled-face ((t (:background "#555" :foreground "#000"))))
     (gnus-summary-high-ancient-face ((t (:bold t :foreground "#555"))))
     (gnus-summary-high-read-face ((t (:bold t :foreground "#666"))))
     (gnus-summary-high-ticked-face ((t (:bold t :foreground "#777"))))
     (gnus-summary-high-unread-face ((t (:bold t :foreground "#888"))))
     (gnus-summary-low-ancient-face ((t (:foreground "#444"))))
     (gnus-summary-low-read-face ((t (:foreground "#555"))))
     (gnus-summary-low-ticked-face ((t (:foreground "#666"))))
     (gnus-summary-low-unread-face ((t (:foreground "#777"))))
     (gnus-summary-normal-ancient-face ((t (:foreground "#555"))))
     (gnus-summary-normal-read-face ((t (:foreground "#666"))))
     (gnus-summary-normal-ticked-face ((t (:foreground "#777"))))
     (gnus-summary-normal-unread-face ((t (:foreground "#888"))))
     (gnus-summary-selected-face ((t (:background "#333"))))
     (message-cited-text-face ((t (:foreground "#aaa"))))
     (message-header-cc-face ((t (:foreground "#888"))))
     (message-header-name-face ((t (:bold t :foreground "#777"))))
     (message-header-newsgroups-face ((t (:bold t :foreground "#777"))))
     (message-header-other-face ((t (:foreground "#666"))))
     (message-header-subject-face ((t (:bold t :foreground "#999"))))
     (message-header-to-face ((t (:bold t :foreground "#777"))))
     (message-header-xheader-face ((t (:foreground "#666"))))
     (message-separator-face ((t (:foreground "#999")))))))
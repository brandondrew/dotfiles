
(setq gnus-select-method '(nnnil ""))
(setq gnus-secondary-select-methods '((nnml ""
                                            (nnml-directory "~/Mail"))))

(setq gnus-home-directory "~/.emacs.d"
      gnus-dribble-directory "~/.emacs.d"
      gnus-always-read-dribble-file t)
(setq user-mail-address "phil@hagelb.org")
(setq user-full-name "Phil Hagelberg")
(setq gnus-ignored-from-addresses "Phil Hagelberg")

;; Now with SSL!
(setq smtpmail-starttls-credentials '(("mail.technomancy.us" 587 nil nil))
      smtpmail-smtp-server "mail.technomancy.us"
      smtpmail-default-smtp-server "mail.technomancy.us"
      send-mail-function 'smtpmail-send-it
      gnus-gcc-mark-as-read t
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-service 587
      starttls-extra-arguments '("--insecure")
      smtpmail-auth-credentials '(("mail.technomancy.us"
                                   587
                                   "send@technomancy.us" ;; throwaway send-only account
                                   "testyy")))

(setq message-kill-buffer-on-exit t)
(setq gnus-treat-display-smileys nil)
(setq gnus-message-archive-group "sent")
(setq gnus-fetch-old-headers 'some)
(setq nnmail-split-methods 'nnmail-split-fancy)
(setq nnmail-crosspost nil)
(setq mail-source-delete-incoming nil)
(setq gnus-asynchronous t)

(setq nnmail-split-fancy
      '(|
        ;; code
        (from "unfuddle" "unfuddle")
        (to "phil@sonian" "sonian")
        (to "ruby_emacs_dev@yahoogroups\\.com" "emacs-rails")
        (to "emacs-on-rails" "emacs-rails")
        (to "obby-users@list.0x539.de" "obby-users")
        (any "conkeror" "conkeror")
        (any "clojure" "clojure")
        (any "compojure" "compojure")
        (any "seafunc" "seafunc")
        (any "Nxhtml" "nxhtml")
        (any "mozlab" "mozlab")
        (any "emacs-rails" "emacs-rails")
        (any "gems.*forge" "gems")
        (to "rubygems-developers@rubyforge.org" "gems")
        (any "emacs-devel" "emacs-devel")
        (any "bus-scheme" "bus-scheme")
        (any "ruby-core" "ruby-core")
        (from "@amazon\.com" "amazon")
        (to "magit" "magit")
        (any "ert-devs" "ert")
        (any "solr" "solr")
        (any "couchapp" "couchapp")
        (any "katta" "katta")
        (any "tika" "tika")
        (any "zenspider\\.com" "seattle.rb")
        (from "github" "github")

        (to "phil@localhost" "feeds")

        ;; personal
        (from "agelberg" - "Alisha" "family")
        (from "Broach" "family")

        (any "David Morton" "xpoint")
        (any "Edward Volz" "xpoint")
        (any "John Gaitan" "xpoint")
        (from ".*crosspoint.*" "xpoint")
        (from "hudsonite" "xpoint")
        (to "parishgroup2@googlegroups\.com" "parish")
        (to "cplparishgroups@googlegroups\.com" "parish")

        (any "zacchaeus-bounces" junk)
        (any "zacchaeus.*" "friends")
        (any ".*hackelford.*" "friends")
        (any ".*peckham.*" "friends")
        (any ".*carroll.*" "friends")
        (any ".*guenther.*" "friends")
        (any ".*rowley.*" "friends")
        (any ".*malabuyo.*" "friends")
        (any ".*holloway.*" "friends")
        (any "Arko" "friends")
        (any "Joel Watson" "friends")

        ;; misc
        (from "VMWare" junk)
        (any "cron" junk)
        (any "Anacron" junk)
        (from "Inbox Archiver" junk)
        (any "Meridius" junk)
        (any "Paris Hilton" junk)
        (any "cartographer" junk)
        (from "Joanne Neumann" junk)
        (any "ocruby" junk)
        (any "CNN Alerts" junk)
        (any "ALM Expo 2008" junk)
        (from "Dianne Des Rochers" junk)
        "inbox"))

(setq mail-sources '((file :path "/var/mail/phil")))

(setq imap-ssl-program "openssl s_client -ssl3 -connect %s:%p")
(setq gnus-agent-expire-days 0)
(setq gnus-agent-enable-expiration 'DISABLE)

(gnus-demon-add-handler 'gnus-group-get-new-news 10 t)
(gnus-demon-init)

;; Unbind this key; it's annoying!
(define-key gnus-summary-mode-map "o" (lambda () (interactive)))

(add-hook 'message-mode-hook 'turn-on-flyspell)

(setq gnus-summary-line-format
      (concat
       "%{|%}"
       "%U%R%z"
       "%{|%}"
       "%(%-18,18f"
       "%{|%}"
       "%*%{%B%} %s%)"
       "\n"))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(setq gnus-use-full-window nil)

(mailcap-add "image/jpeg" "display")

(if (file-exists-p system-specific-config)
    (load system-specific-config))

(shell-command "fetchmail")
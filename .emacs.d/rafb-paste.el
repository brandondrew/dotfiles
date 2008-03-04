(require 'http-post)

(defgroup rafb-paste nil
  "paste regions of text to http://rafb.net/paste/"
                                        ; which parent group to use???
  :group 'processes)

(defcustom rafb-paste-nick "emacs user"
  "the default nick to be used for pasting"
  :group 'rafb-paste
  :type 'string)

(defcustom rafb-paste-description ""
  "the default description to be used for pasting"
  :group 'rafb-paste
  :type 'string)

(defcustom rafb-paste-paste-url "http://rafb.net/paste/paste.php"
  "the url to post the text to (do not change! why is it a option?)"
  :group 'rafb-paste
  :type 'string)

(defcustom rafb-paste-copy-url-to-clipboard t
  "when t copy the url to the clipboard"
  :group 'rafb-paste
  :type 'boolean)

(defcustom rafb-paste-copy-url-to-kill-ring nil
  "when t copy the url to the kill ring"
  :group 'rafb-paste
  :type 'boolean)

(defcustom rafb-paste-lang-alist
  '((c-mode . "C") (c++-mode . "C++"))
  "a alist that maps the major mode to the language string for rafb.net"
  :group 'rafb-paste
  :type '(alist :key-type (symbol :format "major mode: %v")
                :value-type (string :format "rafb language: %v")))

(defun rafb-paste-sentinel (proc msg)
                                        ; is this set-buffer neccessary?
  (set-buffer (process-buffer proc))
  (if (= http-status-code 302)
      (let ((loc (cdr (assoc "location" http-headers)))
            (begin-point (point)))
        (let ((url (concat "http://" http-host loc)))
          (message "pasted to `%s'" url)
          (when rafb-paste-copy-url-to-kill-ring
            (kill-new url))
          (when (and rafb-paste-copy-url-to-clipboard
                     (buffer-name (current-buffer)))
            (insert url)
            (clipboard-kill-ring-save begin-point (point)))))
      (message "unexpected http status code %i" http-status-code))
  (set-buffer-modified-p nil)
  (kill-buffer (current-buffer)))

(defun rafb-paste-nonempty-string-p (str)
  (if (zerop (length str))
      nil
      str))
        
(defun rafb-paste (beg end &optional desc nick)
  "Paste the current region to rafb.net/paste
The region BEG...END is sent to rafb.net and pasted.

The parameters DESC and NICK may overwrite the defaults rafb-paste-description
and rafb-paste-nick.

I have no idea what happens if an error occurs"
  (interactive "r\nsDescription: \nsNick: \n")
  (let ((arg-mod (if (interactive-p)
                     #'rafb-paste-nonempty-string-p
                     #'identity)))
    (let ((text (buffer-substring-no-properties beg end))
          (desc (or (funcall arg-mod desc) rafb-paste-description))
          (nick (or (funcall arg-mod nick) rafb-paste-nick))
          (lang (or (cdr (assoc major-mode rafb-paste-lang-alist))
                    "Plain Text")))
      (let ((proc
             (http-post rafb-paste-paste-url
                        `(("lang" . ,lang)
                          ("nick" . ,nick)
                          ("desc" . ,desc)
                          ("cvt_tabs" . "no")
                          ("text" . ,text))
                        'iso-8859-1
                        '(("Referer" . "http://rafb.net/paste/")
                          ("Connection" . "close"))
                        #'rafb-paste-sentinel)))))))

(provide 'rafb-paste)

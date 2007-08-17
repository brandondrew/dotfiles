;; cheat.el
;; Time-stamp: <2007-08-10 11:35:51 sjs>
;;
;; Provide a handy interface to cheat.
;; See http://cheat.errtheblog.com for details on cheat itself.

(defvar *cheat-host* "cheat.errtheblog.com")
(defvar *cheat-port* "80")
(defvar *cheat-uri* (concat *cheat-host* ":" *cheat-port*))

(defun cheat-command (&rest rest)
  "Run the cheat command with the given arguments."
  (interactive "sArguments for cheat: \n")
  (shell-command (mapconcat 'identity (push "cheat" rest) " ")))

;; TODO add support for --new to bypass the local cache for individual cheats
(defun cheat (name)
  "Show the specified cheat sheet."
  (interactive "sCheat name: \n")
  (cheat-command name))

(defun cheat-sheets ()
  "List all cheat sheets."
  (interactive)
  (cheat-command "sheets"))

(defun cheat-recent ()
  "Show recently added cheat sheets."
  (interactive)
  (cheat-command "recent"))

(defun cheat-clear-cache (&optional name)
  "Clear the local cheat cache, located in ~/.cheat. If a cheat name is given
only relead the one sheet."
  (interactive "sCheat name (hit return to clear them all): \n")
  (if name
      (cheat-command "--clear-cache" name)
      (cheat-command "--clear-cache")))

(defun cheat-versions (name)
  "Version history of the specified cheat sheet."
  (interactive "sCheat name: \n")
  (cheat-command name "--versions"))

;; TODO cheat-diff

(defun cheat-add-current-buffer (name)
  "Add a new cheat with the specified name and the current buffer as the body."
  (interactive "sCheat name: \n")
  (post-cheat name (buffer-string)))

;; TODO cheat-retrieve-for-editing

;; (defun cheat-edit-current-buffer (name)
;;   "Edit an existing cheat ")

(defun url-http-post (url args)
  "Send ARGS to URL as a POST request."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data
         (concat (mapconcat (lambda (arg)
                              (concat (url-hexify-string (car arg))
                                      "="
                                      (url-hexify-string (cdr arg))))
                            args
                            "&")
                 "\r\n")))
    ;; `kill-url-buffer'      to discard the result
    ;; `switch-to-url-buffer' to view the results (debugging).
    (url-retrieve url 'kill-url-buffer)))

(defun kill-url-buffer (status)
  "Kill the buffer returned by `url-retrieve'."
  (kill-buffer (current-buffer)))

(defun switch-to-url-buffer (status)
  "Switch to the buffer returned by `url-retreive'.
    The buffer contains the raw HTTP response sent by the server."
  (switch-to-buffer (current-buffer)))

(defun post-cheat (title body &optional new)
  (let ((uri (concat "http://" *cheat-uri* "/w/" (if new title ""))))
    (url-http-post uri `(("sheet_title" . ,title)
                         ("sheet_body" .  ,body)
                         ("from_gem" . "1")))))

(provide 'cheat)

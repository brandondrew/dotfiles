(require 'icalendar)

(defun technomancy-update-icalendar ()
  (interactive)
  ;; TODO: remove duplicates
  (url-retrieve (concat "https://zimbra.evri.com/home/" user-real-login-name "/Calendar")
		(lambda (status)
		  (replace-string "" "")
		  (beginning-of-buffer)
		  (kill-line 6) ;; remove HTTP headers
		  (icalendar-import-buffer "~/diary" t nil))))

(provide 'my-calendar)
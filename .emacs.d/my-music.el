;;; functions to work with mpd/mpc

(defun random-music ()
  (interactive)
  (unless (boundp 'music-dirs)
    (setq music-dirs (split-string (shell-command-to-string "find ~/music -type d | cut -c 18-") "\n")))
  (let ((dir (nth (random (length music-dirs)) music-dirs)))
    (shell-command (concat 
		    "mpc clear; "
		    "mpc add " (shell-quote-argument dir) " > /dev/null"))
    (message dir)))
    
(defun music-play-dir (dir)
  (interactive (list (completing-read "Play directory: " 
				      (split-string 
				       (shell-command-to-string "find ~/music -type d | cut -c 18-") "\n"))))
  (shell-command (concat 
		  "mpc clear; "
		  "mpc add " (shell-quote-argument dir)
		  "; mpc play > /dev/null")))

(defun music-add-file (file)
  (interactive (list (completing-read "Add file: " 
				      (split-string 
				       (shell-command-to-string "find ~/music -type f | cut -c 18-") "\n"))))
  (shell-command (shell-quote-argument (concat 
					"mpc add " file))))

(defun music-toggle () (interactive)
  (shell-command "mpc toggle"))

(defun music-next () (interactive)
  (shell-command "mpc next"))

(defun music-prev () (interactive)
  (shell-command "mpc prev"))

(provide 'my-music)
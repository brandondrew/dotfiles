;; Extra Bus Scheme specific tweaks to scheme-mode

(setq scheme-program-name "~/projects/bus_scheme/bin/bus")

(font-lock-add-keywords 'scheme-mode
			(list
			 (cons (mapconcat #'identity
					  '("\\(defresource"
					    "fail"
					    "send"
					    "load"
					    "cons"
					    "list"
					    "map"
					    "ruby"
					    "null\\?"
					    "not"
					    "car" "cdr" "cons"
					    "else"
					    "assert-equal"
					    "assert\\)")
					  "\\|")
			       'font-lock-function-name-face)))

(font-lock-add-keywords 'scheme-mode
			(list
			 (cons (mapconcat #'identity
					  '("\\(set!"
					    "xml"
					    "quote\\)")
					  "\\|")
			       'font-lock-keyword-face)))

(provide 'bus-scheme)
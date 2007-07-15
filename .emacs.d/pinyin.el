;; ā á ă à a
;; ē é ě è e
;; ī í ĭ ì i
;; ō ó ŏ ò o
;; ū ú ŭ ù u
;; ǖ ǘ ǚ ǜ ü

(defun macron-char (c)
  (case-string c
    ("a" "ā")
    ("e" "ē")
    ("i" "ī")
    ("o" "ō")
    ("u" "ū")
    (t c)))

(defun breve-char (c)
  (case-string c
    ("a" "ă")
    ("e" "ě")
    ("i" "ĭ")
    ("o" "ŏ")
    ("u" "ŭ")
    (t c)))

(defun macron-prev ()
  (interactive)
  (let ((char (buffer-substring (- (point) 1) (point))))
    (backward-delete-char 1)
    (insert (macron-char char))))

(defun breve-prev ()
  (interactive)
  (let ((char (buffer-substring (- (point) 1) (point))))
    (backward-delete-char 1)
    (insert (breve-char char))))

(global-set-key "\C-x_" 'macron-prev)
(global-set-key "\C-x(" 'breve-prev)

(provide 'pinyin)

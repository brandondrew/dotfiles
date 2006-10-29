;; ā á ǎ à a
;; ē é ě è e
;; ī í ǐ ì i
;; ō ó ǒ ò o
;; ū ú ǔ ù u
;; ǖ ǘ ǚ ǜ ü


(defun macron-char (c)
  (case-string c
    ("a" "ā")
    ("e" "ē")
    ("i" "ī")
    ("o" "ō")
    ("u" "ū")
    (t c)))

(defun caron-char (c)
  (case-string c
    ("a" "ǎ")
    ("e" "ě")
    ("i" "ǐ")
    ("o" "ǒ")
    ("u" "ǔ")
    (t c)))

(defun macron-prev ()
  (interactive)
  (let ((char (buffer-substring (- (point) 1) (point))))
    (backward-delete-char 1)
    (insert (macron-char char))))

(defun caron-prev ()
  (interactive)
  (let ((char (buffer-substring (- (point) 1) (point))))
    (backward-delete-char 1)
    (insert (caron-char char))))

(global-set-key "\C-x_" 'macron-prev)
(global-set-key "\C-x(" 'caron-prev)

(provide 'pinyin)
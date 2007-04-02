;;; -*- emacs-lisp -*-
;;; scrbook.el -- AUCTeX style for scrbook.cls

;; Copyright (C) 2002, 2005 Free Software Foundation
;; License: GPL, see the file COPYING in the base directory of AUCTeX

;; Author: Mark Trettin <Mark.Trettin@gmx.de>
;; Created: 2002-09-26
;; Version: $Id: scrbook.el,v 1.7 2005/06/05 18:32:07 angeli Exp $
;; Keywords: tex

;;; Commentary: 

;; This file adds support for `scrbook.cls'. This file needs
;; `scrbase.el'.

;; This file is part of AUCTeX.

;;; Code:
(TeX-add-style-hook "scrbook"
  (lambda ()
    (LaTeX-largest-level-set "chapter")
    ;; load basic definitons
    (TeX-run-style-hooks "scrbase")
    (TeX-add-symbols
     "chapapp"
     "raggeddictum"
     '("chapappifchapterprefix" "Additional text")
     '("setpartpreamble" [ TeX-arg-KOMA-setpreamble ] [ "Width" ] t)
     '("setchapterpreamble" [ TeX-arg-KOMA-setpreamble ] [ "Width" ] t)
     '("dictum" [ "Author" ] t))
    (LaTeX-section-list-add-locally '("addchap" 1))
    (make-local-variable 'LaTeX-section-label)
    (setq LaTeX-section-label (append
			       LaTeX-section-label
			       '(("addchap" . nil))))
    ;; Definitions for font-latex
    (when (and (featurep 'font-latex)
	       (eq TeX-install-font-lock 'font-latex-setup))
      ;; Textual keywords
      (setq font-latex-match-textual-keywords-local
	    (append font-latex-match-textual-keywords-local
		    '("addchap"
		      "setpartpreamble"
		      "setchapterpreamble"
		      "dictum")))
      (font-latex-match-textual-make)
      ;; Sectioning keywords
      (add-to-list 'font-latex-match-sectioning-1-keywords-local "addchap")
      (font-latex-match-sectioning-1-make))))

;;; scrbook.el ends here

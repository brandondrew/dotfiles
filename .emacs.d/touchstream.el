;;; touchstream.el --- Customizations for TouchStream Keyboards

;; Copyright (C) 2002 by Barnabas Wolf
;; Author: Barnabas Wolf <barnabas.wolf@msm.edu>
;; Keywords: extensions

;; Version: 0.7
;; Time-stamp: "02/11/06 22:09:22 westerma"
;; - mapped indent-region to Ctrl-Shift-J to go with new Ctrl-j lefthand twist

;; Version: 0.6
;; Time-stamp: "02/09/04 11:59:22 wolf"
;;  - switched to a minor-mode-map for the custom keymap instead of extending
;;    the global keymapusing inheritance.  (FSF Emacs had problems with
;;     ESC-prefix using the keymap inheritance)
;;  - added M-f4 mapping to close frame
;;  - changed transpose mappings to C-b, C-B, M-b, M-B
;;  - added C-s binding for save-buffer in isearch-mode.  

;; Version: 0.5
;; Time-stamp: "02/08/26 23:04:53 wolf"
;;  - changed transpose to Ctrl-b variants. (Ctrl-v was used by someone, and
;;    we've already re-mapped some Ctrl-f keys)
 
;; Version: 0.4
;; Time-stamp: "02/08/26 18:19:19 wolf"
;;  - updated documentation using Carl Stowen's suggestions and corrections
;;  - mapped transpose to Ctrl-v variants (Ctrl-Shift-T was unmappable)
;;  - added normal emacs bindings to C-insert, S-insert, and S-delete 

;; Version 0.3 
;; Time-stamp: "02/08/23 23:04:45 wolf"
;;  - fixed bug with keymap being initialized at the wrong time.
;;    Not all keymappings were active when it was initialized, so it shadowed
;;    some important key bindings.
;;  - added mappings for Transpose (Needs Ctrl-t + other modifiers to allow
;;    modifiers on the gesture (original mappings are C-t, M-t and such. 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public LicAense for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;; 
;;; TouchStream keyboards are large, multi-touch surfaces that recognize 
;;; multi-finger gestures as well as touch typing and pointing, all within the 
;;; same space and in a seamless manner.  The TouchStream recognition 
;;; engine translates gestures into keyboard and/or mouse event sequences.  
;;; TouchStreams have an 'Emacs' mode that provides simple lefthand 
;;; gestures for the main Emacs command prefixes (e.g. C-x, C-c, M-x, C-g): 
;;;
;;;     http://www.fingerworks.com/emacs_mode.html
;;;
;;; However, some customization of Emacs and XEmacs keyboard shortcuts 
;;; and behavior is necessary to take full advantage of the gesture capabilities. 
;;; (see http://www.fingerworks.com for general info. about the keyboards.)
;;;
;;; Installation: 
;;; 1. Copy this file and the additional packages you may have to download
;;;   into your site-lisp directory, or a personal directory (which should
;;;   be part of load-path).
;;; 2. Put the following two lines in your .emacs file: 
;;;   (require 'touchstream) 
;;;   (touchstream-mode t) 
;;; - at a later time, you can easily toggle touchstream mode on/off through
;;;   the modeline (button3 on the modeline) or with M-x touchstream-mode.
;;; 3. Make sure  Emacs & your X window manager are configured so that 
;;;     Left Alt is free to act as Meta key (most installations already accept 
;;;     Alt as a Meta key).
;;;
;;; Customization Goals:
;;;
;;; Ensure symmetric behavior for common repetitive functions:
;;;  - undo/redo 
;;;    uses the "redo" package in XEmacs, (nothing for GNU emacs yet)
;;;
;;;  - abbreviation completion search
;;;    TODO: dabbrev-expand works in one direction, but changing 
;;           directions resets the completion
;;;
;;; Provide compatible keymappings with other applications and environments
;;;  - file open/close shortcuts
;;;  - window minimize/close shortcuts
;;;  - search & replace shortcuts
;;;  - text selection using shift+movement; non-emacs copy/cut/paste shortcuts
;;;
;;; Provide convenient keyboard mappings for useful commands that are too long
;;;  - append copy/append cut
;;;    simple macro combines these operations, and is mapped to simple keys
;;;  
;;; Simplify keymappings for File, Clipboard, and Search command families
;;; so these gestures can be overloaded with opposite-hand Shift/Alt modifier 
;;; chords to invoke hierarchically related commands. For instance:
;;;   - Save gesture -> save-buffer
;;;   - Shift modifier chord + Save gesture -> write-file
;;;   - Open gesture -> find-file
;;;   - Shift modifier chord + Open gesture -> find-file-other-window
;;;   - Alt modifier chord + Cut gesture -> Append Cut
;;;   - Alt modifier chord + Copy gesture -> Append Copy
;;;   - Shift modifier chord + Search gesture -> RegExp Search
;;;   - Shift modifier chord + Replace gesture -> RegExp Replace
;;;   - Alt modifier chord + Search gesture -> Tags Search
;;;   - Alt modifier chord + Replace gesture -> Tags Query Replace
;;; (See     http://www.fingerworks.com/modifiers.html 
;;;   for a full explanation of modifier chords)
;;; 
;;;            
;;; Provide consistent behavior with other common gesture mappings
;;;  - PC-style text selection using Shift+movement keys 
;;;     - uses "pc-select" package, same in Emacs and XEmacs except for init
;;;
;;;  - cycle through buffers 
;;;      - using package "buffer-stack", mapped to M-left and M-right 
;;;
;;;  - variable name completion
;;;      - using dabbrev-expand, mapped to C-<, C->, C-S-space
;;;
;;; TODO
;;;  - make sure keymappings behave nice when emacs is running on a console
;;;  - get a redo package or write a redo macro for FSF EMacs.
;;;
;;; BUGS
;;;  - configuration options related to region behavior don't work on FSF Emacs
;;;  - for some reason, first time touchstream is enabled it does not turn off
;;;    pending-delete-mode as configured, but works on successive toggles.
;;;  - the keybinding for reverse tags-search is wrong.  What would work?
;;;  - variable completions don't work freely wrp direction of search.  
;;; 
;;; touchstream mode uses the following packages:
;;;
;;;  buffer-stack  http://www.emacswiki.org/cgi-bin/wiki.pl?BufferStack
;;;  pc-select     was included with my Emacs/XEmacs distributions...
;;;  redo          was included with my XEmacs distribution....
;;;  dabbrev       was included with my Emacs/XEmacs distributions...
;;;  cua           http://www.cua.dk/
;;;
;;; tested with XEmacs 21.1.14 and GNU Emacs 20.7.1  both under X
;;;


(provide 'touchstream)
  
; configuration and preferences 

(defgroup touchstream nil
  "Touchstream mode customization settings"
  :group 'keyboard)

(defcustom touchstream-modeline-string ""
  "String to display in the modeline when touchstream mode is active."
  :type 'string
  :group 'touchstream)

(defcustom touchstream-emacs-select-semantics t 
  "Controls whether tradition mark & move selection semantics are enabled.
Movement commands will disable the region when set to nil, region will be
resized following the cursor when set to t."
  :type 'boolean
  :group 'touchstream)

(defcustom touchstream-pending-delete-mode nil
  "Controls whether pending delete mode should be enabled.
When pending-delete-mode is active, any change to the buffer will erase the
contents of the active region.  Pending delete mode can be configured in its
own right, however the enabling pc-select-mode always enables pending delete.
as well.  We use this variable to have the last say and disable pending-delete
if the value of this variable is nil."
  :type 'boolean
  :group 'touchstream)

(defcustom touchstream-special-beginning-of-line t 
  "Controls whether touchstream mode should bind home to `touchstream-bol'.
touchstream-bol implements a `beginning-of-line' and `beginning-of-line-text' 
toggle on repeated executions.  If this behavior is not desired, or if the
macro conflicts with another package's home key binding, then disable this
feature by setting this variable to nil."
  :type 'boolean
  :group 'touchstream)


;;; internal variables

(defvar touchstream-map (make-sparse-keymap)
  "The keymap used when touchstream mode is on")

(defvar touchstream-original-keymap nil
  "The original keymap, restored if touchstream mode is disabled.")

(defvar touchstream-mode nil
  "Status of touchstream mode.
A value of t indicates touchstream shortcuts are enabled, nil means the 
original global-nap is in place.")

;;; convenience functions to modify keymap

(defvar touchstream-key-copy-list ()
  "Keys that should be remapped to different keys.
Copying these keys should only occur after touchstream-mode enabled all
packages that may define their own key bindings.  Otherwise, the 
`touchstream-map' would shadow all key definitions made by those packages.")

(defun touchstream-copy-key (old new)
  "Copy a keybinding from the original global map to touchstreams map.
Used to move key definitions around.  The key bindings are copied from the
global-map since we don't want to presume what functions are bound to every
key we might remap.  These remappings are kept in `touchstream-key-copy-list'
and actually mapped when the `touchstream-map' is initialized. This is 
necessary to capture key bindinds that are defined by other pacakges as 
part of touchstream's or any other initialization, but prior to touchstream
mide beging enabled. "
  (add-to-list 'touchstream-key-copy-list 
	       (cons old new)))

(defun touchstream-set-key (key sym)
  "Set a key binding in touchstream's keymap."
  (define-key touchstream-map key sym))

;;; main toggle function to enable/disable or toggle touchstream mode

(defun touchstream-mode (&optional arg)
  "Toggle touchstream mode.  
If ARG is given, turn mode ON with positive argument, OFF otherwise"
  (interactive "P")
  (setq touchstream-mode (if (null arg)
				(not touchstream-mode)
			      (> (prefix-numeric-value arg) 0)))
  (if touchstream-mode
      (progn
	; load packages
	(require 'buffer-stack)
	(cond 
	 ((string-match "XEmacs" emacs-version) 
          ; XEmacs specific initialization
	  (require 'redo)
	  (require 'pc-select)
	  (pc-select-mode t)
	  (setq pc-select-keep-regions touchstream-emacs-select-semantics))
	 (t 
          ;; FSF Emacs 
	  (require 'pc-select)
	  (pc-selection-mode)))
	  ;; cua is a very nice emacs package for pc-compatible selection 
	  ;; If you have it, you can replace pc-select with it.
          ;;(require 'cua)    
          ;;(CUA-mode 'emacs) 
                              
	(if touchstream-special-beginning-of-line
	    (touchstream-set-key [(home)] 'touchstream-bol)
	  (touchstream-copy-key  [(home)] [(home)]))
	(pending-delete-mode touchstream-pending-delete-mode)
	(touchstream-init-keymap)
	(or (assq 'touchstream-mode minor-mode-map-alist)
	    (setq minor-mode-map-alist (cons (cons 'touchstream-mode
						   touchstream-map)
					     minor-mode-map-alist))))
    ; touchstream disabled
    )
  (message "Touchstream mode %s." (if touchstream-mode "Enabled" "Disabled")))

;;; add a minor-mode indicator that shows when touchstream mode is on

(if (fboundp 'add-minor-mode)
    (add-minor-mode 'touchstream-mode 'touchstream-modeline-string
		    nil nil 'touchstream-mode)
  (or (assq 'touchstream-mode minor-mode-alist)
      (setq minor-mode-alist
	    (cons '(touchstream-mode touchstream-modeline-string) minor-mode-alist))))


;;;; keymap definitions
(defun touchstream-init-keymap ()
  (setq touchstream-original-keymap (current-global-map))

  (mapcar (lambda (bindings)
	    (let ((old (car bindings))
		  (new (cdr bindings)))
	      (define-key touchstream-map new 
		(lookup-key touchstream-original-keymap old))))
	  touchstream-key-copy-list))

;;; undo keys: perhaps C-z will be a problem for terminal programs, but is
; are there any other standard undo keys in shells or other console programs?
; At least suspend is not really harmful, just inconvenient.  Emacs can still
; be suspended using C-x C-z. 

(touchstream-set-key     [(control z)]           'undo)
(if (symbolp 'redo)
    (touchstream-set-key [(control Z)]           'redo))

;;; Open and Save file:  compatibility mapping to Ctrl-o and Ctrl-s.  
; we save the shadowed open-line Ctrl-o variants to equiv. enter variants.
; search commands from Ctrl-s variants will be re-mapped further below. 

(touchstream-copy-key    [(control o)]           [(control enter)])
(touchstream-copy-key    [(control O)]           [(control shift enter)])
(touchstream-set-key     [(control o)]           'find-file)
(touchstream-set-key     [(control O)]           'find-file-other-frame)

(touchstream-set-key     [(control s)]           'save-buffer)
(touchstream-set-key     [(control S)]           'write-file)
(touchstream-set-key     [(control f4)]          'kill-buffer)

;;; Search commands
;   Ctrl-f variants initiate search 
;   Ctrl-n repeat last search forward
;   Ctrl-p repeat last search backward
;   Ctrl-r variants initiate search & replace
; Gestures generate the Ctrl- keystrokes.  The following modifier keys can
; be used to choose between the different search functions:
;   none       normal search/replace
;   Shift-     regexp search/replace
;   Meta-      tags search/replace

(touchstream-set-key     [(control f)]           'isearch-forward)           
(touchstream-set-key     [(control n)]           'isearch-forward)           
(touchstream-set-key     [(control p)]           'isearch-backward)
(touchstream-set-key     [(control r)]           'query-replace) 

(touchstream-set-key     [(control F)]           'isearch-forward-regexp) 
(touchstream-set-key     [(control N)]           'isearch-forward-regexp) 
(touchstream-set-key     [(control P)]           'isearch-backward-regexp) 
(touchstream-set-key     [(control R)]           'query-replace-regexp) 

; backward and forward tags searches may be improperly mapped here...
(touchstream-set-key     [(control meta f)]      'tags-search) 
(touchstream-set-key     [(control meta n)]      'tags-loop-continue) 
(touchstream-set-key     [(control meta p)]      'find-tag) 
(touchstream-set-key     [(control meta r)]      'tags-query-replace) 

; isearch requires some customization to work with none default keys,
; since it uses its own keymap during a search.  These changes are *always*
; active, and not toggled with touchstream mode!  Luckly for us, the keys are
; we need are not used by isearch so there are no conflicts.

(define-key isearch-mode-map   [(control f)]     'isearch-repeat-forward) 
(define-key isearch-mode-map   [(control n)]     'isearch-repeat-forward)
(define-key isearch-mode-map   [(control p)]     'isearch-repeat-backward)

(define-key isearch-mode-map   [(control s)]     'save-buffer)

;; variations of Home/End:
;; The intent is to have the ensure behaviors:
;;   Home/End                  : beginning and end of line
;;   Shift Home/End            : beginning and end of line, marking region
;;   Control Home/End          : beginning and end of buffer
;;   Control Shift Home/End    : beginning and end of buffer, marking region
;;   Meta Home/End             : beginning and end of other buffer
;;
;;
;;  Note: I did not find any Emacs or XEmacs versions that didn't have
;;  these keys mapped properly.  Various versions of Emacs and the
;;  pc-select packages use different commands for these functions, so
;;  it may be counterproductive to try to redefine all these keys in
;;  all cases. If we find some version of emacs that needs these
;;  mappings adjusted manually, then we can put the code.
;;


(touchstream-set-key     [(home)]                'touchstream-bol)
(touchstream-set-key     [(end)]                 'end-of-line) 

; mouse scroll events
(touchstream-set-key     [(button4)]             '(lambda ()
						    (interactive)
						    (scroll-down 4)))
(touchstream-set-key     [(button5)]             '(lambda ()
						    (interactive)
						    (scroll-up 4)))

; text cut/copy/paste operations:
; we bind ctrl-insert, shift-insert, and shift-delete keys to normal
; emacs functions (equivalent of ctrl-w, alt-w and ctrl-y).
; (by default XEmacs uses these keys to interact with the OS clipboard)
;  TODO:
;  - wfind/rite macros that will handle both depending on the context.

(touchstream-set-key     [(shift insert)]        'yank)
(touchstream-set-key     [(control insert)]      'kill-ring-save)
(touchstream-set-key     [(meta control insert)] 'kill-ring-save-append)


; in addition we define a Meta- version of the copy & cut keys that append
; the selection to the last entry on the kill-ring.  

(touchstream-set-key     [(shift delete)]        'kill-region)
(touchstream-set-key     [(meta shift delete)]   'kill-region-append)
;(touchstream-set-key     [(meta W)]              'kill-ring-save-append)
;(touchstream-set-key     [(control W)]           'kill-region-append)

;; variable completions. 
(touchstream-set-key     [(control <)]           'dabbrev-expand)
(touchstream-set-key     [(control >)]           'dabbrev-expand-down)
(if (string-match "XEmacs" emacs-version)
    ; XEmacs requires "space" spelled out
    (touchstream-set-key [(control shift space)] 'dabbrev-expand-copy)
  ; Emacs does not understand "space", but rather needs "? ".
  (touchstream-set-key   [(control shift ? )]    'dabbrev-expand-copy))
    

;; buffer cycling
(touchstream-set-key     [(meta left)]           'buffer-stack-up)
(touchstream-set-key     [(meta right)]          'buffer-stack-down)

;; window operations
(touchstream-set-key     [(control f5)]          'iconify-frame)
(touchstream-set-key     [(meta f4)]             '(lambda () 
						    (interactive)
						    (delete-frame nil t)))
(touchstream-set-key     [(control f10)]         'other-window)

;; programming
(touchstream-set-key     [(control \;)]          'comment-region)

;; transpose
(touchstream-set-key     [(control b)]           'transpose-chars)
(touchstream-set-key     [(control B)]           'transpose-words)
(touchstream-set-key     [(meta b)]              'transpose-lines)
(touchstream-set-key     [(meta B)]              'transpose-sentences)

;; should justify (fill-paragraph-function), or indent-region go with Ctrl-j?
(touchstream-set-key     [(control J)]           'indent-region)

;; support  macros

(defun touchstream-bol (arg)
  "Jump to beginning of line or beginning of text on line.
Multiple executions toggle point location between begining-of-line and
beginning-of-line-text."  
  (interactive "p")
  (let ((bol (save-excursion (beginning-of-line arg) (point))))
    (if (/= (point) bol)
	(beginning-of-line arg)
      (beginning-of-line-text arg))))

(defun kill-region-append (beg end)
  "Kill region between point and mark, and append it to previous kill.  
The command \\[yank] can retrieve it from there.  See also the commands 
append-next-kill (\\[append-next-kill]) and kill-region (\\[kill-region])."
  (interactive "r")
  (append-next-kill)
  (kill-region beg end))

(defun kill-ring-save-append (beg end)
  "Append region to previous kill.  See also the commands
append-next-kill  (\\[append-next-kill])  and kill-ring-save \\[kill-ring-save])."
  (interactive "r")
  (append-next-kill)
  (kill-ring-save beg end))


(defun dabbrev-expand-down ()
  "start apprevistion expansion going down from point. 
THIS DOES NOT WORK as it is, because dabbrev-expand overloads the internal
direction flag that is set by the argument.  This will always fail after 
the first match.   dabbrev-expand  has to be fixed so it can always accept an 
argument."    
  (interatcive)
  (if (eq last-command 'dabbrev-expand)
      (dabbrev-expand)
    (dabbrev-expand -1)))



;; touchstream.el ends here





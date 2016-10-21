;;;	ctl-q-keys.el - key bindings starting with Ctl-q
;;;
;;;	Author:		Stefan Hundhammer <sh@suse.de>
;;;	Updated:	2001-17-01


(defvar ctl-q-map (make-keymap) "user defined keymap")

(define-key ctl-q-map "\C-SPC" 'set-mark-command)
(define-key ctl-q-map "\C-a" 'query-replace)
(define-key ctl-q-map "\C-b" 'auto-fill-mode)
(define-key ctl-q-map "\C-c" 'compile)
(define-key ctl-q-map "\C-d" 'dbx)
(define-key ctl-q-map "\C-e" nil)		; rewind-errors
(define-key ctl-q-map "\C-f" 'search-forward)
(define-key ctl-q-map "\C-g" 'goto-line)
(define-key ctl-q-map "\C-h" 'manual-entry)
(define-key ctl-q-map "\C-i" nil)		; toggle-ignore-case
(define-key ctl-q-map "\C-j" 'register-to-point)
(define-key ctl-q-map "\C-k" nil)
(define-key ctl-q-map "\C-l" 'what-line)
(define-key ctl-q-map "\C-m" 'point-to-register)
(define-key ctl-q-map "\C-n" 'next-error)
(define-key ctl-q-map "\C-o" 'overwrite-mode)
(define-key ctl-q-map "\C-p" 'fill-prefix)
(define-key ctl-q-map "\C-q" 'quoted-insert)
(define-key ctl-q-map "\C-r" 'shell)		; run
(define-key ctl-q-map "\C-s" 'shell)
(define-key ctl-q-map "\C-t" 'tabify-whole-buffer)
(define-key ctl-q-map "\C-u" 'untabify-whole-buffer)
(define-key ctl-q-map "\C-v" 'eval-current-buffer)
(define-key ctl-q-map "\C-w" 'kill-trailing-whitespace)
(define-key ctl-q-map "\C-x" nil)
(define-key ctl-q-map "\C-y" nil)
(define-key ctl-q-map "\C-z" 'delete-other-windows)

(define-key ctl-q-map " " 'set-mark-command)
(define-key ctl-q-map "a" 'query-replace)
(define-key ctl-q-map "b" 'auto-fill-mode)
(define-key ctl-q-map "c" 'compile)
(define-key ctl-q-map "d" 'dbx)
(define-key ctl-q-map "e" nil)			; rewind-errors
(define-key ctl-q-map "f" 'search-forward)
(define-key ctl-q-map "g" 'goto-line)
(define-key ctl-q-map "h" 'manual-entry)
(define-key ctl-q-map "i" nil)			; toggle-ignore-case
(define-key ctl-q-map "j" 'register-to-point)
(define-key ctl-q-map "k" nil)
(define-key ctl-q-map "l" 'what-line)
(define-key ctl-q-map "m" 'point-to-register)
(define-key ctl-q-map "n" 'next-error)
(define-key ctl-q-map "o" 'overwrite-mode)
(define-key ctl-q-map "p" 'fill-prefix)
(define-key ctl-q-map "q" 'quoted-insert)
(define-key ctl-q-map "r" 'shell)		; run
(define-key ctl-q-map "s" 'shell)
(define-key ctl-q-map "t" 'tabify-whole-buffer)
(define-key ctl-q-map "u" 'untabify-whole-buffer)
(define-key ctl-q-map "v" 'eval-current-buffer)
(define-key ctl-q-map "w" 'kill-trailing-whitespace)
(define-key ctl-q-map "x" nil)
(define-key ctl-q-map "y" nil)
(define-key ctl-q-map "z" 'delete-other-windows)

(define-key ctl-q-map "0" 'register-to-point-0)
(define-key ctl-q-map "1" 'register-to-point-1)
(define-key ctl-q-map "2" 'register-to-point-2)
(define-key ctl-q-map "3" 'register-to-point-3)
(define-key ctl-q-map "4" 'register-to-point-4)
(define-key ctl-q-map "5" 'register-to-point-5)
(define-key ctl-q-map "6" 'register-to-point-6)
(define-key ctl-q-map "7" 'register-to-point-7)
(define-key ctl-q-map "8" 'register-to-point-8)
(define-key ctl-q-map "9" 'register-to-point-9)

(define-key global-map "\C-q" ctl-q-map)



;;; <EOF>

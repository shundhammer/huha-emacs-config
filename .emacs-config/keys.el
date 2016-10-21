;;;	keys.el - general key bindings
;;;
;;;	Author:		Stefan Hundhammer <sh@suse.de>
;;;	Updated:	2001-01-30


(global-set-key [f1]		'dabbrev-expand		)
(global-set-key [f2]		'next-multiframe-window	)
(global-set-key [f3]		'iconify-frame		)
(global-set-key [f4]		'delete-frame		)
(global-set-key [f5]		'new-frame		)
(global-set-key [f6]		'rmail			)
(global-set-key [f7]		'replace-string		)
(global-set-key [f8]		'replace-regexp		)
(global-set-key [f9]		'undo			)
(global-set-key [f11]		'font-lock-fontify-buffer)
(global-set-key [f12]		'repeat-complex-command	)

(define-key emacs-lisp-mode-map "\177"	  'backward-delete-char)

(global-set-key "\M-p"	'insert-c-block			)
(global-set-key "\M-c"	'save-line-as-kill		)
(global-set-key "\M-j"	'justify-current-line		)
(global-set-key "\M-k"	'kill-entire-line		)
(global-set-key "\M-n"	'switch-to-buffer-other-frame	)
(global-set-key "\M-d"	'kill-word-and-whitespace	)
(global-set-key "\M-f"	'next-word			)
(global-set-key "\M-q"	'fill-paragraph			)
(global-set-key "\M-z"	'center-line			)
(global-set-key "\M-r"	'string-rectangle		)
(global-set-key "\M-s"	'save-all-buffers		)
(global-set-key "\M-n"	'new-frame			)
(global-set-key "\M-a"	'backward-sexp			)
(global-set-key "\M-e"	'forward-sexp			)
(global-set-key "\C-t"	'split-window-find-tag		)

(global-set-key [S-iso-lefttab]	'c-indent-exp		)
(global-set-key [(meta return)]	'electric-buffer-list	)


(global-set-key [help]		'help-for-help		)	; Help
(global-set-key [f24]		'minus			)	; NumKeypad -
(global-set-key [f25]		'slash			)	; NumKeypad /
(global-set-key [f26]		'asterisk		)	; NumKeypad *
(global-set-key [f29]		'scroll-down		)	; PageUp
(global-set-key [f35]		'scroll-up		)	; PageDown
(global-set-key [home]		'beginning-of-line	)	; Home
(global-set-key [f27]		'beginning-of-line	)	; Home
(global-set-key [end]		'end-of-line		)	; End
(global-set-key [f33]		'end-of-line		)	; End
(global-set-key [f31]		'recenter		)	; Num-5
(global-set-key [C-f27]		'beginning-of-buffer	)	; Ctrl-Home
(global-set-key [C-f33]		'end-of-buffer		)	; Ctrl-End
(global-set-key [C-left]	'backward-word		)	; Ctrl-Left
(global-set-key [C-right]	'forward-word		)	; Ctrl-Right
(global-set-key [C-f29]		'other-window		)	; Ctrl-PageUp
(global-set-key [C-f35]		'other-window		)	; Ctrl-PageDown
(global-set-key [C-f31]		'delete-other-windows	)	; Ctrl-Num-5
(global-set-key [C-down]	'scroll-up-line		)	; Ctrl-Down
(global-set-key [C-up]		'scroll-down-line	)	; Ctrl-Up
(global-set-key [S-f34]		'scroll-up-line		)	; Shift-Down
(global-set-key [S-f28]		'scroll-down-line	)	; Shift-Up
(global-set-key [S-f32]		'scroll-left-char	)	; Shift-Left
(global-set-key [S-f30]		'scroll-right-char	)	; Shitf-Right

(global-set-key [S-up]		'scroll-down-line	)	; Ctrl-Up
(global-set-key [S-down]	'scroll-up-line		)	; Ctrl-Down

(global-set-key [C-prior]	'other-window		)	; Ctrl-PageUp
(global-set-key [C-next]	'other-window		)	; Ctrl-PageDn
(global-set-key [C-home]	'beginning-of-buffer	)	; Ctrl-Home
(global-set-key [C-end]		'end-of-buffer		)	; Ctrl-End

(global-set-key [kp-5]		'recenter		)	; Num-5
(global-set-key [kp-begin]	'recenter		)	; Num-5
(global-set-key [C-kp-begin]	'delete-other-windows	)	; Ctrl-Num-5
(global-set-key [C-kp-prior]	'other-window		)	; Ctrl-Num-PageUp
(global-set-key [C-kp-next]	'other-window		)	; Ctrl-Num-PageDn
(global-set-key [C-kp-home]	'beginning-of-buffer	)	; Ctrl-Num-Home
(global-set-key [C-kp-end]	'end-of-buffer		)	; Ctrl-Num-End
(global-set-key [C-kp-down]	'scroll-up-line		)	; Ctrl-Num-Down
(global-set-key [C-kp-up]	'scroll-down-line	)	; Ctrl-Num-Up
(global-set-key [S-kp-down]	'scroll-up-line		)	; Shift-Num-Down
(global-set-key [S-kp-up]	'scroll-down-line	)	; Shift-Num-Up
(global-set-key [C-M-kp-left]	'scroll-left-char	)	; Shift-Num-Left
(global-set-key [C-M-kp-right]	'scroll-right-char	)	; Shift-Num-Right



(global-set-key [C-insert]	'yank			)	; Ctrl-Ins
(global-set-key [C-delete]	'kill-entire-line	)	; Ctrl-Del
(global-set-key [C-kp-add]	'enlarge-window		)	; Ctrl-Num+
(global-set-key [C-kp-subtract]	'shrink-window		)	; Ctrl-Num-



;;; <EOF>

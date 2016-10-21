;;;	dumb-keybaord.el -	some functions to get special U.S. characters
;;;				even on very dumb keyboards
;;;
;;;	Author:		Stefan Hundhammer <sh@suse.de>
;;;	Updated:	2001-17-01


(fset 'brace-left	"{")
(fset 'brace-right	"}")
(fset 'bracket-left	"[")
(fset 'bracket-right	"]")
(fset 'quote-left	"`")
(fset 'quote-right	"'")
(fset 'backslash	"\\")
(fset 'pipe-char	"|")
(fset 'at-sign		"@")
(fset 'tilde		"~")
(fset 'asterisk		"*")
(fset 'slash		"/")
(fset 'minus		"-")


(global-set-key "\M-1"	'pipe-char	)
(global-set-key "\M-2"	'at-sign	)
(global-set-key "\M-3"	'tilde		)
(global-set-key "\M-4"	'backslash	)
(global-set-key "\M-5"	'quote-right	)
(global-set-key "\M-6"	'quote-left	)
(global-set-key "\M-7"	'brace-left	)
(global-set-key "\M-8"	'bracket-left	)
(global-set-key "\M-9"	'bracket-right	)
(global-set-key "\M-0"	'brace-right	)
(global-set-key "\M--"	'backslash	)
(global-set-key "\M-#"	'quote-left	)


(define-key isearch-mode-map "\M-1"	'pipe-char	)
(define-key isearch-mode-map "\M-2"	'at-sign	)
(define-key isearch-mode-map "\M-3"	'tilde		)
(define-key isearch-mode-map "\M-4"	'backslash	)
(define-key isearch-mode-map "\M-5"	'quote-right	)
(define-key isearch-mode-map "\M-6"	'quote-left	)
(define-key isearch-mode-map "\M-7"	'brace-left	)
(define-key isearch-mode-map "\M-8"	'bracket-left	)
(define-key isearch-mode-map "\M-9"	'bracket-right	)
(define-key isearch-mode-map "\M-0"	'brace-right	)
(define-key isearch-mode-map "\M--"	'backslash	)
(define-key isearch-mode-map "\M-#"	'quote-left	)


;;; <EOF>

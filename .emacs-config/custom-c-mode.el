;;;	custom-c-mode.el - C/C++ editing mode customizations
;;;
;;;	Author:		Stefan Hundhammer <sh@suse.de>
;;;	Updated:	2008-06-27

(require 'cc-mode)

;;;	(load "cc-vars")
;;;	(load "cc-defs")
;;;	(load "cc-align")
;;;	(load "cc-cmds")
;;;	(load "cc-compat")
;;;	(load "cc-engine")
;;;	(load "cc-langs")
;;;	(load "cc-menus")
;;;	(load "cc-styles")
;;;	(load "cc-mode")


(setq auto-mode-alist
  (append '(("\\.cpp"	. c++-mode)
	    ("\\.h$"	. c++-mode)
	    ("\\.c$"	. c++-mode)
	    ("\\.ycp"	. ycp-mode))
	  auto-mode-alist))

(defconst sh-c-style
  '((c-tab-always-indent	. nil)
    (c-basic-offset		. 4)
    (c-electric-pound-behavior alignleft)
    (setq indent-tabs-mode nil)		; don't use tabs for indentation
    (c-offsets-alist .	(
			 (substatement-open	. 0)	; '{' after if, else, while, ...
			 (statement-block-intro	. +)	; first line after if(), ...
			 (case-label		. +)	; 'case XY:'
			 (statement-case-intro	. +)	; first line after 'case XY:'
			 (statement-case-open	. +)	; '{' after 'case XY:' (???)
			 (statement-cont	. +)	; statement continuation line
			 (label			. -1000) ; goto label
			 (arglist-intro		. c-lineup-arglist-intro-after-paren)	; ???
			 (arglist-close		. c-lineup-arglist)			; ???
			 )))
  "SH C/C++ Programming Style")

(defconst c-protection-key 
  (concat
   "\\<\\("
   "\\(public\\|protected\\|private\\|signal\\|slot\\)"
   "\\( *slot\\)"
   "\\)\\>"
   ))


;  (setq-default	c-site-default-style	"SH-C/C++")
;  (setq-default	c-default-style		"SH-C/C++")


(defun c-mode-usr-init ()
  "User defined startup function for C program editing mode."
  (c-initialize-builtin-style)
  (c-add-style "SH-C/C++" sh-c-style t)
  (c-set-style "SH-C/C++")
  (local-set-key	"\C-M"		'newline-and-indent)
  (local-set-key	"\177"		'backward-delete-char)
  (local-set-key	"\M-q"		'fill-paragraph)
  (local-set-key	"\C-\M-q"	'fill-paragraph)
  ;;;(setq paragraph-separate	"^$\\|^\\|^[^ 	]")
  )


(defun insert-c-block ()	; no args
  "Insert a C/C++ block: A pair of braces and an empty line to type into"
  (interactive)			; user can invoke cmd
  (beginning-of-line)
  (insert-char ?{ 1)
  (beginning-of-line)
  (c-indent-command)
  (end-of-line)
  (newline 1)
  (insert-char ?} 1)
  (beginning-of-line)
  (c-indent-command)
  (previous-line 1)
  (end-of-line)
  (newline-and-indent))


(setq		c-mode-common-hook	'c-mode-usr-init)

(define-key c-mode-map		"\C-M"	  	'newline-and-indent)
(define-key c-mode-map		"\177"	  	'backward-delete-char)
(define-key c-mode-map		[C-tab]	  	'c-indent-exp)
(define-key c-mode-map		[S-iso-lefttab]	'c-indent-exp)
(define-key c-mode-map		"\M-q"		'fill-paragraph)
(define-key c-mode-map		"\C-\M-q"	'fill-paragraph)

(define-key c++-mode-map	"\C-M"	  	'newline-and-indent)
(define-key c++-mode-map	"\177"	  	'backward-delete-char)
(define-key c++-mode-map	[C-tab]	  	'c-indent-exp)
(define-key c++-mode-map	"\M-q"	  	'fill-paragraph)
(define-key c++-mode-map	"\C-\M-q" 	'fill-paragraph)
(define-key c++-mode-map	"\M-a"    	'backward-sexp)
(define-key c++-mode-map	"\M-e"    	'forward-sexp)


;;; <EOF>

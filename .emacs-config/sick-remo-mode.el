;;;	sick-remo-mode.el -	Sick tab expansion mode for those who can't cope
;;;				with their beginning-70s vi editor
;;;
;;;	Author:		Stefan Hundhammer <sh@suse.de>
;;;	Updated:	2003-12-01


(if (not (memq 'untabify-whole-buffer write-file-hooks))
    (setq write-file-hooks
	  (cons 'untabify-whole-buffer write-file-hooks)))


;;; <EOF>

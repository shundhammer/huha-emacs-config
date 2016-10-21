;;;	mail.el - (Emacs) mail customization
;;;
;;;	Author:		Stefan Hundhammer <sh@suse.de>
;;;	Updated:	2001-17-01


;;;	Notice: all this is probably heavily outdated.

(setq mail-signature		t)
(setq mail-self-blind		t)
(setq rmail-file-name		"~/mail/default.mail")
; (setq rmail-spool-directory	"/var/spool/mail/")
(setq rmail-delete-after-output t)
(setq mail-yank-prefix		"> ")

(add-hook 'rmail-show-message-hook 'rmime-format)
(add-hook 'rmail-edit-mode-hook    'rmime-cancel)
(autoload 'rmime-format "rmime" "" nil)


;;; <EOF>

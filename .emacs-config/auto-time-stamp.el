;;;	auto-time-stamp.el -	automatically update a time stamp when saving a file.
;;;				See this file for an example file header.
;;;
;;;	Author:		Stefan Hundhammer <sh@suse.de>
;;;	Updated:	2001-01-17


(setq time-stamp-line-limit	40)
(setq time-stamp-start		"Updated:[ \t]+")
(setq time-stamp-end		"[ \t]\\|$")
(setq time-stamp-format		"%:y-%02m-%02d")

(autoload 'time-stamp "time-stamp" "Update the time stamp in a buffer." t)

(if (not (memq 'time-stamp write-file-hooks))
    (setq write-file-hooks
	  (cons 'time-stamp write-file-hooks)))


;;; <EOF>

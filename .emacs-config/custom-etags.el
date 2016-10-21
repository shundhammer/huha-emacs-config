;;;	custom-etags.el - accessing "etags" programming tags
;;;
;;;	Author:		Stefan Hundhammer <sh@suse.de>
;;;	Updated:	2001-17-01


(load "etags")

(defvar tag-win-height	 10 "*Default height of tags window")
(defvar tag-line-no	  1 "*Default line no of found tag in tag window")
(defvar tag-mark-always nil "*If t, mark found tag even if in current buffer")

(defun split-window-find-tag ()
  "Close all windows but the current, then open a new window at the top;
in this new window, display the tag near point."
  (interactive)
  (delete-other-windows)
  (let ((buf (current-buffer)))
    (split-window-vertically tag-win-height)
    (switch-to-buffer "*scratch*")
    (other-window 1)
    (recenter)
    (find-tag-other-window (find-tag-default))
    (recenter tag-line-no)
    (if (or
	 (not (eq buf (current-buffer)))
	 tag-mark-always)
	(progn	(beginning-of-line)
		(set-mark-command nil)
		(forward-line))))
  (other-window 1))


;;; <EOF>

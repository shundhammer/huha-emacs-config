;;;	edit-functions.el - general editing functions
;;;
;;;	Author:		Stefan Hundhammer <sh@suse.de>
;;;	Updated:	2006-10-20



(defun scroll-up-line ()
  "Scroll up window one line"
  (interactive)
  (scroll-up 1))

(defun scroll-down-line ()
  "Scroll down window one line"
  (interactive)
  (scroll-down 1))

(defun scroll-right-char ()
  "Scroll right window one character"
  (interactive)
  (scroll-right 1))

(defun scroll-left-char ()
  "Scroll left window one character"
  (interactive)
  (scroll-left 1))

(defun fill-and-justify-paragraph ()
  "Re-break lines in current paragraph and justify to fill-column"
  (interactive)
  (fill-paragraph 0))

(defun save-line-as-kill ()
  "Save the current line as if killed, but don't kill it."
  (interactive)
  (beginning-of-line)
  (let ((beg (point)))
    (forward-line 1)
    (copy-region-as-kill beg (point))
    (previous-line 1)))

(defun kill-entire-line ()
  "Kill the current line and save it in the kill ring"
  (interactive)
  (beginning-of-line)
  (let ((beg (point)))
    (forward-line 1)
    (kill-region beg (point))))

(defun next-word (arg)
  "Move forward arg words; stop at beginning of word"
  (interactive "p")
  (forward-word arg)
  (forward-word 1)
  (backward-word 1))

(defun save-all-buffers ()
  "Save all modified buffers without asking"
  (interactive)
  (save-some-buffers t ))

(defun tabify-whole-buffer ()
  "Replace blanks with tabs whereever possible"
  (interactive)
  (tabify (point-min) (point-max))
  (kill-trailing-whitespace))

(defun untabify-whole-buffer ()
  "Replace all tabs with blanks"
  (interactive)
  (untabify (point-min) (point-max)))

(defun kill-word-and-whitespace (arg)
  "Kill characters forward until encountering the end of a word.
Kill trailing whitespace. With argument, kill that many words."
  (interactive "p")
  (if (eolp)						; at end of line?
      (kill-line)					; -> kill newline
    (if (looking-at "[ \t]")				; whitespace?
	(kill-region (point)
		     (save-excursion
		       (skip-chars-forward " \t")	; -> kill whitespace
		       (point)))
      (save-restriction					; non-whitespace?
	(narrow-to-region				; stay on current line
	 (save-excursion (beginning-of-line) (point))
	 (save-excursion (end-of-line) (point)))
	(save-excursion
	  (kill-region (point)
		       (save-excursion
			 (forward-word arg)		; kill next word
			 (skip-chars-forward " \t")	; kill trailing
			 (point))))))))			;    whitespace

(defun kill-trailing-whitespace ()
  "Kill trailing whitespace (tabs, blanks) at the end of each line of
the current buffer."
  (interactive)
  (save-excursion
    (let ((old-buffer-size (buffer-size)))
      (goto-char (point-min))		; begin at buffer start
      (while (not (eobp))		; while not end of buffer
	(end-of-line)			; search of end of current line
	(kill-region			; delete everything
	 (save-excursion
	   (skip-chars-backward " \t")	; from the last non-whitespace on line
	   (point))
	 (point))			; until the original end of line
    (goto-char (+ 1 (point))))
      (let ((removed-whitespace-chars
	     (- old-buffer-size (buffer-size))))
	(if (> removed-whitespace-chars 0)
	    (message "Removed %d trailing whitespace character(s)."
		     removed-whitespace-chars))))))


;;; <EOF>

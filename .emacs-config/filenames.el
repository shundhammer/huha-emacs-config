;;;	filenames.el - replacement functions for file name creation:
;;;		'xyz~'	-> 'xyz.bak'
;;;		'#xyz#'	-> 'xzy.auto'
;;;
;;;	Author:		Stefan Hundhammer <sh@suse.de>
;;;	Updated:	2002-04-19
;;;
;;;	Those functions are stolen from the original files.el and slightly
;;;	modified. Just setting some variable will not do here: Emacs really
;;;	requires the functions to be reimplemented. 


(defun make-backup-file-name (file)
  "Create the non-numeric backup file name for FILE.
This is a separate function so you can redefine it for customization."
  (if (eq system-type 'ms-dos)
      (let ((fn (file-name-nondirectory file)))
	(concat (file-name-directory file)
		(if (string-match "\\([^.]*\\)\\(\\..*\\)?" fn)
		    (substring fn 0 (match-end 1)))
		".bak"))
    (concat file ".bak")))


(defun make-auto-save-file-name ()
  "Return file name to use for auto-saves of current buffer.
Does not consider `auto-save-visited-file-name' as that variable is checked
before calling this function.  You can redefine this for customization.
See also `auto-save-file-name-p'."
  (if buffer-file-name
      (concat (file-name-directory buffer-file-name)
	      ""
	      (file-name-nondirectory buffer-file-name)
	      ".auto")

    ;; Deal with buffers that don't have any associated files.  (Mail
    ;; mode tends to create a good number of these.)

    (let ((buffer-name (buffer-name))
	  (limit 0))
      ;; Use technique from Sebastian Kremer's auto-save
      ;; package to turn slashes into \\!.  This ensures that
      ;; the auto-save buffer name is unique.

      (while (string-match "[/\\]" buffer-name limit)
	(setq buffer-name (concat (substring buffer-name 0 (match-beginning 0))
			(if (string= (substring buffer-name
						(match-beginning 0)
						(match-end 0))
				     "/")
			    "\\!"
			  "\\\\")
			(substring buffer-name (match-end 0))))
	(setq limit (1+ (match-end 0))))

      (expand-file-name (format "%s.%s.auto" buffer-name (make-temp-name ""))))))

(defun auto-save-file-name-p (filename)
  "Return non-nil if FILENAME can be yielded by `make-auto-save-file-name'.
FILENAME should lack slashes.  You can redefine this for customization."
  (string-match "^.*\.auto$" filename))

(defun backup-file-name-p (file)
  "Return non-nil if FILE is a backup file name (numeric or not).
This is a separate function so you can redefine it for customization.
You may need to redefine `file-name-sans-versions' as well."
    (string-match "\\.bak$" file))


;; This is according to the doc, but it doesn't work:
(setq make-backup-file-name-function 'make-backup-file-name)


;; From Emacs 21.1 files.el:
;; Overwriting the make-backup-file-name function is fucked up big time.
;; This function seems to be what really counts.
;; Hacked after a whole afternoon of debugging :-((
;;
;; sh@suse.de 2002-04-19
(defun find-backup-file-name (fn)
  "Find a file name for a backup file FN, and suggestions for deletions.
Value is a list whose car is the name for the backup file
and whose cdr is a list of old versions to consider deleting now.
If the value is nil, don't make a backup.
Uses `backup-directory-alist' in the same way as does
`make-backup-file-name'.

This version is heavily hacked by sh@suse.de since the original doesn't
work at all as documented. Sigh."
  (let ((handler (find-file-name-handler fn 'find-backup-file-name)))
    ;; Run a handler for this function so that ange-ftp can refuse to do it.
    (if handler
	(funcall handler 'find-backup-file-name fn)
      (if (or (eq version-control 'never)
	      ;; We don't support numbered backups on plain MS-DOS
	      ;; when long file names are unavailable.
	      (and (eq system-type 'ms-dos)
		   (not (msdos-long-file-names))))
	  (list (make-backup-file-name fn))
	(let* ((basic-name (make-backup-file-name-1 fn))
	       (base-versions (concat (file-name-nondirectory basic-name)
				      ".bak"))
	       (backup-extract-version-start (length base-versions))
	       (high-water-mark 0)
	       (number-to-delete 0)
	       possibilities deserve-versions-p versions)
	  (condition-case ()
	      (setq possibilities (file-name-all-completions
				   base-versions
				   (file-name-directory basic-name))
		    versions (sort (mapcar #'backup-extract-version
					   possibilities)
				   #'<)
		    high-water-mark (apply 'max 0 versions)
		    deserve-versions-p (or version-control
					   (> high-water-mark 0))
		    number-to-delete (- (length versions)
					kept-old-versions
					kept-new-versions
					-1))
	    (file-error (setq possibilities nil)))
	  (if (not deserve-versions-p)
	      (list (concat basic-name ".bak"))
	    (cons (format "%s.~%d~" basic-name (1+ high-water-mark))
		  (if (and (> number-to-delete 0)
			   ;; Delete nothing if there is overflow
			   ;; in the number of versions to keep.
			   (>= (+ kept-new-versions kept-old-versions -1) 0))
		      (mapcar (lambda (n)
				(format "%s.~%d~" basic-name n))
			      (let ((v (nthcdr kept-old-versions versions)))
				(rplacd (nthcdr (1- number-to-delete) v) ())
				v))))))))))



;;; <EOF>

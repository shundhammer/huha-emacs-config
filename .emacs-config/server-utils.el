;;;	server-utils.el - Emacs server utilities
;;;
;;;	Author:		Stefan Hundhammer <sh@suse.de>
;;;	Updated:	2001-02-09


(require 'server)


(defun hostname-without-domain()
  "Return the system's hostname without the domain part."
  (interactive)
  (substring system-name 0 (string-match "\\." system-name)))


(defun server-start-if-not-yet-running()
  "Start the Emacs server if it is not yet running.
Use the \"emacsclient\" program to load files into an existing Emacs
rather than start a new one for each file."
  (interactive)
  (if
      (file-exists-p (format "/tmp/esrv%d-%s" (user-uid) (hostname-without-domain)))
      (message "Found Emacs server socket - starting standalone Emacs.")
    (progn
     (server-start)
     (message "Starting Emacs as server - use \"emacsclient\" from the shell to load files.")))
)


;;; <EOF>

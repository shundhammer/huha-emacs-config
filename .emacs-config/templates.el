;;;     templates.el - templates for tempo-mode
;;;
;;;     Author:         Stefan Hundhammer <sh@suse.de>
;;;     Updated:        2001-02-19

(require 'tempo)

(defun tempo-space ()
"Insert a template defined in ~/emacs-config/templates.el 
if a template name has been entered or a simple blank otherwise."
  (interactive "*")
  (or (tempo-expand-if-complete)
      (insert " ")))

(setq-default tempo-show-completion-buffer t)

; (global-set-key " "	'tempo-space)
; this breaks auto-rewrapping in auto-fill-mode :-(

(global-set-key [f2]	'tempo-expand-if-complete)


(set-default 'tempo-match-finder "\\W\\(--[^-]+--\\)\\=")
	;; regexp for template tags:
	;; a non-word character and --tagname-- just before point

;; Caution: The template tags (the name _after_ the contents) 
;; must be enclosed in "--" before and after, e.g. "--myname--"
;; - the name itself may not contain a dash "-", or the above regexp
;; will fail.

(tempo-define-template "yast2"	'(
"/*---------------------------------------------------------------------\\"	n
"|                                                                      |"	n
"|                      __   __    ____ _____ ____                      |"	n
"|                      \\ \\ / /_ _/ ___|_   _|___ \\                     |"	n
"|                       \\ V / _` \\___ \\ | |   __) |                    |"	n
"|                        | | (_| |___) || |  / __/                     |"	n
"|                        |_|\\__,_|____/ |_| |_____|                    |"	n
"|                                                                      |"	n
"|                               core system                            |"	n
"|                                                        (C) SuSE GmbH |"	n
"\\----------------------------------------------------------------------/"	n
n
"  File:       " p n
n
"  Author:     " p n
n
"/-*/" n
n
n
)
"--yast2--"
"File header for YaST2 C++ files"
)


(tempo-define-template "perl-sub" '(
n
"#-----------------------------------------------------------------------------" n
		n
"# " p		n
"#"		n
"# Parameters:"	n
"#\t" p		n
"# Return value:"	n
"#\t" p		n
		n
"sub " p"()"		n
"{"				n
"    my ( $param ) = @_;"	n
				n
"    " p			n
"}"				n
n
)
"--sub--"
"Perl subroutine"
)


;;; <EOF>

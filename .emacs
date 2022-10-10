;;;
;;;	.emacs - user specific emacs startup file
;;;
;;;	Remember to also copy all of ~/.emacs-config
;;;	when using this file!
;;;
;;;	Author:		Stefan Hundhammer
;;;	Updated:	2022-10-10
;;;

;; (setq debug-on-error t)

(load "~/.emacs-config/colors"		)	; font lock and general colors
(load "~/.emacs-config/filenames"	)	; .bak and .auto file name generation
(load "~/.emacs-config/edit-functions"	)	; some simple standalone edit functions
(load "~/.emacs-config/auto-time-stamp"	)	; automatic time stamp insertion
(load "~/.emacs-config/custom-c-mode"	)	; SH's custom C/C++ editing mode
(load "~/.emacs-config/custom-etags"	)	; convenience functions for tags handling
(load "~/.emacs-config/dumb-keyboard"	)	; get weird U.S. characters even on old keyboards
(load "~/.emacs-config/bookmarks"	)	; memorize and jump to locations in file(s)

(load "~/.emacs-config/ctl-q-keys"	)	; Ctl-Q key seqences   - after function definitions!
(load "~/.emacs-config/keys"		)	; general key bindings - after function definitions!
(load "~/.emacs-config/templates"	)	; programming templates (tempo-mode)
(load "~/.emacs-config/server-utils"	)	; utilities for emacs server (see below)

(load "~/.emacs-config/stolen/stig-paren"	)	; highlight matching parentheses
(load "~/.emacs-config/stolen/ke-spec"		)	; mode for spec files
(load "~/.emacs-config/stolen/htmlize"		)	; convert current buffer to HTML with syntax highlighting


(setq auto-mode-alist				; automatic edit modes depending on file extension
      (append
       '(
	 ("\\.po[tx]?\\'"	. po-mode)
	 ("\\.spec\\'"		. ke-spec-mode)
	 ("\\.spec.in\\'"	. ke-spec-mode)
	 )
       auto-mode-alist))


(setq compile-command				"make -k -j 8 && sudo make install")
;(setq compile-command				"make -j 12")
(setq compilation-window-height			20)
(set-default 'fill-column			79)	; automatic line wrapping
(setq blink-matching-paren			t)
(setq highlight-nonselected-windows		nil)
(setq search-highlight				t)	; highlight found match in isearch
(setq paren-sexp-mode				nil)	; highlight only matchin paren, not whole block

(transient-mark-mode				1)	; highlight block between mark and point
; (standard-display-european			1)	; enable Latin1 characters (obsolete?)
(line-number-mode				1)	; display line number in status line
(setq next-screen-context-lines	       		0)	; don't jump-scroll
(setq scroll-step		       		1)	; scroll 1 line at a time
(setq backup-by-copying-when-linked    		t)	; see C-h v
(setq backup-by-copying-when-mismatch  		t)	; see C-h v
(setq vc-make-backup-files	       		t)	; .bak even for files in CVS/RCS
(setq next-line-add-newlines			nil)	; no newlines by just scrolling down
(setq backward-delete-char-untabify-method	nil)	; delete tab chars normally
(setq font-lock-maximum-size			nil)	; no file size limit for syntax highlighting
(setq x-selection-timeout                       10)     ; prevent long delay when exiting


; Enable some seldom-used functions.

(put 'narrow-to-region		'disabled nil)
(put 'downcase-region		'disabled nil)
(put 'upcase-region		'disabled nil)
(put 'eval-expression		'disabled nil)


; Get rid of that icon tool bar
(tool-bar-mode -1)

; Always split window vertically upon M-x compile etc.
(setq split-height-threshold 0)
(setq split-width-threshold nil)


; Start the emacs server.
;
; From here on, you can use the \"emacsclient\" program to load files
; into an existing Emacs rather than start a new one for each file.
(server-start-if-not-yet-running)



; Automatically added by M-x customize :

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-delay 0)
 '(blink-cursor-interval 0.5)
 '(blink-cursor-mode nil)
 '(compilation-scroll-output t)
 '(completion-ignored-extensions
   '("CVS/" ".o" "~" ".bin" ".lbin" ".fasl" ".ufsl" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".dvi" ".fmt" ".tfm" ".pdf" ".class" ".fas" ".lib" ".x86f" ".sparcf" ".lo" ".la" ".toc" ".log" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".ybc"))
 '(delete-selection-mode nil nil (delsel))
 '(diff-switches "-u")
 '(global-cwarn-mode t nil (cwarn))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(make-backup-file-name-function 'make-backup-file-name)
 '(recenter-positions '(middle))
 '(safe-local-variable-values '((TeX-master . t) (TeX-master . "course")))
 '(scroll-bar-mode 'right)
 '(show-paren-mode t)
 '(tool-bar-mode nil nil (tool-bar))
 '(user-mail-address "Stefan.Hundhammer@gmx.de")
 '(vc-follow-symlinks nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 105 :width normal))))
 '(cursor ((t (:background "red"))))
 '(fringe ((((class color) (background light)) (:stipple nil :background "burlywood"))))
 '(makefile-space-face ((((class color)) (:background "cyan"))))
 '(menu ((t nil)))
 '(mode-line ((t (:background "sienna4" :foreground "burlywood1" :box (:line-width 2 :style released-button)))))
 '(scroll-bar ((t (:background "burlywood" :foreground "burlywood"))))
 '(sh-heredoc-face ((t (:foreground "slate blue"))))
 '(tool-bar ((((type x w32 mac) (class color)) (:background "burlywood" :foreground "black" :box (:line-width 1 :style released-button)))))
 '(trailing-whitespace ((((class color) (background light)) (:background "cyan")))))

;;;	colors.el - general and syntax highlighting (font lock) colors
;;;
;;;	Author:		Stefan Hundhammer <sh@suse.de>
;;;	Updated:	2021-04-13



; (set-background-color		"burlywood"	)
(set-foreground-color		"black"         )
; (set-cursor-color		"red"		)
; (set-mouse-color		"forest green"	)
;(set-face-background		'default		"burlywood"	)

(set-face-background		'region			"burlywood1"	)
;(set-face-background		'modeline		"sienna4"	)
;(set-face-foreground		'modeline		"burlywood1"	)

(defun font-lock-mode-usr-init ()
  "User defined startup function for font lock mode."
  (set-face-foreground 'font-lock-comment-face		"#006000"	)
  (set-face-foreground 'font-lock-string-face		"slate blue"	)
  ; (set-face-foreground 'font-lock-reference-face	"purple"	)
  (set-face-foreground 'font-lock-builtin-face		"purple"	)
  (set-face-foreground 'font-lock-constant-face		"purple"	)
  (set-face-foreground 'font-lock-variable-name-face	"firebrick"	)
  (set-face-foreground 'font-lock-function-name-face	"red3"		)
  (set-face-foreground 'font-lock-keyword-face		"chocolate3"	)
  (set-face-foreground 'font-lock-type-face		"maroon2"	)
  (set-face-foreground 'font-lock-warning-face		"red"		)
)

(add-hook 'find-file-hooks 'turn-on-font-lock		t)
(add-hook 'find-file-hooks 'font-lock-mode-usr-init	t)


;; set the printer equivalents for M-x ps-print-with-faces
;;
;; Make sure to add faces to several lists if you want several attributes applied,
;; e.g. bold italic -> add to ps-bold-faces and to ps-italic-faces.

(setq ps-print-color-p nil)	;; prevent color printing, use ps-faces instead

(setq ps-bold-faces	(quote (font-lock-builtin-face
				font-lock-reference-face
				font-lock-constant-face
				font-lock-variable-name-face
				font-lock-function-name-face
				font-lock-keyword-face
				font-lock-type-face
				font-lock-warning-face	)))

(setq ps-italic-faces	(quote (font-lock-comment-face
				font-lock-builtin-face
				font-lock-string-face )))

(setq ps-underlined-faces (quote (font-lock-warning-face)))


; That doesn't work (even though it should according to the doc):
;
(setq default-frame-alist
              '((width		. 80		)
        	(height		. 60		)
        	(background	. "burlywood"	)
        	(foreground	. "black"	)
        	(pointerColor	. "forest green")
        	(cursorColor	. "red"		)))
        
(setq initial-frame-alist
              '((width		. 80		)
        	(height		. 60		)
        	(background	. "burlywood"	)
        	(foreground	. "black"	)
        	(pointerColor	. "forest green")
        	(cursorColor	. "red"		)))
;        
;
; -> Use X resources instead in ~/.Xdefaults :
;
;	Emacs23*background:		burlywood
;	Emacs23.cursorColor:		red
;	Emacs23.pointerColor:		forest green
;
; The window class used to be "Emacs", but at some point the version number
; got added to that (ARGGGH!!!), so it's now "Emacs23", and it will change all
; the time. 
;
; May a thousand camels shit on the grave on whatever moron came up
; with that crazy idea.



;;; <EOF>

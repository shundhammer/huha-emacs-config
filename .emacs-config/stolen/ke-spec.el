;;; ke-spec.el --- major mode to edit spec files.

;; Copyright (C) 1997-1998, 2000, 2001 SuSE GmbH, Nuremberg, Germany.

(defconst ke-spec-version "$Revision: 1.26 $")
(defconst ke-spec-maintainer-address "Karl Eichwalder <ke@suse.de>")

;; Author: Karl Eichwalder <ke@suse.de>
;; Maintainer: Karl Eichwalder <ke@suse.de>

;; ke-spec.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2 of the License, or (at
;; your option) any later version.
 
;; ke-spec.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Thanks:
;; =======
;;
;; Bug reports and patches from Andreas Schwab <schwab@suse.de>, 2000.
;;
;; Outline settings from Reinhard Max <max@suse.de>, 1998.
;; 
;; Some ideas taken from `rpm-spec-mode.el',
;; Copyright (C) 1997 Stig Bjørlykke, <stigb@tihlde.hist.no>
;; Version:  0.03a

;; Installation:
;; =============
;;
;; To install, put this file somewhere GNU Emacs will find it (e.g., on
;; Linux under /usr/share/emacs/site-lisp), to speed up loading
;; byte-compile the file (M-x byte-compile-file).  Then add the
;; following lines to your .emacs file (without comment marks, of
;; course):
;;
;; (setq auto-mode-alist (append '(("\\.spec\\'" . ke-spec-mode))
;;                               auto-mode-alist))
;;
;; and load the file, e.g., with
;;
;; (require 'ke-spec)
;;
;; or better use the autoload feature:
;;
;; (autoload 'ke-spec-mode "ke-spec")
;;
;;
;; Features:
;; =========
;;
;; ke-spec-mode is base on sh-mode using the package `derived.el'.

;; 1. I don't know, how to make it inherit the font-lock keywords of
;; sh-mode (nevertheless, it might be okay this way).

;; It has its own font-lock support; do either
;;
;; (global-font-lock-mode 1)
;;
;; or at least
;;
;; (add-hook 'ke-spec-mode-hook 'turn-on-font-lock)
;;
;; to use font-lock with `ke-spec.el' it.
;;
;;
;; 2. A compile invocation via `rpm' is provided; each has an own history.
;;
;;
;; 3. `tempo' macros are available; I recommend to set
;;
;; (global-set-key [C-tab] 'tempo-forward-mark)
;; (global-set-key [S-C-tab] 'tempo-backward-mark)
;; (global-set-key [S-tab] 'tempo-backward-mark)
;;
;; to move easily from one "interesting point" to the next "interesting"
;; point.
;;
;;
;; Configuration Options:
;; ======================
;;
;; (setq ke-spec-build-new-buffer t) ; insert the template
;;                                   ; `ke-spec-new-buffer-template'
;;                                   ; in every new buffer automatically.
;;
;; (setq ke-spec-new-buffer-template
;;    ;; initial template for a new buffer.
;;    '("Summary: " (P "Package summary: ") n
;;      "Name: " (P "Package name: ") n
;;      "Version: " (P "Package version: ") n
;;      "Release: " (P "Package release: ") n
;;      "Copyright: " p "GPL" n
;;      "Group: " p n
;;      "Source0: " (P "Source file: ") n
;;      "Patch: " (P "Patch file: ") n 
;;      ;; other section may follow...
;;      ;; ...
;;      ;; ...
;;        ))
;;
;; and:
;;
;; ke-spec-rpm-command ke-spec-rpm-history
;; ke-spec-mode-hook
;;
;;
;; Miscellaneous:
;; ==============
;;
;; Use `ke-spec-submit-bug-report' to report bugs or to mail me feature
;; requests ;-)
;; 
;; Most features are accessible via menus.
;;
;; $Log: ke-spec.el,v $
;; Revision 1.26  2001/03/05 08:24:39  ke
;; - Add more licenses.
;; - Allow group tag editing for subpackages (reported by Reinhard Max).
;; - Try to provide missing font-lock faces for XEmacs (reported by Reinhard
;;   Max).
;;
;; Revision 1.25  2001/01/22 10:14:08  ke
;; new group tags from ~grimmer/public_html/Package-HOWTO/groups.txt
;;
;; Revision 1.24  2000/02/14 09:45:34  ke
;; fix group tags list
;;
;; Revision 1.23  2000/02/11 10:06:48  ke
;; source: add "%{version}"
;;
;; Revision 1.22  2000/02/10 16:01:28  ke
;; make %setup quiet
;;
;; Revision 1.21  2000/02/10 15:35:22  ke
;; remove whitespace after header keywords; the intended pretty printing
;; didn't happen ;-(
;;
;; Revision 1.20  2000/02/10 09:21:52  ke
;; adjust url tag whitespace.
;;
;; Revision 1.19  2000/02/09 13:08:23  ke
;; new `ke-spec-group-tags-list from' ~grimmer/public_html/export/Groups.txt
;;
;; Revision 1.18  2000/02/03 16:30:12  ke
;; modify %clean not to remove anything by default
;;
;; Revision 1.17  2000/01/24 14:27:06  ke
;; add %defattr(-, root, root) to the %files section
;;
;; Revision 1.16  2000/01/19 15:56:12  ke
;; typo ("neededforbuild"), reported by Andreas Schwab
;;
;; Revision 1.15  2000/01/13 16:18:18  ke
;; template improved
;;
;; Revision 1.14  2000/01/10 15:29:29  ke
;; suse_check call simplified (according to ro).
;;
;; Revision 1.13  2000/01/10 15:03:38  ke
;; init template fixed
;;
;; Revision 1.12  2000/01/10 14:59:26  ke
;; Now, `configure' and `xmkmf' are called in %build (instead of %prep).
;; Add suse_check macro.
;;
;; Revision 1.11  2000/01/10 14:34:44  ke
;; Improve templates: add missing \n's; remove $RPM_BUILD_ROOT before
;; installing files.
;;
;; Fix ke-spec-pkg-name for mising buffer-file-name (schwab@suse.de).
;;
;; Revision 1.10  2000/01/10 14:28:00  ke
;; Use use `C-c C-k' instead of `C-c C-t' as prefix keys to insert tempo
;; templates; `C-c C-t' is already taken by sh-mode (schwab@suse.de).
;;
;; `C-c C-k k' to invoke rpm; C-c LETTER isn't allowed for packages
;; (schwab@suse.de).
;;
;;
;; Revision 1.9  2000/01/09 06:40:19  ke
;; Some changes and major cleanups.
;;
;; Revision 1.8  1997/11/02 18:33:46  ke
;; add ke-spec-load-hook
;;
;; Revision 1.7  1997/11/01 17:40:15  ke
;; subpackage template
;;
;; Revision 1.6  1997/10/31 18:47:58  ke
;; insert an initial template automagically, if wanted.
;; improved documentation.
;;
;; Revision 1.5  1997/10/28 14:12:59  ke
;; *** empty log message ***
;;
;; Revision 1.4  1997/10/28 14:12:04  ke
;; Remove shell magic (work around an Emacs 19 behaviour, hack).
;;

(run-hooks 'ke-spec-pre-load-hook)

;; ;; just to make it work with Emacs < 20; please, upgrade!
;; (if (< emacs-major-version 20)
;;      (setq font-lock-builtin-face 'font-lock-keyword-face
;;            font-lock-warning-face 'font-lock-keyword-face))

(defvar ke-spec-new-buffer-template
  '("#neededforbuild " (P "Packages #neededforbuild: ") n
    "Summary: " p n
    ;; "Name: " p (let ((pkg (ke-spec-pkg-name)))
    ;;                   (read-from-minibuffer "Name: " '(pkg . 0))) n
    "Name: " p ke-spec-pkg n
    "Version: " p n
    "Release: " p "0" n
    "Group: " p (ke-spec-select-group)
    n
    p "#Requires: " p n
    p "#Provides: " p n
    "Packager: " p user-full-name p " <" user-mail-address ">" n
    "Copyright: " p (ke-spec-select-copyright)
    n
    "URL: " p n
    "Source0: " p "%{version}" n
    p "#Patch: " n
    p "#BuildRoot: /var/tmp/" p ke-spec-pkg "-root" n
    n
    "%description" n
    p ke-spec-pkg " (overridden with value from pac file)." n
    n
    "%define INSTALL install -m755 -s" n
    "%define INSTALL_SCRIPT install -m755" n
    "%define INSTALL_DIR install -d -m755" n
    "%define INSTALL_DATA install -m644" n
    n
    "%prep" n
    "%setup" p " -q -n $RPM_PACKAGE_NAME-$RPM_PACKAGE_VERSION" n
    p "# %patch" n
    n
    "%build" n
    p "CFLAGS=$RPM_OPT_FLAGS \\" n
    p "  ./configure --prefix=/usr %{_target_cpu}-suse-linux-gnu" n
    p "# xmkmf -a" n n
    p "make " p "\"CFLAGS=$RPM_OPT_FLAGS\" # LDFLAGS=-s" n
    n
    "%install" n
    "if [ ! \"x\" = \"x$RPM_BUILD_ROOT\" ] ; then" n
    "   rm -fr $RPM_BUILD_ROOT" n
    "   %{INSTALL_DIR} $RPM_BUILD_ROOT" n
    "fi" n
    "make install prefix=$RPM_BUILD_ROOT/usr" n
    p "# make install.man" n
    "%{?suse_check}" n
    n
    "%clean" n
    p "# rm -fr $RPM_BUILD_ROOT" n
    n
    "%files" n
    p "%defattr(-, root, root)" n
    p "%doc" p n
    n
    "# spec file ends here" n)
  "*Template for new SPEC file buffers.
Inserted by ke-spec-insert-new-buffer-strings automatically, if
`ke-spec-build-new-buffer' is non-nil.")

(defvar ke-spec-build-command "build"
  "*SuSE `build' command.")

(defvar ke-spec-build-history nil
  "*build commands (history).")

(defvar ke-spec-rpm-command "rpm -ba -v"
  "*RPM compile command.")

(defvar ke-spec-rpm-history '("rpm -bp -v"
                              "rpm -bc -v --short-circuit"
                              "rpm -bi -v --short-circuit"
                              "rpm -bl"        ; check list (%files)
                              "rpm -bc -v"
                              "rpm -bi -v"
                              "rpm -ba -v --test"
                              "rpm -ba -v")
  "*Often needed RPM commands (history).")

(defvar ke-spec-mode-hook nil
  "*List of hook functions run by `ke-spec-mode' (see `run-hooks').")

(defvar ke-spec-pre-load-hook nil
  "*Hook run before ke-spec-mode is loaded (see `run-hooks').")

(defvar ke-spec-load-hook nil
  "*Hook run when ke-spec-mode is loaded (see `run-hooks').")

(defvar ke-spec-build-new-buffer nil
  "*If not nil, then ke-spec will insert ke-spec-new-buffer-template
when new buffers are generated.")

(defvar ke-spec-indentation 2
  "*The width for indentation.")

(defvar ke-spec-outline-minor-mode nil
  "*Set to `t' to use Outline mode.")

(defvar ke-spec-outline-regexp
  "^%\\(description\\|prep\\|build\\|install\\|clean\\|files\\)"
  "*Outline REGEXP used in `ke-spec-mode'.")

(defvar ke-spec-copyright-tags-list
  '(("GPL")
    ("LGPL")
    ("GNU Free Documentation License (GFDL)")
    ("BSD License")
    ("LPPL LaTeX Public License")
    ("Public Domain, Freeware")
    ("Artistic License")
    ("X11/MIT")
    ("BSD License and BSD like")
    ("Freely Redistributable Software (FSR)")
    ("Beerware, Cardware, Shareware (not restricted)")
    ("Linux Documentation Project License (LDPL)")
    ("Sun Community License (SCL)")
    ("Mozilla Public License (MPL/NPL)")
    ("The Q Public License (QPL)")
    ("zlib/libpng License")
    ("Python Copyright")
    ("YaST Lizenz")
    ("IBM Public License")
    ("Contact author")
    ("Qt Free Edition Lizenz (Qt 1.x License)")
    ("Restricted Software")
    ("Commercial (all types)")
    ("No license agreement found in package")
    ("Collaborative Virtual Workspace License (CVW)")
    ("Ricoh Source Code Public License (RPL)"))
  "*List which elements are copyright tags.")

;; awk '{printf("(\"%s\")\n", $0)}' <  ~grimmer/public_html/Package-HOWTO/groups.txt
(defvar ke-spec-group-tags-list
  '(("Amusements/Games")
    ("Amusements/Graphics")
    ("Applications/Archiving")
    ("Applications/Clustering")
    ("Applications/Communications")
    ("Applications/Databases")
    ("Applications/Editors")
    ("Applications/Editors/Emacs")
    ("Applications/Emulators")
    ("Applications/Engineering")
    ("Applications/File")
    ("Applications/Graphics")
    ("Applications/Hamradio")
    ("Applications/Internet")
    ("Applications/Mail")
    ("Applications/Math")
    ("Applications/Multimedia")
    ("Applications/Networking")
    ("Applications/News")
    ("Applications/Productivity")
    ("Applications/Publishing")
    ("Applications/Publishing/TeX")
    ("Applications/Scientific")
    ("Applications/Sound")
    ("Applications/Spreadsheets")
    ("Applications/System")
    ("Applications/Text")
    ("Base")
    ("Base/Kernel")
    ("Beowulf/Scientific Computing")
    ("Development")
    ("Development/Building")
    ("Development/Debuggers")
    ("Development/Languages")
    ("Development/Languages/Fortran")
    ("Development/Languages/Java")
    ("Development/Languages/Perl")
    ("Development/Languages/Python")
    ("Development/Languages/Tcl")
    ("Development/Libraries")
    ("Development/Libraries/Hamradio")
    ("Development/Libraries/Parallel")
    ("Development/Libraries/Perl")
    ("Development/Libraries/Python")
    ("Development/Libraries/Tcl")
    ("Development/Libraries/X11")
    ("Development/System")
    ("Development/Tools")
    ("Development/Version Control")
    ("Development/X11")
    ("Documentation")
    ("Extensions/Arabic")
    ("Extensions/Chinese")
    ("Extensions/Japanese")
    ("Extensions/Korean")
    ("Networking/Admin")
    ("Networking/Daemons")
    ("Networking/Hamradio")
    ("Networking/News")
    ("Networking/Security")
    ("Networking/Utilities")
    ("SuSE internal")
    ("System Environment/Base")
    ("System Environment/Daemons")
    ("System Environment/Kernel")
    ("System Environment/Libraries")
    ("System Environment/Shells")
    ("System Environment/YaST")
    ("User Interface/Desktops")
    ("User Interface/Korean")
    ("User Interface/X")
    ("User Interface/X Hardware Support")
    ("Utilities/Archiving")
    ("Utilities/Console")
    ("Utilities/File")
    ("Utilities/Hamradio")
    ("Utilities/Printing")
    ("Utilities/Security")
    ("Utilities/System")
    ("Utilities/Terminal")
    ("Utilities/Text")
    ("X11/Amusements")
    ("X11/Applications")
    ("X11/Applications/Graphics")
    ("X11/Applications/Hamradio")
    ("X11/Applications/Internet")
    ("X11/Applications/Multimedia")
    ("X11/Applications/Networking")
    ("X11/Applications/Sound")
    ("X11/Backgrounds")
    ("X11/Fonts")
    ("X11/GNOME/Applications")
    ("X11/GNOME/Base")
    ("X11/GNOME/Development")
    ("X11/GNOME/Games")
    ("X11/GNOME/Graphics")
    ("X11/GNOME/Multimedia")
    ("X11/GNOME/Network")
    ("X11/GNOME/System")
    ("X11/GNOME/Utilities")
    ("X11/Games")
    ("X11/Games/Strategy")
    ("X11/Games/Video")
    ("X11/KDE/Applications")
    ("X11/KDE/Base")
    ("X11/KDE/Development")
    ("X11/KDE/Games")
    ("X11/KDE/Graphics")
    ("X11/KDE/IDE")
    ("X11/KDE/Math")
    ("X11/KDE/Multimedia")
    ("X11/KDE/Network")
    ("X11/KDE/Science")
    ("X11/KDE/Utilities")
    ("X11/KDE/i18n")
    ("X11/Libraries")
    ("X11/Terminals")
    ("X11/Utilities")
    ("X11/Window Managers")
    ("X11/XFree86")
    ("X11/XFree86/Servers"))
  "*List which elements are valid group tags.")

(require 'easymenu)
(require 'tempo)
(require 'reporter)

;; XEmacs
(if (string-match "XEmacs" emacs-version)
    (progn
      (if (not (boundp 'font-lock-builtin-face))
          (copy-face 'font-lock-preprocessor-face 'font-lock-builtin-face))
      (if (not (boundp 'font-lock-constant-face))
          (copy-face 'font-lock-type-face 'font-lock-constant-face))))

(defvar ke-spec-font-lock-keywords
  `(
    ;; Header lines
    ("^[A-Za-z]\\(\\w\\|_\\)*:[ \t]*" . font-lock-variable-name-face)
    ;; %description
    ("^%description" . font-lock-reference-face)
    ;; %define
    ("^%define" . font-lock-function-name-face)
    ;; macros: %{...}
    ("%{\\(\\w\\|_\\)+}" . font-lock-variable-name-face)
    ;; "%build" "%clean" "%install" "%prep"
    ("^%\\(build\\|clean\\|install\\|prep\\)\\s-*$" . font-lock-builtin-face)
    ;; "%files" "%pre" "%preun" "%post" "%postun" "%package"
    (,(concat
       "^%\\(files\\|p\\(ackage\\|ost\\(un\\)?\\|re\\(un\\)?\\)\\)"
       "\\(\\s-+\\(\\w\\|_\\)+\\s-*$\\)?") . font-lock-builtin-face)
    ;; "%attr" "%defattr"
    (,(concat
       "^%\\(attr\\|defattr\\)"
       "(.*)") . font-lock-constant-face)
    ;; '("%config" "%ghost" "%config(missingok)" "%ghost(noreplace)"
    ;;   "%config(noreplace)" "%ghost(missingok)")
    ("%\\(config\\|ghost\\)\\((\\(missingok\\|noreplace\\))\\)?\\s-+.*"
     . font-lock-warning-face)
    ;; "%dir" "%docdir" "%doc" "%patch" "%setup"
    ("^%\\(d\\(ir\\|oc\\(dir\\)?\\)\\|patch\\|setup\\).*"
     . font-lock-type-face)
    ;; still missing: special treatment for subpackages
    ;;
    )
  "*font-lock keywords for spec file mode.
To be improved...")

(defun ke-spec-compile ()
  "Compile with `rpm'."
  (interactive)
  ;; This `let' is for protecting the previous value of compile-command,
  ;; stolen from po-mode.el. -ke-
  (let ((compile-command (read-from-minibuffer
                          "RPM command without spec file (use history!): "
                          ke-spec-rpm-command
                          nil nil 'ke-spec-rpm-history)))
    (setq ke-spec-rpm-command compile-command)
    (compile (concat compile-command " " buffer-file-name))))

(defun ke-spec-build ()
  "Invoke SuSE `build'."
  (interactive)
  ;; This `let' is for protecting the previous value of compile-command,
  ;; stolen from po-mode.el. -ke-
  (let ((compile-command (read-from-minibuffer
                          "`build' command (use history!): "
                          ke-spec-build-command
                          nil nil 'ke-spec-build-history)))
    (setq ke-spec-build-command compile-command)
    (compile compile-command)))

(defun ke-spec-select-group ()
  (let ((completion-ignore-case t))
    (completing-read "Group (use TAB to select with completion): "
                     ke-spec-group-tags-list nil t)))

(defun ke-spec-select-copyright ()
  (let ((completion-ignore-case t))
    (completing-read "Copyright (edit with completion or RET): "
                     ke-spec-copyright-tags-list nil nil '("GPL" . 0))))

(defun ke-spec-search-and-replace-tag (tag otag)
  "Search for TAG and change its contents.
If TAG isn't found, add TAG after OTAG."
  (if (search-forward-regexp (concat "^" tag ":.*$") nil t)
      (replace-match "")
    (progn
      (search-forward-regexp (concat "^" otag ":.*$") nil t)
      (forward-line 1)
      (open-line 1))))

(defun ke-spec-add-or-change-group ()
  "Add or change Group tag."
  (interactive "*")
  (end-of-line)
  (if (re-search-backward "^%package" nil t)
      '()
    (goto-char (point-min)))
  (ke-spec-search-and-replace-tag "group" "summary")
  (insert "Group: ")
  (save-excursion
    (insert (ke-spec-select-group))))

(defun ke-spec-add-or-change-copyright ()
  "Add or change Copyright tag."
  (interactive "*")
  (goto-char (point-min))
  (ke-spec-search-and-replace-tag "copyright" "name")
  (insert "Copyright: ")
  (save-excursion
    (insert (ke-spec-select-copyright))))

(defun ke-spec-add-or-change-packager ()
  "Add or change Packager tag."
  (interactive "*")
  (goto-char (point-min))
  (ke-spec-search-and-replace-tag "packager" "name")
  (insert "Packager: ")
  (save-excursion
    (insert user-full-name " <" user-mail-address ">")))

(defun ke-spec-insert-new-buffer-strings ()
  "Insert ke-spec-new-buffer-strings."
  (interactive)
  (if (zerop (buffer-size))
      (tempo-template-ke-spec-skeleton)
    (message "Buffer not empty - no template inserted.")))

(defun ke-spec-pkg-name ()
  "Guess package name."
  (if buffer-file-name
      (let ((file (file-name-nondirectory buffer-file-name)))
        (string-match "[a-zA-Z0-9_]+[-.]" file)
        (substring file
                   0 (- (match-end 0) 1)))
    ""))

(defun ke-spec-delete-buildroot ()
  "Delete `BuildRoot' directory."
  (interactive)
  (let ((buildroot))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^buildroot:\\s-*/" 5000 t)
          (progn
            ;; (looking-at ".*$")
            ;; (setq buildroot
            ;;       (concat "/"
            ;;               (buffer-substring (match-beginning 0)
            ;;                                 (match-end 0))))
            ;; )))))
            (end-of-line)
            (setq buildroot
                  (buffer-substring (- (match-end 0) 1) (point)))
            ;; (message "%s" buildroot)
            (if (yes-or-no-p (concat "Remove directory `" buildroot "'? "))
                (shell-command (concat "rm -fr " buildroot))))
        (message "`BuildRoot' not set.")))))

;; (font-lock-add-keywords 'ke-spec-mode '("#neededforbuild"))

;;; Define macros with tempo.el

(defvar ke-spec-tempo-tags nil
  "List of tags used in completion.")

(tempo-define-template "ke-spec-skeleton" ke-spec-new-buffer-template
                       nil
                       "Insert a skeleton for a SPEC file.")
(tempo-define-template "description"
                       '("%description" n))
;; subpackage
(tempo-define-template "package"
                       ;; try to remember the name and use it later
                       '("%package -n " p n
                         "Version: " p n
                         "Release: " p "0" n
                         "Copyright: " p "GPL" n
                         "Summary: " p n
                         "Group: " p (ke-spec-select-group)
                         "%description -n " p n
                         p n))
(tempo-define-template "prep"
                       '("%prep" n
                         "%setup" p " -q -n $RPM_PACKAGE_NAME-$RPM_PACKAGE_VERSION" n
                         p "# %patch" n))
(tempo-define-template "build"
                       '("%build" n
                         p "CFLAGS=$RPM_OPT_FLAGS \\" n
                         p "  ./configure --prefix=/usr %{_target_cpu}-suse-linux-gnu" n
                         p "# xmkmf -a" n
                         p "make " p "\"CFLAGS=$RPM_OPT_FLAGS\" # LDFLAGS=-s" n))
(tempo-define-template "install"
                       '("%install" n
                         "if [ ! \"x\" = \"x$RPM_BUILD_ROOT\" ] ; then" n
                         "  rm -fr $RPM_BUILD_ROOT" n
                         "  %{INSTALL_DIR} $RPM_BUILD_ROOT" n
                         "fi" n
                         "make install prefix=$RPM_BUILD_ROOT/usr" n
                         p "# make install.man" n 
                         "%{?suse_check}" n))
(tempo-define-template "files"
                       '("%files" n
                         p "%defattr(-, root, root)" n
                         p "%doc" p n))
(tempo-define-template "clean"
                       '("%clean" n
                         p "# rm -fr $RPM_BUILD_ROOT" n))

(tempo-define-template "preamble"
                       '("#neededforbuild " p n
                         "Summary: " p n
                         "Name: " p ke-spec-pkg n
                         "Version: " p n
                         "Release: " p "0" n
                         "Copyright: " p (ke-spec-select-copyright)
                         n
                         "Group: " p (ke-spec-select-group)
                         n
                         "Source0: " p "%{name}-" p "%{version}" n
                         p "#Patch: " p n
                         p "#Provides: " p n
                         p "#Requires: " p n
                         "Packager: " p user-full-name " <" user-mail-address ">" n
                         "URL: " p n
                         p "BuildRoot: %{_tmppath}/%{name}-root" n
                         "%description" n
                         p n
                         n
                         "%define prefix /usr" n n
                         "%define INSTALL install -m755 -s" n
                         "%define INSTALL_DIR install -d -m755" n
                         "%define INSTALL_DATA install -m644" n))

;; ;; hack to work around an Emacs 19 annoyance
;; (defun ke-spec-e19-sh-check ()
;;   "Do Emacs 19 shell check."
;;   ;; (interactive)
;;   (let ((kill-whole-line t))
;;     (if (looking-at (concat ".*\\(" sh-shell-file "\\|/bin/bash\\)"))
;;         (kill-line))))
;; 
;; (if (< emacs-major-version 20)
;;     (add-hook 'ke-spec-mode-hook 'ke-spec-e19-sh-check))

(define-derived-mode ke-spec-mode sh-mode "Spec"
  "Major mode for editing spec files.
Run `ke-spec-mode-hook'.\n
\\{ke-spec-mode-map}"
  (set (make-local-variable 'font-lock-defaults)
      '(ke-spec-font-lock-keywords nil nil))
  (set (make-local-variable 'ke-spec-pkg)
       (ke-spec-pkg-name))
  (set (make-local-variable 'sh-indentation)
       ke-spec-indentation)
  (set (make-local-variable 'sh-shell-file)
       "/bin/sh")             ; rpm default
  (set (make-local-variable 'outline-regexp)
       ke-spec-outline-regexp)
  (if ke-spec-outline-minor-mode
      (outline-minor-mode))
  (set (make-local-variable 'require-final-newline)
       t)
  (easy-menu-add ke-spec-menu)
  (if ke-spec-build-new-buffer
      (ke-spec-insert-new-buffer-strings))
  (run-hooks 'ke-spec-mode-hook))

;;; Keybindings -- still a matter of change.
;; (unless ke-spec-mode-map
;;   (setq ke-spec-mode-map (make-sparse-keymap))
;;   (set-keymap-name ke-spec-mode-map 'ke-spec-mode-map)
(define-key ke-spec-mode-map "\C-c\C-kk" 'ke-spec-compile)
(define-key ke-spec-mode-map "\C-c\C-kc" 'tempo-template-clean)
(define-key ke-spec-mode-map "\C-c\C-kd" 'tempo-template-description)
(define-key ke-spec-mode-map "\C-c\C-kb" 'tempo-template-build)
(define-key ke-spec-mode-map "\C-c\C-ki" 'tempo-template-install)
;; (define-key ke-spec-mode-map "\C-c\C-kf" 'tempo-template-files)
(define-key ke-spec-mode-map "\C-c\C-kh" 'tempo-template-preamble)
(define-key ke-spec-mode-map "\C-c\C-kp" 'tempo-template-prep)
(define-key ke-spec-mode-map "\C-c\C-ks" 'tempo-template-package)
(define-key ke-spec-mode-map "\C-c\C-kn" 'ke-spec-insert-new-buffer-strings)

(define-key ke-spec-mode-map  "\C-c\C-ag" 'ke-spec-add-or-change-group)
(define-key ke-spec-mode-map  "\C-c\C-ac" 'ke-spec-add-or-change-copyright)
(define-key ke-spec-mode-map  "\C-c\C-ap" 'ke-spec-add-or-change-packager)
;; )


(easy-menu-define ke-spec-menu ke-spec-mode-map "Spec Menu"
                  '("Spec"
                    ["Insert Initial Template" ke-spec-insert-new-buffer-strings t]
                    "---"
                    ["Insert Preamble (header lines)" tempo-template-preamble t]
                    ["Insert %package Section (subpackage)" tempo-template-package t]
                    ["Insert %prep Section"     tempo-template-prep t]
                    ["Insert %build Section"    tempo-template-build t]
                    ["Insert %install Section"  tempo-template-install t]
                    ["Insert %clean Section"    tempo-template-clean t]
                    ;; ["Insert %files Section"    tempo-template-files t]
                    "---"
                    ["Add or Change `Group:' Tag..." ke-spec-add-or-change-group t]
                    ["Add or Change `Copyright:' Tag..." ke-spec-add-or-change-copyright t]
                    ["Add or Change `Packager:' Tag" ke-spec-add-or-change-packager t]
                    "---"
                    ["Invoke RPM..."           ke-spec-compile t]
                    ["Remove BuildRoot..."     ke-spec-delete-buildroot t]
                    ["Invoke SuSE build..."    ke-spec-build t]
                    "---"
                    ["Send Bug Report..."      ke-spec-submit-bug-report t]
                    ))

(defun ke-spec-submit-bug-report ()
  "Submit via mail a bug report on `ke-spec.el'."
  (interactive)
  (let ((reporter-prompt-for-summary-p
         "Subject (short - I'll add package name and version!): "))
    (reporter-submit-bug-report
     ke-spec-maintainer-address
     (concat "ke-spec.el " (substring ke-spec-version 11 -2))
     (list 'ke-spec-build-new-buffer
           'ke-spec-font-lock-keywords
           'ke-spec-mode-hook))))

(provide 'ke-spec)
(run-hooks 'ke-spec-load-hook)

;; ke-spec ends here

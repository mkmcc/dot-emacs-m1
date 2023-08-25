;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init.el -- runs the show
;;
;; set up the load path and `require' my various modules.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load path utilities
(defun mkmcc-add-subfolders-to-load-path (parent-dir &optional the-list)
  "Adds all first level `parent-dir' subdirs to a list.  Default
to the Emacs load path."
  (let ((mlist (if the-list the-list 'load-path )))
    (mkmcc-add-subfolders-to-list parent-dir mlist)))

(defun mkmcc-add-subfolders-to-list (parent-dir the-list)
  "Adds all first level `parent-dir' subdirs to a list."
  (dolist (file (directory-files parent-dir t "\\w+"))
    (when (file-directory-p file)
      (add-to-list the-list file))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load path
(defvar base-dir     "~/.emacs.d/")
(defvar elisp-dir    (expand-file-name "elisp"    base-dir))
(defvar vendor-dir   (expand-file-name "vendor"   base-dir))
(defvar themes-dir   (expand-file-name "themes"   base-dir))
(defvar snippets-dir (expand-file-name "snippets" base-dir))
(defvar savefile-dir (expand-file-name "savefile" base-dir))
(defvar personal-dir (expand-file-name "personal" base-dir)
  "All Emacs Lisp files here are loaded automatically.")

(unless (file-exists-p savefile-dir)    ; sometimes emacs is stupid...
  (make-directory savefile-dir))

(add-to-list 'load-path elisp-dir)
(add-to-list 'load-path vendor-dir)
(add-to-list 'load-path personal-dir)

(setq custom-theme-load-path nil)       ; nix the bundled themes

;; add the first level subfolders of themes and vendor
;;   this way I can keep things organized in subdirectories
(mkmcc-add-subfolders-to-load-path vendor-dir)
(mkmcc-add-subfolders-to-list themes-dir 'load-path) ;some themes load code
(mkmcc-add-subfolders-to-list themes-dir 'custom-theme-load-path)

(setq custom-file (expand-file-name "custom.el" personal-dir))
(setq custom-theme-directory themes-dir)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load modules
(require 'init-benchmarking)

;; packages
(require 'mkmcc-packages)               ; should come first!

;; core macros and function definitions
(require 'mkmcc-core)
(require 'mkmcc-file-defuns)
(require 'mkmcc-buffer-defuns)
(require 'mkmcc-editing-defuns)
(require 'mkmcc-misc)

;; interface
(require 'mkmcc-ui)
(require 'mkmcc-modeline)
(require 'mkmcc-editor)
(require 'mkmcc-isearch)
(require 'mkmcc-autocomplete)
(require 'mkmcc-ido)
(require 'mkmcc-dired)
(require 'mkmcc-global-keybindings)

;; specific file modes
(require 'prelude-programming)
(require 'mkmcc-c)
(require 'mkmcc-text)
(require 'mkmcc-latex)
;;(require 'prelude-python)
(require 'mkmcc-shell-script)
;(require 'mkmcc-gnuplot)
(require 'mkmcc-athena)
(require 'mkmcc-emacs-lisp)
;(require 'mkmcc-mathematica)

;; productivity
(require 'mkmcc-deft)
(require 'mkmcc-ediff)
;(require 'mkmcc-org)
;(require 'mkmcc-org-website)
;(require 'mkmcc-gdb)
(after-load 'paredit
 (require 'mkmcc-paredit))
(after-load 'grep
 (require 'mkmcc-rgrep))

;; other
;;(require 'mkmcc-erc)
;(require 'mkmcc-web)
;;(require 'mkmcc-mu4e)
;;(require 'mkmcc-weather)
(require 'mkmcc-shell)

;; load the personal settings (this includes `custom-file')
(when (file-exists-p personal-dir)
  (mapc 'load (directory-files personal-dir nil "^[^#].*el$")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; finish up

(load-file (expand-file-name "loaddefs.el" vendor-dir))

;; necessary to run gnuplot.  may not be the right solution?
;(setenv "DISPLAY" ":0")

(require 'dash)        ; can't `require' these in benchmark since they
(require 's)           ; may not be installed yet!
(message "%s" (benchmark/report-require-times))

;;; fin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Local Variables:
;;   no-byte-compile: t
;; End:

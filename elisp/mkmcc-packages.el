;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package manager
;;
(require 'cl-lib)                           ; can't use dash yet!
(require 'package)
(defvar base-dir)

;; repositories
;(add-to-list 'package-archives
;             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(setq package-user-dir (expand-file-name "elpa" base-dir))
(package-initialize)

;; automatically update or install some packages
(defvar mkmcc-packages
  '(org
    deft
    ;gnuplot-mode
    ;; bibslurp
    ;jumblr
    rainbow-delimiters
    rainbow-blocks
    flycheck
    expand-region
    smart-forward
    volatile-highlights
    diminish
    paredit
    elisp-slime-nav
    ;;ack-and-a-half
    ;gitconfig-mode
    ;gitignore-mode
    git-commit
    magit
    flx-ido
    ido-vertical-mode
    ;;ido-ubiquitous
    smex
    ;; yasnippet
    dash
    s
    ht
    loop
    define-word)
  "A list of packages to ensure are installed at launch.")

(defun mkmcc-packages-installed-p ()
  "Check if all packages in `mkmcc-packages' are installed."
  (cl-every #'package-installed-p mkmcc-packages))

(defun mkmcc-install-packages ()
  "Install all packages listed in `mkmcc-packages'."
  (unless (mkmcc-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (mapc #'package-install
          (remove-if #'package-installed-p mkmcc-packages))))

(mkmcc-install-packages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'mkmcc-packages)

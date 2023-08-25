;;; athinput-mode.el --- a major mode for editing athinput files

;; Copyright (c) 2013 Mike McCourt (mkmcc@berkeley.edu)
;;

;;; Commentary:

;; A major mode for editing athena input files.  Provides basic
;; indentation, syntax highlighting, and some other nice features.

;; Note that this mode aims to leave you with no control whatsoever
;; over your whitespace.  If that strikes you like too much bondage
;; and discipline, remember what a pain it is to keep input files
;; under version control -- with this mode you will never again have
;; to deal with annoying "whitespace-only" commits.
;;
;; And you don't *really* want to spent time formatting your input
;; files manually, do you?

;;; TODO:
;; 1. maybe make athena-align-*-equals ignore comments?
;;    - it's not clear whether the current behavior is good or bad --
;;      most comments with = in them are just commented-out lines,
;;      which I typically *do* want aligned.

;;; Code:

(require 'conf-mode)

(defcustom athinput-mode-hook nil
  "Hook to run upon entering `athinput-mode'."
  :type  'hook
  :group 'athinput)

(defun athena-indent-line ()
  "indent the current line"
  (let ((old-point (point-marker)))
    (beginning-of-line)
    (delete-horizontal-space)
    (goto-char (marker-position old-point))))

(defun athena-align-from-equals ()
  "align things to the right of the equals sign.  minus signs
'hang' into the left margin"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "=\\(\\s-*\\)[^-]" nil t)
      (replace-match "  " t t nil 1))
    (goto-char (point-min))
    (while (re-search-forward "=\\(\\s-*\\)-" nil t)
      (replace-match " " t t nil 1))))

(defun athena-align-to-equals ()
  "align parameter definitions with each block"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "<\\sw+>\\([^<]+\\)" nil t)
      (let ((beg (match-beginning 1))
            (end (match-end 1)))
        ;; the below means "find one or more spaces before an equals
        ;; sign and pad that sub-expression so that the equals signs
        ;; line up."
        (align-regexp beg end "\\(\\s-*\\) =" 1 1)))))

(defun athena-add-par-end ()
  "automatically add the block <par_end> if it isn't present"
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^\\s-*<par_end>" nil t)
      (goto-char (point-max))
      (skip-chars-backward " \t\n\t\f\v")
      (delete-region (point) (point-max))
      (insert "\n\n<par_end>\n"))))

(defun athena-before-save-hook ()
  (athena-add-par-end)
  (indent-region (point-min) (point-max))
  (athena-align-to-equals)
  (athena-align-from-equals)
  (whitespace-cleanup))

(defvar athena-mode-syntax-table
  (let ((table (make-syntax-table conf-unix-mode-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    table)
  "syntax table to use in athinput buffers")

(defvar athena-mode-font-lock-keywords
  `(
    ;; blocks
    ("^\\(<\\)\\(\\sw+\\)\\(>\\)"
     (2 'font-lock-type-face))
    ;; numbers
    (,(format "=\\s-*%s\\s-*"
              (regexp-opt '("yes" "no" "true" "false" "on" "off") 'words))
     (1 'font-lock-constant-face))
    ("=\\s-*\\([0-9\\.eE-]+\\)\\s-*"
     (1 'font-lock-constant-face))
    ;; parameter definitions
    ("^\\s-*\\(\\b\\w+\\b\\)\\s-*=\\s-*\\(.*\\)"
     (1 'font-lock-keyword-face)
     (2 'font-lock-string-face)))
  "font lock extensions for athinput files")

;;;###autoload
(define-derived-mode athinput-mode conf-unix-mode "athinput"
  "A major mode for editing athinput files."
  (conf-mode-initialize "#" athena-mode-font-lock-keywords)
  (setq indent-tabs-mode nil)
  (set (make-local-variable 'indent-line-function)
       'athena-indent-line)
  (set-syntax-table athena-mode-syntax-table)
  (add-hook 'before-save-hook 'athena-before-save-hook nil t)
  (run-hooks 'athinput-mode-hook))

;;;###autoload
(dolist (pattern '("athinput\\'" "athinput\\.*\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'athinput-mode)))

(provide 'athinput-mode)

;;; athinput-mode.el ends here

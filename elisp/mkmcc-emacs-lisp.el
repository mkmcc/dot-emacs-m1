;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs lisp
;;
(require 'prelude-lisp)

(defun prelude-recompile-elc-on-save ()
  "Recompile your elc when saving an elisp file."
  (add-hook 'after-save-hook
            (lambda ()
              (when (file-exists-p (byte-compile-dest-file buffer-file-name))
                (emacs-lisp-byte-compile)))
            nil
            t))

(defun prelude-visit-ielm-buffer ()
  "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
  (interactive)
  (prelude-start-or-switch-to 'ielm "*ielm*"))

(defun mkmcc-emacs-lisp-mode-hook ()
  "defaults for `emacs-lisp-mode'."
  (prelude-lisp-coding-hook)
  (turn-on-eldoc-mode)
  (rainbow-delimiters-mode +1)
  (prelude-recompile-elc-on-save)
  ; doc checker is super annoying
  (setq-local flycheck-checkers '(emacs-lisp)))

(add-hook 'emacs-lisp-mode-hook 'mkmcc-emacs-lisp-mode-hook)


;; ielm is an interactive Emacs Lisp shell
(defun mkmcc-ielm-mode-hook ()
  "defaults for `ielm'."
  (prelude-interactive-lisp-coding-hook)
  (turn-on-eldoc-mode))

(add-hook 'ielm-mode-hook 'mkmcc-ielm-mode-hook)

(after-load 'elisp-slime-nav (diminish 'elisp-slime-nav-mode))
(after-load 'rainbow-delimiters-mode (diminish 'rainbow-delimiters-mode))
(after-load 'eldoc (diminish 'eldoc-mode))

;; enable elisp-slime-nav-mode
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-emacs-lisp)

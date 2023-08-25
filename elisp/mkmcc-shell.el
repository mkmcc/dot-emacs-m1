;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell
;;
(defvar base-dir)
(defvar shell-mode-map)
(defvar eshell-mode-map)

(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(after-load 'shell
  (define-key shell-mode-map
    (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eshell
(defvar eshell-directory-name (expand-file-name "eshell" base-dir))

(defvar eshell-where-to-jump 'begin)    ;TODO: after breaks em-smart...
(defvar eshell-review-quick-commands nil)
(defvar eshell-smart-space-goes-to-end t) ;TODO: think about this
(defvar eshell-history-size 512)
(defvar eshell-hist-ignoredups t)

(defvar eshell-prompt-function
  (lambda ()
    (concat (file-name-nondirectory (eshell/pwd))
            (if (= (user-uid) 0) " # " " $ "))))

(defvar eshell-prompt-regexp "^[^#$\n]* [#$] ")


(after-load 'eshell
  (require 'em-smart))

(defun mkmcc-eshell-mode-hook ()
  "my hook for eshell-mode"
  ;; can't put this in after-load (as above) because eshell-mode-map
  ;; is a buffer-local variable.
  (define-key eshell-mode-map
    (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)
  (eshell-smart-initialize))

(add-hook 'eshell-mode-hook 'mkmcc-eshell-mode-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-shell)

(require 'paredit)
(require 'dash)
(require 's)

(defun paredit-wrap-round-from-behind ()
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-round)
  (insert " ")
  (forward-char -1))

(defun paredit-wrap-square-from-behind ()
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-square))

(defun paredit-wrap-curly-from-behind ()
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-curly))

(defun paredit-kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (paredit-backward-kill-word)))

(define-key paredit-mode-map (kbd "M-(") 'paredit-wrap-round)
(define-key paredit-mode-map (kbd "M-)") 'paredit-wrap-round-from-behind)
(define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
(define-key paredit-mode-map (kbd "M-]") 'paredit-wrap-square-from-behind)
(define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)
(define-key paredit-mode-map (kbd "M-}") 'paredit-wrap-curly-from-behind)

(define-key paredit-mode-map (kbd "C-w") 'paredit-kill-region-or-backward-word)

;; ;; Change nasty paredit keybindings
;; (defvar my-nasty-paredit-keybindings-remappings
;;   '(("M-s"         "s-s"         paredit-splice-sexp)
;;     ("M-<up>"      "s-<up>"      paredit-splice-sexp-killing-backward)
;;     ("M-<down>"    "s-<down>"    paredit-splice-sexp-killing-forward)
;;     ("C-<right>"   "s-<right>"   paredit-forward-slurp-sexp)
;;     ("C-<left>"    "s-<left>"    paredit-forward-barf-sexp)
;;     ("C-M-<left>"  "s-S-<left>"  paredit-backward-slurp-sexp)
;;     ("C-M-<right>" "s-S-<right>" paredit-backward-barf-sexp)))

;; (define-key paredit-mode-map (kbd "s-r") 'paredit-raise-sexp)

;; (--each my-nasty-paredit-keybindings-remappings
;;   (let ((original (car it))
;;         (replacement (cadr it))
;;         (command (car (last it))))
;;     (define-key paredit-mode-map (read-kbd-macro original) nil)
;;     (define-key paredit-mode-map (read-kbd-macro replacement) command)))

;; don't hijack \ please
(define-key paredit-mode-map (kbd "\\") nil)

;; use paredit in the minibuffer!
(defun prelude-conditionally-enable-paredit-mode ()
  "Enable `paredit-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (paredit-mode 1)))

(add-hook 'minibuffer-setup-hook 'prelude-conditionally-enable-paredit-mode)


;; making paredit work with delete-selection-mode
(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-open-round 'delete-selection t)
(put 'paredit-open-square 'delete-selection t)
(put 'paredit-doublequote 'delete-selection t)
(put 'paredit-newline 'delete-selection t)


(diminish 'paredit-mode " π")

(provide 'mkmcc-paredit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customizing ido mode
;;
(require 'dash)
(defvar savefile-dir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fix some things before starting ido

;; set the file permissions on the ido.last file.  This is a potential
;; security concern.
;;; NB: recentf and saveplace already do this.
(defvar ido-save-file-modes 384
  "Mode bits for the ido save file.  Integer or nil.  If non-nil,
set the mode bits to that value.  By default give R/W access only
to the owner of the file.  See the function `set-file-modes'.")

(defadvice ido-save-history (after ido-set-file-mode)
  "Set the permissions bit for the .ido.last file"
  (when ido-save-file-modes
    (set-file-modes ido-save-directory-list-file ido-save-file-modes)))

(ad-activate 'ido-save-history)

;; ~ takes me home
(defvar ido-file-completion-map)
(defun mkmcc-ido-setup ()
  "sensible defaults for ido."
  ;; Go straight home
  (define-key ido-file-completion-map
    (kbd "~")
    (λ (if (looking-back "/")
           (insert "~/")
         (call-interactively 'self-insert-command)))))

(add-hook 'ido-setup-hook 'mkmcc-ido-setup)

;; more sensible defaults
(defvar ido-case-fold                 t)
(defvar ido-enable-flex-matching      t)
(defvar ido-create-new-buffer        'prompt)
(defvar ido-max-prospects             6)
(defvar ido-confirm-unique-completion t)

(defvar ido-enable-last-directory-history t)
(defvar ido-use-url-at-point              nil)

(defvar ido-save-directory-list-file (expand-file-name "ido.last" savefile-dir))
(defvar ido-ignore-buffers
  '( "\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido"
     "*compilation*" "*gnuplot errors*" ))

(defvar completion-ignore-case t)
(defvar read-file-name-completion-ignore-case t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; now fire up ido
(ido-mode 'both)
(ido-everywhere 1)
;; (ido-ubiquitous-mode)

;; ;; Fix ido-ubiquitous for newer packages
;; (defmacro ido-ubiquitous-use-new-completing-read (cmd package)
;;   `(eval-after-load ,package
;;      '(defadvice ,cmd (around ido-ubiquitous-new activate)
;;         (let ((ido-ubiquitous-enable-compatibility nil))
;;           ad-do-it))))

;; (ido-ubiquitous-use-new-completing-read webjump 'webjump)
;; (ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
;; (ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)
; TODO: this doesn't work.  RET returns the empty string, rather than
; first item in the list.
;; (ido-ubiquitous-use-new-completing-read load-theme 'custom)

(ido-vertical-mode 1)
(defvar ido-vertical-define-keys)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(defvar ido-use-faces)
;;(setq ido-use-faces nil)
(setq ido-use-faces t)                  ; I think I prefer this?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smex
(defvar smex-save-file (expand-file-name "smex-items" savefile-dir))

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enable recent files mode.
(defvar recentf-save-file (expand-file-name "recentf" savefile-dir))
(defvar recentf-max-saved-items 50)
(defvar recentf-max-menu-items 15)

(recentf-mode t)                        ; this takes a while to load.
                                        ; maybe add it to a find-file
                                        ; hook or something?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completion in the minibuffer
(defvar icomplete-prospects-height 2)
(icomplete-mode t)                       ; completion in minibuffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-ido)

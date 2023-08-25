;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; deft customizations
;;
(defvar base-dir)

(defvar deft-extension "org")
(defvar deft-text-mode 'org-mode)
(defvar deft-directory
  (expand-file-name "deft/" base-dir))  ; trailing / is impt.
(defvar deft-strip-title-regexp
  "^#\\+\\w+:[ \t]+\\|^[#* ]*")

(global-set-key [f5] 'deft)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-deft)

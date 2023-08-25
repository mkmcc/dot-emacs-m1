;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modeline setup
;;
(line-number-mode t)                    ; modeline settings
(column-number-mode t)
(size-indication-mode t)

(file-name-shadow-mode 1)

(defvar display-time-format "%b %e %l:%M%#p")
(defvar display-time-load-average-threshold 1.0e99)
(display-time-mode 1)

(defvar battery-mode-line-format " [%b%p%%]")

;;; TODO: apparently this takes 400ms to load
;; (unless (string= system-name "strada.berkeley.edu")
;;   (display-battery-mode 1))


(require 'uniquify)

(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":"
  uniquify-after-kill-buffer-p t        ; rename after killing uniquified
  uniquify-ignore-buffers-re "^\\*")    ; don't muck with special buffers


;; modeline format
(setq-default
 mode-line-format
 '(;; Position, including warning for 80 columns
   (:propertize "%4l:" face mode-line-position-face)
   (:eval (propertize "%3c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   "  "
   ;; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize " RO " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize " ** " 'face 'mode-line-modified-face))
          (t (propertize "    " 'face 'mode-line-blank-face))))
   "  "
   ;; directory and buffer/file name
   (:propertize "%b"
                face mode-line-filename-face)
   ;; mode indicators: vc, recursive edit, major mode, etc.
   (vc-mode vc-mode)
   "  %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%] "
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
   (:propertize mode-line-process
                face mode-line-process-face)
   ; nyan-mode uses nyan cat as an alternative to %p
   ;"   "
   ;(:eval (when nyan-mode (list (nyan-create))))
   ;; pad to right and show the time/battery status
   (:eval (propertize " " 'display '((space :align-to (- right-fringe 20)))))
   (global-mode-string global-mode-string)))


;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-blank-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

(set-face-attribute 'mode-line-read-only-face nil
                    :inherit 'mode-line-face)

(set-face-attribute 'mode-line-blank-face nil
                    :inherit 'mode-line-face)

(set-face-attribute 'mode-line-modified-face nil
                    :inherit 'mode-line-face)

(set-face-attribute 'mode-line-folder-face nil
                    :inherit 'mode-line-face)

(set-face-attribute 'mode-line-filename-face nil
                    :inherit 'mode-line-face
                    :weight 'bold)

(set-face-attribute 'mode-line-position-face nil
                    :inherit 'mode-line-face)

(set-face-attribute 'mode-line-mode-face nil
                    :inherit 'mode-line-face)

(set-face-attribute 'mode-line-minor-mode-face nil
                    :inherit 'mode-line-mode-face)

(set-face-attribute 'mode-line-process-face nil
                    :inherit 'mode-line-face)

(set-face-attribute 'mode-line-80col-face nil
                    :inherit 'font-lock-warning-face)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-modeline)

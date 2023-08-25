;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; isearch
;;
(defvar dired-isearch-filenames t) ; make C-s and C-r only match filenames
                                   ; ...in dired buffers


;; http://www.emacswiki.org/emacs/ZapToISearch
;;; TODO: maybe use point instead of mark?  need to experiment and get
;;  used to this.
(defun zap-to-isearch (rbeg rend)
  "Kill the region between the mark and the closest portion of
the isearch match string. The behaviour is meant to be analogous
to zap-to-char; let's call it zap-to-isearch. The deleted region
does not include the isearch word. This is meant to be bound only
in isearch mode.  The point of this function is that oftentimes
you want to delete some portion of text, one end of which happens
to be an active isearch word. The observation to make is that if
you use isearch a lot to move the cursor around (as you should,
it is much more efficient than using the arrows), it happens a
lot that you could just delete the active region between the mark
and the point, not include the isearch word."
  (interactive "r")
  (when (not mark-active)
    (error "Mark is not active"))
  (let* ((isearch-bounds (list isearch-other-end (point)))
         (ismin (apply 'min isearch-bounds))
         (ismax (apply 'max isearch-bounds)))
    (if (< (mark) ismin)
        (kill-region (mark) ismin)
      (if (> (mark) ismax)
          (kill-region ismax (mark))
        (error "Internal error in isearch kill function.")))
    (isearch-exit)))

(define-key isearch-mode-map (kbd "M-z") 'zap-to-isearch)


;; Activate occur easily inside isearch (this is great!)
(define-key isearch-mode-map (kbd "C-o")
  (λ (let ((case-fold-search isearch-case-fold-search))
       (occur (if isearch-regexp
                  isearch-string
                (regexp-quote isearch-string))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-isearch)

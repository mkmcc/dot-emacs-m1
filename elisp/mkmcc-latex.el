;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; latex mode tweaks
;;

(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

(defun mkmcc-set-latex-compile-command ()
  "create a buffer-local compile command for latex buffers."
  (when buffer-file-name
    (let ((cmd "rubber -df --warn all")
          (fname (file-name-nondirectory (buffer-file-name))))
      (setq-local compile-command
                  (concat cmd " " fname)))))

(defun mkmcc-view-latex-file ()
  "find the pdf associated with a latex file and open it."
  (interactive)
  (when buffer-file-name
    (shell-command
     (concat "open "
             (file-name-sans-extension buffer-file-name)
             ".pdf"))))

(defvar reftex-docstruct-symbol)
(defun mkmccc-find-reftex-label ()
  "Prompt for a label (with completion) and return it."
  (interactive)
  (reftex-access-scan-info)
  (let* ((docstruct (symbol-value reftex-docstruct-symbol))
         (label (completing-read "Label: " docstruct
                                 (lambda (x) (stringp (car x))) t nil nil)))
    label))

(defadvice LaTeX-fill-region-as-paragraph (around LaTeX-sentence-filling)
  "Start each sentence on a new line."
  (let ((from (ad-get-arg 0))
        (to-marker (set-marker (make-marker) (ad-get-arg 1)))
        tmp-end)
    (while (< from (marker-position to-marker))
      (forward-sentence)
      ;; might have gone beyond to-marker --- use whichever is smaller:
      (ad-set-arg 1 (setq tmp-end (min (point) (marker-position to-marker))))
      ad-do-it
      (ad-set-arg 0 (setq from (point)))
      (unless (or
               (bolp)
               (looking-at "\\s *$"))
        (LaTeX-newline)))
    (set-marker to-marker nil)))
(ad-activate 'LaTeX-fill-region-as-paragraph)

(defun mkmcc-latex-mode-hook ()
  "defaults for `latex-mode'."
  (turn-on-reftex)
  (diminish 'reftex-mode)
  (abbrev-mode -1)
  (turn-on-auto-fill)
  (visual-line-mode +1)

  ;; show line endings
  (defvar whitespace-style)
  (setq-local whitespace-style
              '(face newline trailing empty newline-mark))
  (whitespace-mode +1)

  ;; F7 to compile, F8 to view the file; keep F12 for dictionary.
  (mkmcc-set-latex-compile-command)
  (local-unset-key (kbd "<f12>"))
  (local-set-key (kbd "<f8>") 'mkmcc-view-latex-file))

(add-hook 'latex-mode-hook 'mkmcc-latex-mode-hook)
(add-hook 'LaTeX-mode-hook 'mkmcc-latex-mode-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun auto-fill-by-sentences ()
  (if (looking-back (sentence-end))
      ;; Break at a sentence
      (progn
        (LaTeX-newline)
        t)
    ;; Fall back to the default
    (do-auto-fill)))
(add-hook 'LaTeX-mode-hook (lambda () (setq auto-fill-function 'auto-fill-by-sentences)))

;; Modified from http://pleasefindattached.blogspot.com/2011/12/emacsauctex-sentence-fill-greatly.html
(defadvice LaTeX-fill-region-as-paragraph (around LaTeX-sentence-filling)
  "Start each sentence on a new line."
  (let ((from (ad-get-arg 0))
        (to-marker (set-marker (make-marker) (ad-get-arg 1)))
        tmp-end)
    (while (< from (marker-position to-marker))
      (forward-sentence)
      ;; might have gone beyond to-marker---use whichever is smaller:
      (ad-set-arg 1 (setq tmp-end (min (point) (marker-position to-marker))))
      ad-do-it
      (ad-set-arg 0 (setq from (point)))
      (unless (or (looking-back "^\\s *")
                  (looking-at "\\s *$"))
        (LaTeX-newline)))
    (set-marker to-marker nil)))
(ad-activate 'LaTeX-fill-region-as-paragraph)

(provide 'mkmcc-latex)

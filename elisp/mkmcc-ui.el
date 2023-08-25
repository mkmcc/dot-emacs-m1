;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tweaks to user interface
;;
(defvar mkmcc-macosx-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annoyances and conveniences
(scroll-bar-mode 0)                     ; disable...
(when (fboundp 'tool-bar-mode)          ; ...
  (tool-bar-mode -1))                   ; ...
(unless (eq system-type 'darwin)        ; ...
  (menu-bar-mode -1))                   ; ...useless things

(put 'narrow-to-region 'disabled nil)   ; enable...
(put 'downcase-region 'disabled nil)    ; ...
(put 'upcase-region 'disabled nil)      ; ...
(put 'erase-buffer 'disabled nil)       ; ...useful things

(blink-cursor-mode -1)                  ; annoyances
(setq inhibit-startup-screen t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq echo-keystrokes 0.1)

(setq ring-bell-function 'ignore)       ; turn off the damn bell!

;; (defvar cua-enable-cua-keys nil)        ; only for rectangles
;; (defvar cua-delete-selection nil)       ; don't delete selection
;; (cua-mode t)

(when mkmcc-macosx-p
  (defvar locate-command "mdfind"))

;; compilation window
(defvar compilation-scroll-output 'first-error) ; scroll until first error
(defvar compilation-read-command t)             ; require enter to compile
(defvar compilation-window-height 16)           ; keep it readable

;; show-paren
(defvar show-paren-style 'parenthesis)
(show-paren-mode t)

(setq require-final-newline t)          ; end files with a newline

(electric-pair-mode t)                  ; smart pairing, indenting, etc.
(electric-indent-mode t)
(electric-layout-mode t)

(setq kill-buffer-query-functions       ; don't prompt me about processes
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(setq scroll-margin 0                   ; nice scrolling
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(setq redisplay-dont-pause t)           ; more responsive display

(set-default 'imenu-auto-rescan t)

;; revert buffers automatically when underlying files are changed
;; externally
(defvar global-auto-revert-non-file-buffers t) ; also revert dired
(defvar auto-revert-verbose nil)
(global-auto-revert-mode t)

;; saner regex syntax (maybe switch to rx?)
;;   NB: C-c C-w copies and converts to elisp format
(defvar reb-re-syntax 'string)

;; Use elisp ls program.  The osx one doesn't have the full GNU
;; functionality.
(defvar ls-lisp-ignore-case t)
(autoload 'insert-directory "ls-lisp.el") ; TODO: is this necessary?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spell check
(add-hook 'message-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook    'flyspell-mode)

(defvar ispell-silently-savep t)
(defvar ispell-program-name "aspell")
(after-load 'flyspell (diminish 'flyspell-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; frame font, colors, and other parameters
;;
;; fonts
(defvar mkmcc-fixed-pitch nil
  "monospace font to use.  E.g. 'Menlo-11'.  Define this in
 personal/personal.el")
(defvar mkmcc-variable-pitch nil
  "proportional font to use.  E.g. 'Helvetica-11'.  Define this
 in personal/personal.el")

;; don't propagate definitions from one theme to the next!
(defadvice load-theme
  (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

;; don't apply fonts and colors for text-only displays
;;   NB: this won't apply changes to standalone instances of emacs.  I
;;   think this is actually a good thing: just by looking at a window
;;   I can know whether or not it is attached to the emacs daemon.
;;   Also, there is no reason to use standalone instances...
(defun setup-window-system-frame-colours (&rest frame)
  (unless (eq window-system nil)
    (load-theme 'solarized-light t)
    (when mkmcc-fixed-pitch
      (set-frame-font mkmcc-fixed-pitch))
    (when mkmcc-fixed-pitch
      (set-face-attribute 'fixed-pitch    nil :font mkmcc-fixed-pitch))
    (when mkmcc-variable-pitch
      (set-face-attribute 'variable-pitch nil :font mkmcc-variable-pitch))))

(defadvice server-create-window-system-frame
  (after set-window-system-frame-colours ())
  "Set custom frame colours when creating the first frame on a display"
  (setup-window-system-frame-colours))

(ad-activate 'server-create-window-system-frame)

(add-hook 'after-make-frame-functions 'setup-window-system-frame-colours t)

(defun adjust-opacity (frame incr)
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

;; fullscreen
(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; coding -> reading
(defvar writeroom-mode-line nil
  "storage variable for the `mode-line-format' variable")

(defun writeroom-enable ()
  "set generous margins and delete the fringe."
  (interactive)
  (delete-other-windows)
  (let ((margin
         (/ (- (window-body-width) fill-column) 3)))
    (setq left-margin-width  margin
          right-margin-width 0
          left-fringe-width  0
          right-fringe-width 0)
    (setq writeroom-mode-line mode-line-format
          mode-line-format    nil))
  (set-window-buffer nil (current-buffer)))

(defun writeroom-disable ()
  "restore margins and fringe."
  (interactive)
  (setq left-margin-width  0
        right-margin-width 0
        left-fringe-width  nil
        right-fringe-width nil)
  (setq mode-line-format    writeroom-mode-line
        writeroom-mode-line nil)
  (set-window-buffer nil (current-buffer)))

(defun readability ()
  (interactive)
  ;; buffer-face-mode is both a function and a variable
  (if (and (boundp 'buffer-face-mode)
           (symbol-value 'buffer-face-mode))
      (progn
        (variable-pitch-mode -1)
        (text-scale-increase -3)
        (setq line-spacing nil)
        (writeroom-disable))
    (progn
      (variable-pitch-mode t)
      (text-scale-increase 3)
      (setq line-spacing 7)
      (writeroom-enable))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; whitespace-mode config
(defvar whitespace-line-column 80)
(defvar whitespace-style
  '(face tabs spaces newline trailing lines-tail
         indentation empty
         space-mark tab-mark newline-mark))

(after-load 'whitespace
  (diminish 'whitespace-mode))

(defun prelude-enable-whitespace ()
  "enable `whitespace-mode'."
  (add-hook 'before-save-hook 'whitespace-cleanup nil t)
  (whitespace-mode +1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; games
(defvar tetris-score-file "~/.emacs.d/games/tetris-scores")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-ui)

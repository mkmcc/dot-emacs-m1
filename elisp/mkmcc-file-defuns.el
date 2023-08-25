;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file defuns
;;
(require 'dash)
(defvar base-dir)

(defun mkmcc-open-with (arg)
  "Open visited file in default external program.

With a prefix ARG always prompt for command to use."
  (interactive "P")
  (when buffer-file-name
    (shell-command (concat
                    (cond
                     ((and (not arg) mkmcc-macosx-p) "open")
                     (t (read-shell-command "Open current file with: ")))
                    " "
                    (shell-quote-argument buffer-file-name)))))

(defun prelude-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun prelude-sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (or (equal major-mode 'dired-mode)
              (and (buffer-file-name)
                   (not (file-exists-p (file-name-directory (buffer-file-name)))))
              (and (buffer-file-name)
                   (file-writable-p buffer-file-name)))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun prelude-recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: "
                                   (-map 'abbreviate-file-name recentf-list)
                                   nil t)))
    (when file
      (find-file file))))

(defun prelude-recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory base-dir 0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; upward find file: used in general and c mode
(defun mkmcc-upward-find-file (filename &optional startdir)
  "Move up directories until we find a certain filename. If we
manage to find it, return the containing directory.  Else if we
get to the toplevel directory and still can't find it, return
nil. Start at startdir or . if startdir not given"
  (let ((dirname (expand-file-name (if startdir startdir ".")))
        (found nil)
        (top nil))
    ;; Traverse directory, looking for filename
    (while (not (or found top))
      (if (string= (expand-file-name dirname) "/")
          (setq top t))
      (if (file-exists-p (expand-file-name filename dirname))
          (setq found t)
        (setq dirname (directory-file-name (file-name-directory dirname)))))
    ;; Return dirname or nil
    (if found dirname nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tags
;; update tags file
(defun mkmcc-update-tags ()
  "Create or update the ctags tag file.  Prompts for the top of
the source tree using ido."
  (interactive)
  (let ((olddir default-directory)
        (topdir)
        (dir-buffer)
        (tag-flag))

    ; guess TAGS location, then verify using ido
    (setq topdir (mkmcc-upward-find-file "TAGS" default-directory))
    (unless topdir
      (setq topdir default-directory))
    (setq topdir (ido-read-directory-name "ctags: top of source tree:"
                                          topdir))

    ; run ctags
    (save-excursion
      (setq dir-buffer (find-file-noselect topdir))
      (set-buffer dir-buffer)
      (setq tag-flag
            (call-process "ctags" nil "*ctags errors*" nil
                          "-e" "-a" "-o TAGS" "-R" "."))
      (if (eq tag-flag 0)
          (progn
            (message "Tags created")
            (kill-buffer "*ctags errors*"))
          (progn
            (message "Ctags returned an error")
            (switch-to-buffer-other-window "*ctags errors*")))
      (kill-buffer dir-buffer))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run current file
(defun run-current-file ()
  "Execute or compile the current file.
For example, if the current buffer is the file x.pl, then it'll
call \"perl x.pl\" in a shell.  The file can be php, perl, python,
ruby, javascript, bash, ocaml, java.  File suffix is used to
determine what program to run."
  (interactive)
  (let ((extension-alist '(("pl"  . "perl")
                           ("py"  . "python")
                           ("rb"  . "ruby")
                           ("sh"  . "bash")
                           ("m"   . "mash")
                           ("clj" . "clojure")))
        (executable-name nil)
        file-name
        file-suffix
        command-string)

    (setq file-name         (buffer-file-name)
          file-suffix       (file-name-extension file-name)
          executable-name   (cdr (assoc file-suffix extension-alist))
          command-string    (concat executable-name " \"" file-name "\""))

    (cond ((string-equal file-suffix "el")
           (load-file file-name))
          ((stringp executable-name)
           (shell-command command-string))
          (t
           (message "Couldn't interpret file extension.")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-file-defuns)

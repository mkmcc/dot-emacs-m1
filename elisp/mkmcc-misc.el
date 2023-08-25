;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; miscellaneous definitions
;;
(require 'thingatpt)

;; evaluate lisp sexps anywhere
(defun prelude-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (backward-kill-sexp)
    (insert (format "%s" value))))


(defun mkmcc-call-dict-command ()
  "look up word-at-point in the osx dictionary."
  (interactive)
  (let* ((word (word-at-point)))
    (if word
        (concat "open dict://" (substring-no-properties word))
      "open -a /Applications/Dictionary.app")))

(defvar mkmcc-dictionary-file
  "~/Documents/Other/dictionary/dict-data.tex"
  "file path to my personal dictionary")

(defun mkmcc-add-definition-to-dictionary (word)
  "add a word and definition to my personal dictionary."
  (interactive (list (read-from-minibuffer "word: ")))
  (let* ((cmd (concat "~/build/bin/define" " " word))
         (def (shell-command-to-string cmd))
         (entry
          (concat "\\word{" word "}{uncategorized}%\n"
                  "{" (s-trim def) "}\n\n")))
    (append-to-file entry nil mkmcc-dictionary-file)))

(defun prelude-google ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Google: "))))))

(defun prelude-view-url ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    (cond ((search-forward "<?xml" nil t) (nxml-mode))
          ((search-forward "<html" nil t) (html-mode)))))

(defun prelude-annotate-todo ()
  "Put fringe marker on TODO: lines in the curent buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "TODO:" nil t)
      (let ((overlay (make-overlay (- (point) 5) (point))))
        (overlay-put overlay
                     'before-string
                     (propertize (format "A")
                                 'display '(left-fringe right-triangle)))))))

(defun prelude-insert-date ()
  "Insert a timestamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mkmcc-misc)

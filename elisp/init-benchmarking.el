;;; can't use features from s and dash here
(defvar benchmark/require-times nil
  "A list of (FEATURE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defadvice require
  (around build-require-times (feature &optional filename noerror) activate)
  "Note in `benchmark/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        ad-do-it
      (when (and (not already-loaded) (memq feature features))
        (add-to-list 'benchmark/require-times
                     (cons feature
                           (benchmark/time-subtract-millis (current-time)
                                                           require-start-time))
                     t)))))


;;; use s and dash in these functions, but be careful when you call them!
(defun benchmark/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))

(defun benchmark/format-item (item)
  (concat
   (s-pad-right 20 " " (s-truncate 20 (symbol-name (car item))))
   (format "%7.3f"   (cdr item))))

(defun benchmark/report-require-times ()
  (let* ((total-time (benchmark/time-subtract-millis
                      (current-time) before-init-time))
         (require-time (apply '+ (-map 'cdr benchmark/require-times)))
         (offenders (-take 10 (--sort (> (cdr it) (cdr other))
                                      benchmark/require-times))))
    (apply 'concat
           (-interpose "\n"
                       (list
                        (format "init completed in %.5fs" (/ total-time 1000))
                        (format "require's took %.5fs" (/ require-time 1000))
                        "leading offenders:"
                        (mapconcat 'benchmark/format-item offenders "\n"))))))

(provide 'init-benchmarking)

;;; my-application-paths --- Set paths to external application binaries
;;; Commentary:
;;; Code:

(defun my-normalize-path (path)
  "Return a string representing a normalized version of PATH.
This function is intended for use with Windows paths, which are
case-insensitive."
  (downcase (expand-file-name "" path)))

(defun my-remove-and-prepend-paths (ps1 ps2)
  "Prepend list of paths PS1 in reverse order to list of paths PS2.
Any elements of PS1 initially present in PS2 are removed from the
original list PS2."
  (if ps1
      (my-remove-and-prepend-paths
       (cdr ps1)
       (cons (car ps1) (remove (car ps1) ps2)))
    ps2))

(defun my-concat-paths (ps1 ps2)
  "Prepend list of paths PS1 to list of paths PS2.
Each path in the resulting list will be normalized and duplicates
removed from the final list."
  (let ((ps1_ (mapcar 'my-normalize-path ps1))
        (ps2_ (mapcar 'my-normalize-path ps2)))
    (my-remove-and-prepend-paths ps1_ ps2_)))

(provide 'my-application-paths)
;;; my-application-paths ends here

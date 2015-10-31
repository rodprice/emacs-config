;;; my-application-paths --- Set paths to external application binaries
;;; Commentary:
;;; Code:

(defun my-normalize-path (path)
  "Return a string representing a normalized version of PATH.
This function is intended for use with Windows paths, which are
case-insensitive."
  (downcase (expand-file-name "" path)))

(defun my-concat-paths (ps1 ps2)
  "Prepend list of paths PS1 to list of paths PS2.
Each path in the resulting list will be normalized and duplicates
removed from the final list."
  (let ((ps1_ (mapcar 'my-normalize-path ps1))
        (ps2_ (mapcar 'my-normalize-path ps2)))
    (delete-dups (append ps2_ ps1_))))

(provide 'my-application-paths)
;;; my-application-paths ends here

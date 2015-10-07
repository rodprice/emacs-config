;;; functions --- Functions for use in emacs customization
;;; Commentary:
;;; Code:

(defun remove-all-matches-from-alist (name alist)
  "Remove any entries matching NAME from ALIST.

This function assumes that ALIST has the same form as `auto-mode-alist' and friends.  This function recurses until all entries matching NAME are removed.  The return value is the alist with matching entries removed."
  (let ((mode (assoc-default name alist 'string-match)))
    (if mode
        (remove-all-matches-from-alist
         name
         (remove (rassoc mode alist) alist))
      alist)))

(provide 'functions)
;;; functions.el ends here

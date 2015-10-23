;;; my-wolfram.el --- Customizations for Mathematica code
;;; Commentary:
;;; Code:

;;  (autoload 'wolfram-mode "wolfram-mode" nil t)
;;  (autoload 'run-wolfram "wolfram-mode" nil t)
(setq wolfram-program my-mathematica-kernel-dir)

(defun remove-all-matches-from-alist (name alist)
  "Remove any entries matching NAME from ALIST.

This function assumes that ALIST has the same form as
`auto-mode-alist' and friends.  This function recurses until all
entries matching NAME are removed.  The return value is the alist
with matching entries removed."
  (let ((mode (assoc-default name alist 'string-match)))
    (if mode
        (remove-all-matches-from-alist
         name
         (remove (rassoc mode alist) alist))
      alist)))

;; Remove binding for .m files to objc-mode; bind to wolfram-mode
(setq auto-mode-alist
 (cons '("\\.m\\'" . wolfram-mode)
       (remove-all-matches-from-alist ".m" auto-mode-alist)))

;; Auto-pair comment brackets
(sp-local-pair '(wolfram-mode) "(*" "*)"
               :unless '(sp-in-string-p)
               :actions '(insert wrap))

(provide 'my-wolfram)
;;; my-wolfram.el ends here

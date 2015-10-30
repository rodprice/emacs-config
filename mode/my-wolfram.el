;;; my-wolfram.el --- Customizations for Mathematica code
;;; Commentary:
;;; Code:

(defun remove-all-matches-from-alist (name alist)
  "Remove any entries matching NAME from ALIST.

This function assumes that ALIST has the same form as
`auto-mode-alist' and friends.  This function recurses until all
entries matching NAME are removed.  The return value is the alist
with matching entries removed."
  (let ((mode (assoc-default name alist 'string-match)))
    (if mode
        (remove-all-matches-from-alist name
         (remove (rassoc mode alist) alist))
      alist)))

(require 'use-package)
(use-package wolfram-mode
  :config
  (setq wolfram-program my-mathematica-kernel-dir)
  ;; Remove binding for .m files to objc-mode; bind to wolfram-mode
  (setq auto-mode-alist
        (cons '("\\.m\\'" . wolfram-mode)
              (remove-all-matches-from-alist ".m" auto-mode-alist)))
  :pin local)

;; To set up flycheck mode I need a syntax checker. Perhaps
;; `http://mathematica.stackexchange.com/questions/24176/mathematica-command-for-type-checking'
;; is a start?

;; Auto-pair comment brackets --- solution doesn't work
;; (sp-local-pair '(wolfram-mode) "(*" "*)"
;;                :unless '(sp-in-string-p)
;;                :actions '(insert wrap))

(add-hook 'wolfram-mode-hook
          (lambda ()
            (turn-off-smartparens-mode)
            (turn-off-show-smartparens-mode)))

(provide 'my-wolfram)
;;; my-wolfram.el ends here

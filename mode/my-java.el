;;; my-java.el --- My Java programming configuration
;;; Commentary:
;; TODO: try out eclim
;; URL: https://github.com/senny/emacs-eclim
;; TODO: try out java-imports
;; URL: https://github.com/dakrone/emacs-java-imports
;;; Code:

;; A few settings from https://justin.abrah.ms/dotfiles/emacs.html
(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 2)
            (setq fill-column 100)
            (fci-mode t)
            (subword-mode t)
            (local-set-key (kbd "C-M-h") 'windmove-left)
            (hs-minor-mode 1)))

;; Doesn't require connection to external program
(use-package java-imports
  :disabled t
  :config
  (progn
    (define-key java-mode-map (kbd "M-I") 'java-imports-add-import-dwim)
    ;(define-key java-mode-map (kbd "M-I") 'java-imports-add-import)
    ;(define-key java-mode-map (kbd "M-I") 'java-imports-scan-file)
   (setq java-imports-find-block-function 'java-imports-find-place-sorted-block)
    (add-hook 'java-mode-hook 'java-imports-scan-file))
  :pin melpa-stable)

(provide 'my-java)
;;; my-java.el ends here

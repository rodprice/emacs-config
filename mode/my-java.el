;;; my-java.el --- My Java programming configuration
;;; Commentary:
;; TODO: try out eclim
;; URL: https://github.com/senny/emacs-eclim
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

(provide 'my-java)
;;; my-java.el ends here

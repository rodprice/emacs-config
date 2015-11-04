;;; my-emacs-lisp.el --- My Emacs lisp programming configuration
;;; Commentary:
;;; Code:

;; Provide arguments for lisp functions in the minibuffer
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; Tell flycheck to use load-path from the current session
(require 'flycheck)
(setq flycheck-emacs-lisp-load-path 'inherit)


(provide 'my-emacs-lisp)
;;; my-emacs-lisp.el ends here

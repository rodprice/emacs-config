;;; my-emacs-lisp.el --- My Emacs lisp programming configuration
;;; Commentary:
;;; Code:

;; Provide arguments for lisp functions in the minibuffer
(require 'eldoc)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; Tell flycheck to use load-path from the current session
(require 'flycheck)
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
(setq flycheck-emacs-lisp-load-path 'inherit)

(provide 'my-emacs-lisp)
;;; my-emacs-lisp.el ends here

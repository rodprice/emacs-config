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

;; Set up auto-completion
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)
;(add-hook 'emacs-lisp-mode-hook
;          (lambda ()
;            (set (make-local-variable 'tab-always-indent) 'complete)))

;; Set up jump to definition M-. and jump back M-,
(require 'use-package)
(use-package elisp-slime-nav
  :ensure t
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode)))

(provide 'my-emacs-lisp)
;;; my-emacs-lisp.el ends here

;;; my-python.el --- Python mode and programming tools setup code

;; Created: 20 Oct 2015
;; Version: 0.1
;; Package-Requires: ((ob-ipython "0.1") (flycheck "0.24"))

;;; Commentary:
;;; Code:

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up pyflakes for use with flycheck
;; See https://github.com/Wilfred/flycheck-pyflakes

(require 'flycheck)

(flycheck-define-checker python-pyflakes
  "A Python syntax and style checker using the pyflakes utility."
  :command ("pyflakes" source-inplace)
  :error-patterns
  ((error line-start (file-name) ":" line ":" (message) line-end))
  :modes python-mode)

(add-to-list 'flycheck-checkers 'python-pyflakes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use org-babel mode with ipython notebooks

;; See https://github.com/gregsexton/ob-ipython/tree/ipython3
(use-package ob-ipython
  :config
  (setq org-confirm-babel-evaluate nil)
  (add-hook 'org-babel-after-execute-hook
            'org-display-inline-images 'append)
  :pin local)

(provide 'my-python)
;;; my-python.el ends here

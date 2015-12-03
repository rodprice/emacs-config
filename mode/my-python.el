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
  "A Python syntax checker using the pyflakes utility.

Pyflakes analyzes Python programs and detects various errors. It
works by parsing the source file, not importing it, so it is safe
to use on modules with side effects. Unlike Pylint, it does not
check for style. See URL `https://pypi.python.org/pypi/pyflakes'."
  :command ("pyflakes" source-inplace)
  :error-patterns
  ((error line-start (file-name) ":" line ":" (message) line-end))
  :modes python-mode)

(add-to-list 'flycheck-checkers 'python-pyflakes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup autocomplete using the jedi python library
;; Manual setup:
;;   cd ~/.emacs.d/elpa/jedi-core-0.2.6
;;   python setup.py install
;; Then check to see if jediepcserver.py shows up in the directory below.
(use-package jedi
  :ensure t
  :config
;  (setq python-environment-directory
;        (expand-file-name "envs" my-anaconda-dir))
;  (setq python-environment-virtualenv
;        (list "conda" )) ; incomplete
;  (setq python-environment-default-root-name "emacs")
;  (setq jedi:server-command
;        (list (expand-file-name "lib/site-packages/jediepcserver.py" my-anaconda-dir)))
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot)
  :pin melpa-stable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use org-babel mode with ipython notebooks

;; See https://github.com/gregsexton/ob-ipython/tree/ipython3
(use-package ob-ipython
  :disabled t
  :config
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-preserve-indentation t)
  (add-hook 'org-babel-after-execute-hook
            'org-display-inline-images 'append)
  :pin local)

(provide 'my-python)
;;; my-python.el ends here

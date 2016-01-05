;;; my-python.el --- Python mode and programming tools setup code

;; Created: 20 Oct 2015
;; Version: 0.1
;; Package-Requires: ((ob-ipython "0.1") (flycheck "0.24"))

;;; Commentary:
;;; Code:

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple utility defuns

;; http://stackoverflow.com/questions/20274336/how-to-automatically-align-comments-in-different-pieces-of-code
(defun my-align-comments (beginning end)
  "Align comments within marked region."
  (interactive "*r")
  (let (indent-tabs-mode align-to-tab-stop)
    (align-regexp beginning end (concat "\\(\\s-*\\)"
                                        (regexp-quote comment-start)))))

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
  :disabled t
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
;; Inferior Python shell stuff

(defun my-python-shell-send (start end &optional send-main)
  "If a region is marked, send the region to the inferior Python
  process; otherwise, send the entire buffer.  Finally, switch to
  the buffer containing the inferior Python process."
  (interactive "r\nP")
  (if (use-region-p)
      (python-shell-send-region start end send-main)
    (python-shell-send-buffer send-main))
  (other-window 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple ipython setup
;; http://stackoverflow.com/questions/25669809/how-do-you-run-python-code-using-emacs

(require 'python)
(setq python-shell-interpreter
      (expand-file-name "python.exe" my-anaconda-dir))
(setq python-shell-interpreter-args
      (concat "-i " (expand-file-name "ipython-script.py" my-anaconda-scripts-dir)))
(setq python-shell-prompt-regexp "In \\[[0-9]+\\]: ")
(setq python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")

;; Stop python-mode from complaining about matching prompts
(setq python-shell-prompt-detect-failure-warning nil)

;; TODO set `python-shell-virtualenv-path' correctly

; python-shell-completion-setup-code
; "from IPython.core.completerlib import module_completion"
; python-shell-completion-string-code
; "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toggle between a Python buffer and its inferior Python process
;; See https://www.masteringemacs.org/article/toggling-python-buffers
;; TODO find out why python-shell-internal-buffer is not defined

(defvar python-last-buffer nil
  "Name of the Python buffer that last invoked `toggle-between-python-buffers'")
(make-variable-buffer-local 'python-last-buffer)

(defun toggle-between-python-buffers ()
  "Toggles between a `python-mode' buffer and its inferior Python process

When invoked from a `python-mode' buffer it will switch the
active buffer to its associated Python process. If the command is
invoked from a Python process, it will switch back to the `python-mode' buffer."
  (interactive)
  ;; check if `major-mode' is `python-mode' and if it is, we check if
  ;; the process referenced in `python-buffer' is running
  (if (and (eq major-mode 'python-mode)
           (processp (get-buffer-process python-shell-internal-buffer)))
      (progn
        ;; store a reference to the current *other* buffer; relying
        ;; on `other-buffer' alone wouldn't be wise as it would never work
        ;; if a user were to switch away from the inferior Python
        ;; process to a buffer that isn't our current one. 
        (switch-to-buffer python-shell-internal-buffer)
        (setq python-last-buffer (other-buffer)))
    ;; switch back to the last `python-mode' buffer, but only if it
    ;; still exists.
    (when (eq major-mode 'inferior-python-mode)
      (if (buffer-live-p python-last-buffer)
          (switch-to-buffer python-last-buffer)
        ;; buffer's dead; clear the variable.
        (setq python-last-buffer nil)))))

(define-key inferior-python-mode-map (kbd "<f12>") 'toggle-between-python-buffers)
(define-key python-mode-map (kbd "<f12>") 'toggle-between-python-buffers)

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

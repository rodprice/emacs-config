;;; init.el --- Emacs initialization file
;;; Commentary:
;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set paths and load the site-specific file

;; Set up load paths within .emacs.d
(add-to-list 'load-path (expand-file-name "init/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "mode/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "pkgs/" user-emacs-directory))

(require 'my-paths)

;; Load the site-specific preload file.  See the path variables
;; defined in `my-paths' for information on required paths.
(load (concat system-name "-preload") 'noerror)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run the emacs server and set up packaging systems

;; From https://ipython.org/ipython-doc/1/config/editors.html
(defvar server-buffer-clients)
(when (and (fboundp 'server-start)
           (string-equal (getenv "TERM") 'xterm))
  (server-start)
  (defun fp-kill-server-with-buffer-routine ()
    (and server-buffer-clients (server-done)))
  (add-hook 'kill-buffer-hook 'fp-kill-server-with-buffer-routine))

(require 'my-packaging)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make emacs look and behave better

(require 'better-defaults)              ; other people like these
(require 'my-defaults)                  ; my misc settings
(require 'my-geometry)                  ; save emacs frame
(require 'my-theme)                     ; fonts and colors

(require 'my-functions)
(setq ring-bell-function 'echo-area-bell)

(global-set-key (kbd "M-j") 'my-join-lines)
(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "M-o") 'open-previous-line)
(global-set-key (kbd "M-<up>") 'scroll-row-up)
(global-set-key (kbd "M-<down>") 'scroll-row-down)
(global-set-key (kbd "C-<up>") 'xah-backward-block)
(global-set-key (kbd "C-<down>") 'xah-forward-block)
(global-set-key (kbd "C-x C-o") 'other-frame)
(global-set-key (kbd "M-p") 'my-rearrange-windows)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load packages and configure them

(use-package direx
  :ensure t
  :bind (("C-x C-j" . direx:jump-to-directory))
  :pin melpa-stable)

;; Flip between buffers
(use-package iflipb
  :ensure t
  :bind (("M-<prior>" . iflipb-next-buffer)
         ("M-<next>" . iflipb-previous-buffer))
  :pin melpa-stable)

(use-package markdown-mode
  :ensure t
  :defer t
  :mode "\\.md\\'"
  :commands (markdown-mode gfm-mode)
  :init
  (setq
   markdown-command "pelican content"
   markdown-command-needs-filename nil
   markdown-enable-math t
   markdown-open-command nil)
  :pin melpa-stable)

;; Preview Markdown content in a browser at every save
;; https://github.com/ancane/markdown-preview-mode
(use-package markdown-preview-mode
  :disabled
  :ensure t
  :defer t
  :pin melpa-stable)

(use-package rainbow-mode
  :ensure t
  :pin gnu)

(use-package smartparens
  :ensure t
  :init
  (progn
    (use-package smartparens-config)
    (smartparens-global-mode 1)
    (show-smartparens-global-mode 1)
    (setq sp-show-pair-delay 0))
  :config
  (progn
    (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p)))
  :bind
  (("C-M-k" . sp-kill-sexp-with-a-twist-of-lime)
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-n" . sp-up-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-u" . sp-backward-up-sexp)
   ("C-M-p" . sp-backward-down-sexp)
   ("C-M-w" . sp-copy-sexp)
   ("M-s" . sp-splice-sexp)
   ("M-r" . sp-splice-sexp-killing-around)
   ("C-)" . sp-forward-slurp-sexp)
   ("C-}" . sp-forward-barf-sexp)
   ("C-(" . sp-backward-slurp-sexp)
   ("C-{" . sp-backward-barf-sexp)
   ("M-S" . sp-split-sexp)
   ("M-J" . sp-join-sexp)
   ("C-M-t" . sp-transpose-sexp)))

;; Kill and yank entire lines
(use-package whole-line-or-region
  :ensure t
  :bind (("C-w" . whole-line-or-region-kill-region)
         ("M-w" . whole-line-or-region-copy-region-as-kill)
         ("C-y" . whole-line-or-region-yank))
  :pin melpa-stable)

(use-package yaml-mode
  :ensure t
  :mode "\\.\\(condarc\\|ya?ml\\)\\''"
  :bind ("C-m" . newline-and-indent)
  :pin melpa-stable)

(use-package yasnippet
  :ensure t
  :config
  (progn
    (yas-global-mode 1)
    (define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-ido-expand))
  :pin gnu)

(use-package yatemplate
  :ensure t
  :config
  (progn
    (add-hook 'find-file-hook 'auto-insert)
    (setq yatemplate-dir (expand-file-name "templates/" user-emacs-directory))
    (yatemplate-fill-alist))
  :pin melpa-stable)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming modes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++

(require 'my-c)

;; (use-package my-c
;;   :commands (my-compile)
;;   :bind (("C-c C-c" . my-compile)))

;; (use-package jedi
;;   :ensure t
;;   :config
;;   (add-hook 'python-mode-hook 'jedi:setup)
;;   (setq jedi:setup-keys t)
;;   (setq jedi:complete-on-dot t)
;;   :pin melpa-stable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple ipython setup
;; http://stackoverflow.com/questions/25669809/how-do-you-run-python-code-using-emacs

;; (require 'python)
;; Arguments to the Python interpreter are as follows
;;   -u (unbuffered; interpreter running under comint can hang otherwise)
;;   -i (interactive)
;; Arguments to the ipython-script are
;;   console (This appears to be the magic incantation to get plotting functionality.
;;            Once the shell starts up, invoke %pylab and you're in business.)

;; -i
;;     If running code from the command line, become interactive afterwards.
;;     It is often useful to follow this with `--` to treat remaining flags as
;;     script arguments.
;; --no-confirm-exit
;;     Don't prompt the user when exiting.
;; --pylab=<CaselessStrEnum> (InteractiveShellApp.pylab)
;;     Default: None
;;     Choices: [u'auto', u'gtk', u'gtk3', u'inline', u'nbagg', u'notebook', u'osx', u'qt', u'qt4', u'qt5', u'tk', u'wx']
;;     Pre-load matplotlib and numpy for interactive use, selecting a particular
;;     matplotlib backend and loop integration.
;; --matplotlib=<CaselessStrEnum> (InteractiveShellApp.matplotlib)
;;     Default: None
;;     Choices: [u'auto', u'gtk', u'gtk3', u'inline', u'nbagg', u'notebook', u'osx', u'qt', u'qt4', u'qt5', u'tk', u'wx']
;;     Configure matplotlib for interactive use with the default matplotlib
;;     backend.
;; --colors=<CaselessStrEnum> (InteractiveShell.colors)
;;     Default: 'Linux'
;;     Choices: [u'NoColor', u'LightBG', u'Linux']
;;     Set the color scheme (NoColor, Linux, or LightBG).
;; --color-info
;;     IPython can display information about objects via a set of functions,
;;     and optionally can use colors for this, syntax highlighting
;;     source code and various other elements. This is on by default, but can cause
;;     problems with some pagers. If you see such problems, you can disable the
;;     colours.
;; --nosep
;;     Eliminate all spacing between prompts.
;; --pprint
;;     Enable auto pretty printing of results.

;; (setq python-shell-interpreter "ipython")
;; (setq python-shell-interpreter-args "--pylab --pprint")
;; (setq python-shell-prompt-regexp "In \\[[0-9]+\\]: ")
;; (setq python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")

;; ;; Stop python-mode from complaining about matching prompts
;; (setq python-shell-prompt-detect-failure-warning nil)
;; ;; Completion stuff that I don't understand
;; (setq python-shell-completion-setup-code
;;       "from IPython.core.completerlib import module_completion"
;;       python-shell-completion-module-string-code
;;       "';'.join(module_completion('''%s'''))\n"
;;       python-shell-completion-string-code
;;       "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; TODO set `python-shell-virtualenv-path' correctly


;; (defvar myPackages
;;   '(ein
;;     elpy
;;     flycheck
;;     py-autopep8))

;; (mapc #'(lambda (package)
;;     (unless (package-installed-p package)
;;       (package-install package)))
;;       myPackages)

;; ;; BASIC CUSTOMIZATION
;; ;; --------------------------------------

;; ;; PYTHON CONFIGURATION
;; ;; --------------------------------------

(use-package flycheck
  :ensure t
  :config
  ;; Use pyflakes and nothing else
  (flycheck-define-checker python-pyflakes
    "A Python syntax checker using the pyflakes utility."
    :command ("pyflakes" source-inplace)
    :error-patterns
    ((error line-start (file-name) ":" line ":" (message) line-end))
    :modes python-mode)
  (add-to-list 'flycheck-checkers 'python-pyflakes)
  (add-to-list 'flycheck-disabled-checkers 'python-flake8)
  (add-to-list 'flycheck-disabled-checkers 'python-pylint)
  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (global-flycheck-mode)
  :pin melpa)

(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (elpy-use-ipython)
  ;; (when (require 'flycheck nil t)
  ;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  ;;   (add-hook elpy-mode-hook 'flycheck-mode))
  :pin melpa)

(defun my-pytest-all (&optional flags)
  (interactive)
  (pytest-run nil flags)
  (other-window 1))

(defun my-pytest-module (&optional flags)
  (interactive)
  (pytest-run buffer-file-name flags)
  (other-window 1))

(defun my-pytest-one (&optional flags)
  (interactive)
  (pytest-run (format "%s" (pytest-py-testable)) flags)
  (other-window 1))

(defun my-pytest-directory (&optional flags)
  (interactive)
  (pytest-run (file-name-directory buffer-file-name) flags)
  (other-window 1))

(use-package pytest
  :ensure t
  :config
  (setq pytest-cmd-flags "-x -s -r a"
        pytest-use-verbose nil
        pytest-loop-on-failing nil
        pytest-assert-plain t)
  :pin melpa)

(add-hook 'python-mode-hook
          (lambda ()
            ;; (local-set-key (kbd "C-c l") 'my-python-shell-send-line)
            (local-set-key (kbd "C-x C-e") 'python-shell-send-defun)
            (local-set-key (kbd "C-c m") 'my-pytest-module)
            (local-set-key (kbd "C-c f") 'my-pytest-one)
            (local-set-key (kbd "C-c d") 'my-pytest-directory)))

;; (setq python-shell-interpreter-args "--simple-prompt -i")

;; ;; use flycheck not flymake with elpy
;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

;; ;; enable autopep8 formatting on save
;; (require 'py-autopep8)
;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; ;; init.el ends here
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(elpy-syntax-check-command "pyflakes")
;;  '(elpy-test-discover-runner-command (quote ("python" "-m" "pytest")))
;;  '(elpy-test-runner (quote elpy-test-pytest-runner)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )

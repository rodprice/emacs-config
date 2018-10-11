;;; init.el --- Emacs initialization file
;;; Commentary:
;;; Code:


;; Get emacs 25 to stop pestering me about packages until I'm ready.
;; (package-initialize)


;; Set up load paths within .emacs.d

(add-to-list 'load-path (expand-file-name "init/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "mode/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "pkgs/" user-emacs-directory))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run the emacs server and set up packaging systems

;; From https://ipython.org/ipython-doc/1/config/editors.html
;; (defvar server-buffer-clients)
;; (when (and (fboundp 'server-start)
;;            (string-equal (getenv "TERM") 'xterm))
;;   (server-start)
;;   (defun fp-kill-server-with-buffer-routine ()
;;     (and server-buffer-clients (server-done)))
;;   (add-hook 'kill-buffer-hook 'fp-kill-server-with-buffer-routine))

(require 'my-packaging)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load the site-specific file and set paths

;; Load the site-specific preload file.  See the path variables
;; defined in `my-paths' for information on required paths.
(load (concat (system-name) "-preload") 'noerror)

;; Add site-specific paths to emacs' exec-path and $PATH
(use-package my-paths
  :config
  ;; Prepend the contents of `my-path-variables' to `exec-path'.
  (setq exec-path
        (let ((my-paths (mapcar 'symbol-value my-path-variables)))
          (my-concat-paths my-paths exec-path)))
  ;; Make the environment variable $PATH match `exec-path'
  (let ((sep (if (eq system-type (intern "windows-nt")) ";" ":")))
    (setenv "PATH" (mapconcat 'identity exec-path sep)))
  (require 'exec-path-from-shell))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make emacs look and behave better

(require 'better-defaults)              ; other people like these
(require 'my-defaults)                  ; my misc settings
(require 'my-geometry)                  ; save emacs frame
(require 'my-theme)                     ; fonts and colors
(message "past my-theme")

(require 'my-functions)
(setq ring-bell-function 'echo-area-bell)

(global-set-key (kbd "C-c q") 'auto-fill-mode)
(global-set-key (kbd "M-j") 'my-join-lines)
(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "M-o") 'open-previous-line)
(global-set-key (kbd "C-t") 'transpose-next-line)
(global-set-key (kbd "M-t") 'transpose-previous-line)
(global-set-key (kbd "M-<up>") 'scroll-row-up)
(global-set-key (kbd "M-<down>") 'scroll-row-down)
(global-set-key (kbd "C-<up>") 'xah-backward-block)
(global-set-key (kbd "C-<down>") 'xah-forward-block)
(global-set-key (kbd "C-x C-o") 'other-frame)
(global-set-key (kbd "M-p") 'my-rearrange-windows)
(global-set-key [remap fill-paragraph]  ; M-q toggles fill-paragraph
                #'endless/fill-or-unfill)
(setq set-mark-command-repeat-pop t)    ; better C-u C-SPC C-SPC ...
(define-key ctl-x-map "n" #'narrow-or-widen-dwim)

(setq split-height-threshold nil)
(setq split-width-threshold 160)

(global-hl-line-mode t)                 ; Highlight current line

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(add-hook 'comint-mode-hook
          (lambda ()
            (define-key comint-mode-map
              (kbd "C-d") 'my-comint-delchar-or-eof-or-kill-buffer)
            (define-key comint-mode-map
              (kbd "C-<up>") 'comint-previous-matching-input-from-input)
            (define-key comint-mode-map
              (kbd "C-p") 'comint-previous-matching-input-from-input)
            (define-key comint-mode-map
              (kbd "C-<down>") 'comint-next-matching-input-from-input)
            (define-key comint-mode-map
              (kbd "C-n") 'comint-next-matching-input-from-input)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load packages and configure them

(use-package beacon
  :disabled
  :config
  (beacon-mode 1)
  (setq beacon-blink-delay 0.3))

(use-package cdlatex
  :ensure t
  :pin melpa-stable)

(use-package clang-format
  :ensure t
  :pin melpa)

(use-package dumb-jump
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
	 ("M-g j" . dumb-jump-go)
	 ("M-g x" . dumb-jump-go-prefer-external)
	 ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :pin melpa-stable)

(use-package expand-region
  :ensure t
  :config
  :bind (("C-=" . er/expand-region)))

(use-package ggtags
  :ensure t
  :config 
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1)))))

(use-package highlight-indent-guides
  :ensure t
  :config
  (progn
    (setq highlight-indent-guides-method 'column)
    (add-hook 'c-mode-hook 'highlight-indent-guides-mode)
    (add-hook 'c++-mode-hook 'highlight-indent-guides-mode)))

(use-package htmlize
  :ensure t
  :pin melpa-stable)

;; Mark and edit all copies of the marked region simultaneously. 
(use-package iedit
  :ensure t)

(use-package iflipb
  :ensure t
  :bind (("M-<prior>" . iflipb-next-buffer)
         ("M-<next>" . iflipb-previous-buffer))
  :pin melpa-stable)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-dispatch-popup))
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

(use-package pkgbuild-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("/PKGBUILD$" . pkgbuild-mode))
  :pin melpa-stable)

(use-package projectile  ; ... vomiting - awful name
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :pin melpa-stable)

(use-package rainbow-mode
  :ensure t
  :pin gnu)

(use-package realgud
  :ensure t
  :pin gnu)

(use-package smartparens
  :ensure t
  :init
  (progn
    (use-package smartparens-config)
    (use-package smartparens-html)
    (use-package smartparens-python)
    (use-package smartparens-latex)
    (smartparens-global-mode 1)
    (show-smartparens-global-mode 1)
    (setq sp-show-pair-delay 0))
  :config
  (progn
    (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p)))
  :bind
  (("M-<right>" . sp-slurp-hybrid-sexp)
   ("C-M-k" . sp-kill-sexp-with-a-twist-of-lime)
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

(use-package spice-mode
  :ensure t
  :mode (("\\.cir\\'" . spice-mode))
  :pin melpa)

;; Mike Zamansky's key bindings
;; (use-package smartparens
;;   :ensure t
;;   :config
;;   (use-package smartparens-config)
;;   (use-package smartparens-html)
;;   (use-package smartparens-python)
;;   (use-package smartparens-latex)
;;   (smartparens-global-mode t)
;;   (show-smartparens-global-mode t)
;;   :bind
;;   ( ("C-<down>" . sp-down-sexp)
;;     ("C-<up>"   . sp-up-sexp)
;;     ("M-<down>" . sp-backward-down-sexp)
;;     ("M-<up>"   . sp-backward-up-sexp)
;;     ("C-M-a" . sp-beginning-of-sexp)
;;     ("C-M-e" . sp-end-of-sexp)
;;     ("C-M-f" . sp-forward-sexp)
;;     ("C-M-b" . sp-backward-sexp)
;;     ("C-M-n" . sp-next-sexp)
;;     ("C-M-p" . sp-previous-sexp)
;;     ("C-S-f" . sp-forward-symbol)
;;     ("C-S-b" . sp-backward-symbol)
;;     ("C-<right>" . sp-forward-slurp-sexp)
;;     ("M-<right>" . sp-forward-barf-sexp)
;;     ("C-<left>"  . sp-backward-slurp-sexp)
;;     ("M-<left>"  . sp-backward-barf-sexp)
;;     ("C-M-t" . sp-transpose-sexp)
;;     ("C-M-k" . sp-kill-sexp)
;;     ("C-k"   . sp-kill-hybrid-sexp)
;;     ("M-k"   . sp-backward-kill-sexp)
;;     ("C-M-w" . sp-copy-sexp)
;;     ("C-M-d" . delete-sexp)
;;     ("M-<backspace>" . backward-kill-word)
;;     ("C-<backspace>" . sp-backward-kill-word)
;;     ([remap sp-backward-kill-word] . backward-kill-word)
;;     ("M-[" . sp-backward-unwrap-sexp)
;;     ("M-]" . sp-unwrap-sexp)
;;     ("C-x C-t" . sp-transpose-hybrid-sexp)
;;     ("C-c ("  . wrap-with-parens)
;;     ("C-c ["  . wrap-with-brackets)
;;     ("C-c {"  . wrap-with-braces)
;;     ("C-c '"  . wrap-with-single-quotes)
;;     ("C-c \"" . wrap-with-double-quotes)
;;     ("C-c _"  . wrap-with-underscores)
;;     ("C-c `"  . wrap-with-back-quotes)
;;     ))

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))

;; (use-package web-mode
;;   :ensure t
;;   :init
;;   (setq web-mode-enable-current-element-highlight t
;;         web-mode-enable-current-column-highlight t)
;;   :config
;;   (setq web-mode-engines-alist
;;         '(("php"    . "\\.phtml\\'")
;;           ("jinja2" . "\\.html\\'")))
;;   (setq web-mode-enable-auto-pairing nil)
;;   (defun my-web-mode-hook ()
;;     "Hooks for Web mode."
;;     (setq web-mode-markup-indent-offset 2))
;;   (add-hook 'web-mode-hook  'my-web-mode-hook)
;;   :mode
;;   (("\\.html?\\'"     . web-mode)
;;    ("\\.tmpl\\'"      . web-mode)
;;    ("\\.phtml\\'"     . web-mode)
;;    ("\\.tpl\\.php\\'" . web-mode)
;;    ("\\.[agj]sp\\'"   . web-mode)
;;    ("\\.as[cp]x\\'"   . web-mode)
;;    ("\\.erb\\'"       . web-mode)
;;    ("\\.mustache\\'"  . web-mode)
;;    ("\\.djhtml\\'"    . web-mode))
;;   :pin melpa-stable)

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

(require 'my-compilation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript

(use-package skewer-mode
  :ensure t
  :config
  (setq httpd-port 8088)
  :pin melpa)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++

(require 'my-c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python

(defun my-pydoc-info-symbol ()
  (interactive)
  (info-lookup-symbol (pydoc-info-python-symbol-at-point))
  (other-window 1))

(use-package pydoc-info
  :ensure t
  :init
  (progn
    (add-to-list 'load-path "/usr/share/info")
    (pydoc-info-add-help '("python" "matplotlib")))
  :bind (("<f1>" . my-pydoc-info-symbol))
  :pin melpa)

(use-package ein
  :ensure t
  :pin melpa)

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


;; (defun my-pytest-all (&optional flags)
;;   (interactive)
;;   (pytest-run nil flags)
;;   (other-window 1))

;; (defun my-pytest-module (&optional flags)
;;   (interactive)
;;   (pytest-run buffer-file-name flags)
;;   (other-window 1))

;; (defun my-pytest-one (&optional flags)
;;   (interactive)
;;   (pytest-run (format "%s" (pytest-py-testable)) flags)
;;   (other-window 1))

;; (defun my-pytest-directory (&optional flags)
;;   (interactive)
;;   (pytest-run (file-name-directory buffer-file-name) flags)
;;   (other-window 1))

;; (use-package pytest
;;   :ensure t
;;   :config
;;   (setq pytest-cmd-flags "-x -s -r a"
;;         pytest-use-verbose nil
;;         pytest-loop-on-failing nil
;;         pytest-assert-plain t)
;;   :pin melpa)

;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             ;; (local-set-key (kbd "C-c l") 'my-python-shell-send-line)
;;             (local-set-key (kbd "C-x C-e") 'python-shell-send-defun)
;;             (local-set-key (kbd "C-c a") 'my-pytest-all)
;;             (local-set-key (kbd "C-c m") 'my-pytest-module)
;;             (local-set-key (kbd "C-c o") 'my-pytest-one)
;;             (local-set-key (kbd "C-c d") 'my-pytest-directory)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Experimental


;; Fixing DOuble capitals as you type
;; http://endlessparentheses.com/fixing-double-capitals-as-you-type.html
;; (add-hook 'text-mode-hook #'dubcaps-mode)


;; Edit gmail messages in emacs using Markdown syntax
(use-package gmail-message-mode
  :disabled
  :pin melpa-stable)

(provide 'init)
;;; init.el ends here

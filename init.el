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

(use-package jedi
  :ensure t
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:setup-keys t)
  (setq jedi:complete-on-dot t)
  :pin melpa-stable)

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

;; (elpy-enable)
;; (elpy-use-ipython)

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

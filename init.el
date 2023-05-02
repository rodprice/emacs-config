;;; init.el --- Emacs initialization file
;;; Commentary:
;;; Code:


(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "mode/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "pkgs/" user-emacs-directory))

;; Load the site-specific preload file.
(load (concat (car (split-string (system-name) "\\.")) "-preload") 'noerror)

;; Have this available just in case
(setq confirm-kill-emacs 'y-or-n-p)
(defun server-shutdown ()
  "Save buffers, quit, and shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (when (bound-and-true-p desktop-mode)
    (desktop-kill))
  (kill-emacs))
(global-set-key (kbd "C-c x") 'server-shutdown)

;; Sane modifier keys for MacOS
(when (eq system-type 'darwin)
  (setq
   ns-command-modifier 'control
   ns-option-modifier 'meta
   ns-control-modifier 'control
   ns-function-modifier 'super))

;; Set up packaging
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package org
  :ensure t
  :pin manual
  :custom
  ;; (org-fold-catch-invisible-edits t)
  (org-element-use-cache nil) ;; org's cache get messed up easily, so don't use it
)
  ;; (org-babel-do-load-languages
  ;;  'org-babel-load-languages
  ;;  '((emacs-lisp . t)
  ;;    ;;(R . t)
  ;;    (python . t)
  ;;    ;;(ipython . t)
  ;;    (latex . t)
  ;;    (ditaa . t)
  ;;    (shell . t)
  ;;    (jupyter . t)))

;; Load the various settings files
;; (org-babel-jupyter-override-src-block 'python)
(org-babel-load-file (expand-file-name "~/.emacs.d/settings.org"))
;; (org-babel-load-file (expand-file-name "~/.emacs.d/settings-minimal.org"))
;; (org-babel-load-file (expand-file-name "~/.emacs.d/settings-theme.org"))
;; (org-babel-load-file (expand-file-name "~/.emacs.d/settings-global.org"))
;; (org-babel-load-file (expand-file-name "~/.emacs.d/settings-org-mode.org"))
;; (org-babel-load-file (expand-file-name "~/.emacs.d/settings-python.org"))

;; Load the site-specific postload file
(load (concat (car (split-string (system-name) "\\.")) "-postload") 'noerror)
(put 'upcase-region 'disabled nil)

;;; init.el --- Emacs initialization file
;;; Commentary:
;;; Code:


(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
;; (add-to-list 'load-path (expand-file-name "mode/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site/" user-emacs-directory))
;; (add-to-list 'load-path (expand-file-name "pkgs/" user-emacs-directory))

;; Load the site-specific preload file.
(load (concat (car (split-string (system-name) "\\.")) "-preload") 'noerror)

(defvar my-emacs-base-dir
  (file-name-as-directory
   (expand-file-name
    (concat invocation-directory "..")))
  "Base directory of Emacs installation.")

;; Decide whether to turn on debugging for use-package
;; From https://github.com/jwiegley/use-package/issues/768
(defvar init-file-debug t)
(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

;; Turn off annoying, useless warnings
(require 'warnings)
(setq warning-minimum-level :error)

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
(require 'my-functions)
(org-babel-load-file (expand-file-name "~/.emacs.d/settings-minimal.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/settings-markup.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/settings-internet.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/settings-completion.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/settings-shells.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/settings-programming.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/settings-elisp.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/settings-python.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/settings-cpp.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/settings-javascript.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/settings-julia.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/quote-of-the-day.org"))
;; (org-babel-jupyter-override-src-block 'python)

;; Load the site-specific postload file
(load (concat (car (split-string (system-name) "\\.")) "-postload") 'noerror)
(put 'upcase-region 'disabled nil)

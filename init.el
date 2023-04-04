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

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Org's cache gets messed up during initialization, so don't use it
(require 'org)
(setq org-element-use-cache nil)
;; Load the various settings files
(org-babel-load-file (expand-file-name "~/.emacs.d/settings.org"))
;; (org-babel-load-file (expand-file-name "~/.emacs.d/settings-minimal.org"))
;; (org-babel-load-file (expand-file-name "~/.emacs.d/settings-theme.org"))
;; (org-babel-load-file (expand-file-name "~/.emacs.d/settings-global.org"))
;; (org-babel-load-file (expand-file-name "~/.emacs.d/settings-org-mode.org"))
;; (org-babel-load-file (expand-file-name "~/.emacs.d/settings-python.org"))

;; Load the site-specific postload file
(load (concat (car (split-string (system-name) "\\.")) "-postload") 'noerror)
(put 'upcase-region 'disabled nil)

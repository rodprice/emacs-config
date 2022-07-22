;;; init.el --- Emacs initialization file
;;; Commentary:
;;; Code:


(add-to-list 'load-path (expand-file-name "init/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "mode/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "pkgs/" user-emacs-directory))

;; Load the site-specific preload file.
(load (concat (system-name) "-preload") 'noerror)

;; Have this available just in case
(defun server-shutdown () 
  "Save buffers, quit, and shutdown (kill) server" 
  (interactive) 
  (save-some-buffers) 
  (kill-emacs))
(global-set-key (kbd "C-c x") 'server-shutdown)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(org-babel-load-file (expand-file-name "~/.emacs.d/settings.org"))

;; Load the site-specific postload file
(load (concat (system-name) "-postload") 'noerror)
(put 'upcase-region 'disabled nil)

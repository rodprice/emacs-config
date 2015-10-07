;;; Package --- Init file for graphene setup for emacs
;;; Commentary:
;; Stop flycheck from whining about a commentary section
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load some site-specific configuration before anything else

(require 'functions)
(defvar dotfiles-dir (file-name-directory
                      (or (buffer-file-name) load-file-name))
  "The directory where the user's configuration files are kept.")

(load-site-file "-preload")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make emacs run the server so emacsclientw can connect

(require 'server)
(unless (eq (server-running-p) t)
  (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up use-package for use with MELPA

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load packages and configure them

(use-package graphene
  :ensure t
  :init
  (setq graphene-default-font "Consolas-11"))

(use-package hc-zenburn-theme
  :ensure t
  :config
  (load-theme 'hc-zenburn t))

(use-package ack
  :ensure t)
	     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load the rest of the site-specific configuration

(let* ((system-name-short (car (split-string system-name "\\.")))
       (site-path (concat dotfiles-dir "site/"))
       (file-name (concat site-path system-name-short "-postload")))
  ;; fail silently
  (load file-name 'noerror 'nomessage))

(provide 'init)
;;; init.el ends here

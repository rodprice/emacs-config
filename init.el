;;; Package --- Init file for graphene setup for emacs
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load some site-specific configuration before anything else

;; Set up load paths within .emacs.d
(add-to-list 'load-path (expand-file-name "init/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "mode/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "pkgs/" user-emacs-directory))

;; See README in ~/.emacs.d/site/
(load (concat system-name "-preload") 'noerror)

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
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     `("local" . ,(expand-file-name "pkgs/")) t)
(package-initialize)

;; See https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load packages and configure them

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t)
  :pin melpa-stable)

;; Needs setup (external perl install, perl code for ack)
(use-package ack
  :disabled t)

;; Highlight all regexps in a buffer
(use-package hi-lock
  :ensure t
  :bind (("M-o h" . highlight-regexp)
         ("M-o u" . unhighlight-regexp))
  :config
  (setq hi-lock-auto-select-face t)
;; Use instead bold Pink (standard error face)
  (setq hi-lock-face-defaults '("hi-black-b" "hi-pink")))

;; Placeholder for what looks like a great HTML, CSS, JavaScript dev package
;; See also https://truongtx.me/2014/02/23/set-up-javascript-development-environment-in-emacs/
(use-package skewer-mode
  :disabled t)

;; Disable this if you want to use skewer-mode
(use-package js-mode
  :disabled nil
  :mode "\\.json\\'")

(use-package whole-line-or-region
  :ensure t
  :bind ("C-w" . whole-line-or-region-kill-region)
  :pin melpa-stable)

;; Load my own initialization functions
(use-package functions
  :config
  (setq ring-bell-function 'echo-area-bell))

(use-package graphene
  :disabled t
  :init
  (setq graphene-default-font "Consolas-11")
  :pin melpa-stable)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load the rest of the site-specific configuration

;; See README in ~/.emacs.d/site/
(load (concat system-name "-postload") 'noerror)

(provide 'init)
;;; init.el ends here

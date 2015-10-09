;;; Package --- Init file for graphene setup for emacs
;;; Commentary:
;; Stop flycheck from whining about a commentary section
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load some site-specific configuration before anything else

(defvar dotfiles-dir
  (file-name-directory
   (or (buffer-file-name) load-file-name))
  "The directory where the user's configuration files are kept.")

;; Set up load paths within .emacs.d
(add-to-list 'load-path (concat dotfiles-dir "init/"))
(add-to-list 'load-path (concat dotfiles-dir "mode/"))
(add-to-list 'load-path (concat dotfiles-dir "site/"))

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
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Preferred setup according to https://github.com/jwiegley/use-package
;;(eval-when-compile
;;  (require 'use-package))
;;(require 'diminish)  ; if you use :diminish
;;(require 'bind-key)  ; if you use any :bind variant

(setq use-package-verbose t)

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

;; Needs setup (external perl install, perl code for ack)
(use-package ack
  :disabled t)

;; Highlight all regexps in a buffer
(use-package hi-lock
  :ensure t
  :bind (("M-o r" . highlight-regexp)
         ("M-o u" . unhighlight-regexp))
  :config
  (setq hi-lock-auto-select-face t)
  (setq hi-lock-face-defaults '("hi-black-b" "hi-pink")))

;; Placeholder for what looks like a great HTML, CSS, JavaScript dev package
;; See also https://truongtx.me/2014/02/23/set-up-javascript-development-environment-in-emacs/
(use-package skewer-mode
  :disabled t)

;; Disable this if you want to use skewer-mode
(use-package js-mode
  :disabled t
  :mode "\\.json\\'")

;; Load my own initialization functions
(use-package functions
  :config
  (setq ring-bell-function 'echo-area-bell))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load the rest of the site-specific configuration

;; See README in ~/.emacs.d/site/
(load (concat system-name "-postload") 'noerror)

(provide 'init)
;;; init.el ends here

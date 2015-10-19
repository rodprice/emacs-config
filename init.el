
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
;; Set up use-package for use with MELPA Stable and local archives

;; MELPA Stable archive
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; Local archive. This is for packages that are not available on the
;; MELPA Stable or GNU package archives, but may be available on
;; MELPA.  Simply adding MELPA to package-archives results in all
;; packages being downloaded from these archives. I prefer versioned
;; packages, so I set up this local archive to store local versions of
;; packages not available on GNU of MELPA Stable. It also gives me the
;; opportunity to clean up the mess all too often found in these
;; unversioned packages.
(require 'package-x)
(defvar local-archive
  (expand-file-name "local/" user-emacs-directory)
  "Location of the package archive for packages stored locally.")
(setq package-archive-upload-base local-archive)
(add-to-list 'package-archives `("local" . ,local-archive) t)

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

(use-package hc-zenburn-theme
  :ensure t
  :config
  (load-theme 'hc-zenburn t)
  :pin local)

(use-package discover
  :ensure t
  :config
  (global-discover-mode 1)
  :pin melpa-stable)

(use-package avy
  :disabled t
  :ensure t
  :bind (("C-'" . avy-goto-char-2)
         ("M-'" . avy-goto-word-1))
  :pin gnu)

;; Jump through Emacs buffers easily
(use-package ace-jump-mode
  :ensure t
  :bind ("C-c SPC" . ace-jump-mode)
  :config
  (setq ace-jump-mode-submode-list
        '(ace-jump-char-mode
          ace-jump-word-mode
          ace-jump-line-mode))
  :pin melpa-stable)

;; Placeholder for what looks like a great HTML, CSS, JavaScript dev package
;; See also https://truongtx.me/2014/02/23/set-up-javascript-development-environment-in-emacs/
(use-package skewer-mode
  :disabled t)

;; Disable this if you want to use skewer-mode
(use-package js-mode
  :disabled nil
  :mode "\\.json\\'")

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :pin melpa-stable)

(use-package whole-line-or-region
  :ensure t
  :bind ("C-w" . whole-line-or-region-kill-region)
  :pin melpa-stable)

;; Load my own initialization functions
(use-package functions
  :config
  (setq ring-bell-function 'echo-area-bell))

(use-package graphene
;  :disabled nil
  :load-path "pkgs/graphene-0.9.2/"
  :init
  (setq graphene-completion-auto nil)
  (setq graphene-default-font "Consolas-11"))
;  :pin melpa-stable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load the rest of the site-specific configuration

;; See README in ~/.emacs.d/site/
(load (concat system-name "-postload") 'noerror)

(provide 'init)
;;; init.el ends here

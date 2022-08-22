;;; init.el --- Emacs initialization file
;;; Commentary:
;;; Code:


;; Remove this if and when I want to use this config for real.
(add-to-list 'load-path (getenv "HOME"))
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;; Sane modifier keys for MacOS
(when (eq system-type 'darwin)
  (setq ns-command-modifier  'control
        ns-option-modifier   'meta
        ns-control-modifier  'control
        ns-function-modifier 'super))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(mwim smex expand-region whole-line-or-region counsel use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package vscode-dark-plus-theme
  :ensure t
  :config
  (load-theme 'vscode-dark-plus t))

;; Remove the border around the TODO word on org-mode files
(setq vscode-dark-plus-box-org-todo nil)

;; Do not set different heights for some org faces
(setq vscode-dark-plus-scale-org-faces nil)

;; Avoid inverting hl-todo face
(setq vscode-dark-plus-invert-hl-todo nil)

(setq nano-font-family-monospaced "Roboto Mono")
(setq nano-font-family-proportional nil)
(setq nano-font-size 13)


(require 'nano-layout)

;; Theme
(require 'nano-faces)
(nano-faces)

;; (require 'nano-theme)
;; (nano-theme)

(require 'nano-defaults)

;; Nano session saving (optional)
(require 'nano-session)

;; Nano header & mode lines (optional)
(require 'nano-modeline)

;; Nano key bindings modification (optional)
(require 'nano-bindings)

;; Compact layout (need to be loaded after nano-modeline)
(when (member "-compact" command-line-args)
  (require 'nano-compact))
  
;; Help (optional)
(unless (member "-no-help" command-line-args)
  (require 'nano-help))

(setq visible-bell t)
(global-linum-mode t)

(require 'my-functions)
(global-set-key (kbd "C-;") 'my-insert-semicolon)
(global-set-key (kbd "M-j") 'my-join-lines)
(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "M-o") 'open-previous-line)
(global-set-key (kbd "C-t") 'transpose-next-line)
(global-set-key (kbd "M-t") 'transpose-previous-line)

(use-package mwim
  :ensure t
  :bind (("C-a"      . mwim-beginning-of-line-or-code)
         ("C-e"      . mwim-end-of-line-or-code)
         ("<home>"   . mwim-beginning-of-line-or-code)
         ("<end>"    . mwim-end-of-line-or-code)
         ("M-<"      . scroll-row-up)
         ("M->"      . scroll-row-down)
         ("M-<up>"   . scroll-row-up)
         ("M-<down>" . scroll-row-down))
  :pin melpa)

(use-package whole-line-or-region
  :ensure t
  :bind (("C-w" . whole-line-or-region-kill-region)
         ("M-w" . whole-line-or-region-copy-region-as-kill))
  :pin melpa-stable)

(use-package expand-region
  :ensure t
  :config
  :bind (("C-=" . er/expand-region)))

(use-package smex
  :ensure t
  :pin melpa-stable)

(use-package counsel
  :ensure t
  :after (smex)
  
  :config
  (require 'smex)
  (setq ivy-height 4)
  (setq ivy-count-format "")
  (setq ivy-initial-inputs-alist: '((counsel-minor .            "^+")
                                    (counsel-package .          "^+")
                                    (counsel-org-capture .      "^")
                                    (counsel-M-x .              "^")
                                    (counsel-describe-symbol .  "^")
                                    (org-refile .               "") 
                                    (org-agenda-refile .        "")
                                    (org-capture-refile .       "")
                                    (Man-completion-table .     "^")
                                    (woman .                    "^")))
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  
  :bind (("C-c r"   . counsel-recentf)
         ("C-c b"   . counsel-bookmark)
         ("C-x C-b" . counsel-switch-buffer)
         ("C-c c"   . counsel-org-capture))
  :pin melpa-stable)

;; Nano counsel configuration (optional)
;; Needs "counsel" package to be installed (M-x: package-install)
;; (require 'nano-counsel)


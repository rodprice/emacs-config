;;; init.el --- Emacs initialization file
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set a few paths first

;; Set up load paths within .emacs.d
(add-to-list 'load-path (expand-file-name "init/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "mode/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "pkgs/" user-emacs-directory))

;; Get some utilities for manipulating Windows paths
(require 'my-application-paths)

;; Declare a few paths to external applications used by Emacs. These
;; paths are specific to each site (meaning machine and operating
;; system), and so should be set in the site/*-preload.el file. The
;; paths stored in the path variables will be prepended to `exec-path'
;; in the order that they are defined below. See the site/README.md
;; file for details.
(defvar my-path-variables nil
  "A list of path variables to be prepended to $PATH and `exec-path'.")
(defvar my-git-for-windows-dir nil
  "The path to the top directory of my git installation.")
(add-to-list 'my-path-variables 'my-git-for-windows-dir)
(defvar my-msys-binaries-dir nil
  "The path to the bin directory of my MSYS installation.")
(add-to-list 'my-path-variables 'my-msys-binaries-dir)
(defvar my-anaconda-dir nil
  "The path to the top directory of my Python Anaconda distribution.")
(add-to-list 'my-path-variables 'my-anaconda-dir)
(defvar my-anaconda-scripts-dir nil
  "The path to the scripts directory of my Python Anaconda distribution.")
(add-to-list 'my-path-variables 'my-anaconda-scripts-dir)
(defvar my-mathematica-kernel-dir nil
  "The path to the Kernel/Binaries directory of my Mathematica installation.")
(add-to-list 'my-path-variables 'my-mathematica-kernel-dir)
(defvar my-mathematica-license-dir nil
  "The path to the $BaseDirectory/Licensing directory of my Mathematica installation.")

;; Load the site-specific preload file
(load (concat system-name "-preload") 'noerror)

;; Prepend the contents of `my-path-variables' to `exec-path'.
(setq exec-path
      (let ((my-paths (mapcar 'symbol-value my-path-variables)))
        (my-concat-paths my-paths exec-path)))
;; Make the environment variable $PATH match `exec-path'
(setenv "PATH" (mapconcat 'identity exec-path ";"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make emacs run the server so emacsclientw can connect

(require 'server)
(unless (server-running-p)
  (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up use-package for use with MELPA and local archives

;; MELPA and MELPA Stable archives
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

;; Local archive. This is for packages that are not available on the
;; MELPA Stable or GNU package archives, but may be available on
;; MELPA.  Simply adding MELPA to package-archives results in all
;; packages being downloaded from these archives. I prefer versioned
;; packages, so I set up this local archive to store local versions of
;; packages not available on GNU or MELPA Stable. It also gives me the
;; opportunity to clean up the mess all too often found in these
;; unversioned packages.
(require 'package-x)
(defvar local-archive
  (expand-file-name "local/" user-emacs-directory)
  "Location of the package archive for packages stored locally.")
(unless (file-exists-p local-archive)
  (mkdir local-archive))
(setq package-archive-upload-base local-archive)
(add-to-list 'package-archives `("local" . ,local-archive) t)

;; Upload packages from current contents of pkgs directory
;; http://emacs.stackexchange.com/questions/19068/correct-usage-of-package-upload-file-for-multi-file-package
;; copy of tar files is necessary due to bug in package-x.el lines 246-252,
;; function package-upload-buffer-internal. It copies the directory
;; listing rather than the tar file itself.  In fact, the author just
;; ignored the tar file case altogether!
;; (dolist (file (directory-files myelpa-msde 'fqn "\\.*[.]\\(el\\|tar\\)"))
;;   (message "Preparing %s" file)
;;   (package-upload-file file)
;;   (when (string= (file-name-extension file) "tar")
;;     (copy-file file myelpa 'force)))

(package-initialize)

;; See https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'diminish)
(setq use-package-verbose t)
;;(setq use-package-debug t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load packages and configure them

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-tomorrow-night t)
  (let ((base00 (plist-get base16-tomorrow-night-colors :base00))
        (base01 (plist-get base16-tomorrow-night-colors :base01))
        (base02 (plist-get base16-tomorrow-night-colors :base02))
        (base03 (plist-get base16-tomorrow-night-colors :base03))
        (base04 (plist-get base16-tomorrow-night-colors :base04))
        (base05 (plist-get base16-tomorrow-night-colors :base05))
        (base06 (plist-get base16-tomorrow-night-colors :base06))
        (base07 (plist-get base16-tomorrow-night-colors :base07))
        (base08 (plist-get base16-tomorrow-night-colors :base08))
        (base09 (plist-get base16-tomorrow-night-colors :base09))
        (base0A (plist-get base16-tomorrow-night-colors :base0A))
        (base0B (plist-get base16-tomorrow-night-colors :base0B))
        (base0C (plist-get base16-tomorrow-night-colors :base0C))
        (base0D (plist-get base16-tomorrow-night-colors :base0D))
        (base0E (plist-get base16-tomorrow-night-colors :base0E))
        (base0F (plist-get base16-tomorrow-night-colors :base0F)))
    (setq face-remapping-alist
          `((show-paren-match
             . (:foreground ,base0D
                :background ,base02))
            (show-paren-mismatch
             . (:strike-through t
                :foreground ,base09
                :background ,base02))
            (font-lock-keyword-face
             . (:foreground ,base0E
                :weight bold))
            (font-lock-function-name-face
             . (:foreground "cornflower blue"))
            (font-lock-doc-face
             . (:foreground ,base0C))
            (font-lock-string-face
             . (:foreground "slate gray"))
            (font-lock-comment-face
             . (:foreground "light slate gray"))
            (font-lock-comment-delimiter-face
             . (:foreground "light slate gray"))
            (aw-leading-char-face
             . (:height 2.0 :foreground "cornflower blue")))))
  :pin melpa-stable)

(use-package ace-window
  :ensure t
  :config
  (global-set-key [remap other-window] 'ace-window))

(use-package popwin
  :ensure t
  :config
  (popwin-mode 1)
  :pin melpa)

(use-package direx
  :ensure t
  :config
  (push '(direx:direx-mode
          :position left
          :width 25
          :dedicated t)
        popwin:special-display-config)
  :bind (("C-x C-j" . direx:jump-to-directory-other-window))
  :pin melpa)

(use-package discover
  :ensure t
  :config
  (global-discover-mode 1)
  :pin melpa-stable)

(use-package avy
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

;; Flip between buffers
;; TODO: Figure out how to flip windows this way
;; TODO: Configure to include *Python* etc buffers optionally
(use-package iflipb
  :ensure t
  :bind (("M-<prior>" . iflipb-next-buffer)
         ("M-<next>" . iflipb-previous-buffer))
  :pin melpa-stable)

(use-package whole-line-or-region
  :ensure t
  :bind (("C-w" . whole-line-or-region-kill-region)
         ("M-w" . whole-line-or-region-copy-region-as-kill)
         ("C-y" . whole-line-or-region-yank))
  :pin melpa-stable)

;; Configure M-; to align comments etc the way I want
;; (use-package newcomment
;;   :disabled t
;;   :config
;;   (setq comment-styles "read the documentation for this variable"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages required for graphene

(use-package dash
  :ensure t
  :pin melpa-stable)

(use-package exec-path-from-shell
  :ensure t
  :pin melpa-stable)

(use-package ido-ubiquitous
  :ensure t
  :pin melpa-stable)

(use-package smex
  :ensure t
  :pin melpa-stable)

(use-package web-mode
  :ensure t
  :pin melpa-stable)

(use-package smartparens
  :ensure t
  :bind (("M-<right>" . sp-forward-slurp-sexp)
         ("M-<left>"  . sp-forward-barf-sexp))
  :pin melpa-stable)

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))

;; Highlight current line
(global-hl-line-mode t)

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

(use-package expand-region
  :ensure t
  :config
  :bind (("C-=" . er/expand-region)))

;; Flycheck uses standard error navigation commands of Emacs, `M-g n'
;; for `next-error' and 'M-g p' for `previous-error'. See section 4.4
;; of the Flycheck manual.
(use-package flycheck
  :ensure t
  :pin melpa-stable)

(use-package graphene
  :load-path "pkgs/graphene-0.9.2/"
  :init
  (setq graphene-completion-auto nil)
  (setq graphene-default-font "Consolas-12"))

;; Load my own initialization functions
(use-package my-functions
  :config
  (setq ring-bell-function 'echo-area-bell)
  :pin manual)

(use-package smartscan
  :ensure t
  :config
  (global-smartscan-mode 1))

(use-package rainbow-mode
  :ensure t
  :pin gnu)

;; Belongs in *-look.el file
(global-visual-line-mode 0)
(setq-default truncate-lines t)
(fset 'yes-or-no-p 'y-or-n-p)

;; Backup file configuration
;; http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files?rq=1
(setq delete-old-versions t
      kept-new-versions 2
      kept-old-versions 2
      version-control t)

;; File locations for custom settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (custom-save-all))                    ; Create new, empty custom file
(load custom-file)

;; Control the window in which Emacs visits a new file
(setq ido-default-file-method 'raise-frame)
;; Don't pop up window of completions
(setq ido-cannot-complete-command 'ido-next-match)

;; When opening a help window, always select the new help window
(setq help-window-select t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major modes

;; See https://www.gnu.org/software/emacs/manual/html_node/efaq/Installing-Texinfo-documentation.html
(require 'info)
(info-initialize)                       ; populate Info-directory-list

;; setup article http://www.seaandsailor.com/emacs-config.html
(use-package matlab-mode
  ;; :ensure t
  :init
  ;; Remove binding for .m files to objc-mode; bind to matlab-mode
  (setq auto-mode-alist
        (cons '("\\.m\\'" . matlab-mode)
              (remove-all-matches-from-alist ".m" auto-mode-alist)))
  :pin local)

(use-package json-mode
  :disabled t
  :config
  (defun pretty-print-json(&optional b e)
    "Shells out to Python to pretty print JSON."
    (interactive "r")
    (shell-command-on-region b e "python -m json.tool" (current-buffer) t))
  :pin local)

(use-package my-emacs-lisp
  :config
  (setq eldoc-argument-case 'fontify-eldoc-argument-list)
  :pin manual)

;; Emacs interface to git
(require 'my-magit)

;; Python programming mode and tools
;; (use-package my-python
;;   :demand
;;   :bind (("C-c C-c" . my-python-shell-send))
;;   :pin manual)
(require 'my-python)

(require 'my-org-mode)
(use-package org-pomodoro
  :ensure t
  :config
  (setq org-pomodoro-format "pom~%s")
  :pin melpa-stable)

;; Mathematica programming mode
(use-package my-wolfram
  :pin local)

;; Placeholder for what looks like a great HTML, CSS, JavaScript dev
;; package.  See also URL
;; `https://truongtx.me/2014/02/23/set-up-javascript-development-environment-in-emacs/'.
(use-package skewer-mode
  :disabled t)

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
  :ensure t
  :defer t
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
;; Global key bindings

(global-set-key (kbd "M-j") 'my-join-lines)
(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "M-o") 'open-previous-line)
(global-set-key (kbd "M-<up>") 'scroll-row-up)
(global-set-key (kbd "M-<down>") 'scroll-row-down)
(global-set-key (kbd "C-x p") 'my-rotate-windows)
(global-set-key (kbd "<f5>") 'revert-buffer)

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

(global-set-key (kbd "C-<up>") 'xah-backward-block)
(global-set-key (kbd "C-<down>") 'xah-forward-block)

;; Window creation and manipulation
(global-set-key (kbd "C-x C-o") 'other-frame)
(global-set-key (kbd "M-p") 'my-rearrange-windows)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings for prog-mode, shell-mode, etc.

(add-hook 'comint-mode-hook
          (lambda ()
            (define-key comint-mode-map
              (kbd "C-d") 'my-comint-delchar-or-eof-or-kill-buffer)
            (define-key comint-mode-map
              (kbd "C-<up>") 'comint-previous-matching-input-from-input)
            (define-key comint-mode-map
              (kbd "C-p") 'comint-previous-matching-input-from-input)
            (define-key comint-mode-map
              (kbd "C-<down>") 'comint-next-matching-input-from-input)
            (define-key comint-mode-map
              (kbd "C-n") 'comint-next-matching-input-from-input)))

(add-hook 'python-mode-hook
          (lambda ()
            ;; (local-set-key (kbd "C-c l") 'my-python-shell-send-line)
            (local-set-key (kbd "C-x C-e") 'python-shell-send-defun)
            (local-set-key (kbd "C-c m") 'my-pytest-module)
            (local-set-key (kbd "C-c o") 'my-pytest-one)
            (local-set-key (kbd "C-c d") 'my-pytest-directory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Experimental stuff

;; Use Firefox rather than Internet Explorer
(setq browse-url-browser-function 'browse-url-firefox
      browse-url-firefox-program
      "C:\\Users\\rodprice\\AppData\\Local\\Mozilla Firefox\\firefox.exe")
(setq org-file-apps
      `((auto-mode . emacs)
        ("\\.mm\\'" . default)
        ("\\.x?html\\'" . ,browse-url-firefox-program)
        ("\\.pdf\\'" . default)))

;; Display the column number in the mode line
(setq column-number-mode t)

;; In some buffers, you can hit `q' to restore the previous window.
;; Normally, this just buries the buffer, leaving a bunch of clutter
;; around.  If you want to kill the buffer, you have to hit `C-u q'.
;; This function reverses the key sequence: hitting `q' kills the
;; buffer, and `C-u q' buries it.
(defun my-quit-window (&optional bury window)
  "This function kills the window rather than burying it by default."
  (interactive "P")
  (quit-restore-window window (if bury 'bury 'kill)))
(fset 'quit-window 'my-quit-window)

(defun my-beginning-of-sexp ()
  "Move to beginning of sexp, unless previous invocation did not result
in movement of point.  In that case, move one character to the left."
  (interactive)
  (let ((posn (point)))
    (sp-beginning-of-sexp)
    (if (eq posn (point))
        (backward-char))))

(defun my-end-of-sexp ()
  "Move to end of sexp, unless previous invocation did not result
in movement of point.  In that case, move one character to the right."
  (interactive)
  (let ((posn (point)))
    (sp-end-of-sexp)
    (if (eq posn (point))
        (forward-char))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load the rest of the site-specific configuration

;; See README in ~/.emacs.d/site/
(load (concat system-name "-postload") 'noerror)

(provide 'init)
;;; init.el ends here

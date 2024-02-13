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

;; Patch for "invalid image type 'svg'" error. See
;; https://github.com/caldwell/build-emacs/issues/126
(when (and
       (< emacs-major-version 29)
       (eq system-type 'darwin))
  (add-to-list 'image-types 'svg))

;; Set up packaging
(require 'package)
(setq package-enable-at-startup nil)

;; See https://emacs.stackexchange.com/questions/60278/gpg-no-public-key.
;; Also https://gnupg.org/gph/en/manual.html
;; The MSYS2 version of `gpg' doesn't like Windows paths, so we
;; substitute a Unix-like path that `gpg' can deal with.
(when (eq system-type 'windows-nt)
  (setq package-gnupghome-dir (concat package-user-dir "/gnupg")))


(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/"))
(package-initialize)
(if (gnutls-available-p)
    (package-refresh-contents)
  (warn "Can't reach repositories: GnuTLS is not available"))

;; See https://elpa.gnu.org/packages/gnu-elpa-keyring-update.html.
;; package.el uses keys to verify packages upon installation, but
;; these keys are only good for a year. The `gnu-elpa-keyring-update'
;; package needs to be re-installed when the keys expire.
(unless (package-installed-p 'gnu-elpa-keyring-update)
  (let ((package-check-signature nil))  ;; keys are expired already
    (package-install 'gnu-elpa-keyring-update)))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Decide whether to turn on debugging for use-package. From
;; https://github.com/jwiegley/use-package/issues/768
(when init-file-debug
  (setq use-package-verbose t
        use-package-expand-minimally nil
        use-package-compute-statistics t
        debug-on-error t))

;; Set up exec-path, unless on Windows
(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))

;; Temporary fix
(setq help-window-select t)

(use-package org
  :ensure t
  :pin manual
  :custom
  ;; (org-fold-catch-invisible-edits t)
  (org-element-use-cache nil) ;; org's cache get messed up easily, so don't use it
  )

;; Adapted from https://martinralbrecht.wordpress.com/2020/08/23/conda-jupyter-and-emacs/
(use-package ob
  :disabled
  :ensure nil
  :config
  (progn
    ;; Load more languages for org-babel
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       (shell . t)
       (latex . t)
       (C . t)
       (makefile . t)
       (jupyter . t)))          ; must be last

    ;; Make org-babel fontify more languages
    (add-to-list 'org-src-lang-modes
                 '("conf" . conf))

    (setq org-babel-default-header-args:sh    '((:results . "output replace"))
          org-babel-default-header-args:bash  '((:results . "output replace"))
          org-babel-default-header-args:shell '((:results . "output replace"))
          org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                         (:session . "py")
                                                         (:kernel . "sagemath"))
          org-confirm-babel-evaluate nil)))

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
(org-babel-load-file (expand-file-name "~/.emacs.d/settings-octave.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/quote-of-the-day.org"))
;; (org-babel-jupyter-override-src-block 'python)

;; Load the site-specific postload file
(load (concat (car (split-string (system-name) "\\.")) "-postload") 'noerror)
(put 'upcase-region 'disabled nil)

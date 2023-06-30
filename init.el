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

;; Set up packaging
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/"))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Another approach
;; https://stackoverflow.com/questions/5701388/where-can-i-find-the-public-key-for-gnu-emacs
(defun my-keyring-update-age ()
  "Return the age in months of the public key for package.el."
  (require 'calendar)
  (let* ((desc (alist-get 'gnu-elpa-keyring-update package-alist))
         (date (calendar-current-date))
         (year (nth 2 date))
         (month (car date)))
    (if (null desc)
        nil
      (let* ((pkg-date (package-desc-version (car desc)))
             (pkg-year (car pkg-date))
             (pkg-month (cadr pkg-date)))
        (if (> pkg-month month)
            (- (* 12 (- year pkg-year))
               (- 12 (- pkg-month month)))
          (+ (* 12 (- year pkg-year))
             (- month pkg-month)))))))

;; See https://emacs.stackexchange.com/questions/60278/gpg-no-public-key.
;; The MSYS2 version of `gpg' doesn't like Windows paths, so we
;; substitute a Unix-like path that `gpg' can deal with.
(when (eq system-type 'windows-nt)
  (setq package-gnupghome-dir (concat package-user-dir "/gnupg")))

(defun my-keyring-modification-time ()
  "Return the time the keyring file was last modified."
  (file-attribute-modification-time
   (file-attributes
    (concat package-gnupghome-dir "/pubring.kbx")
    'string)))

;; See https://elpa.gnu.org/packages/gnu-elpa-keyring-update.html.
;; package.el uses keys to verify packages upon installation, but
;; these keys are only good for a year. The `gnu-elpa-keyring-update'
;; package needs to be re-installed when the keys expire.
(unless (package-installed-p 'gnu-elpa-keyring-update)
  (let ((package-check-signature nil))  ;; keys are expired already
    (package-install 'gnu-elpa-keyring-update)))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Decide whether to turn on debugging for use-package. From
;; https://github.com/jwiegley/use-package/issues/768
(defvar init-file-debug t)
(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

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
    ;; load more languages for org-babel
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       (shell . t)
       (latex . t)
       (C . t)
       (makefile . t)
       (jupyter . t)))          ; must be last

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
(org-babel-load-file (expand-file-name "~/.emacs.d/quote-of-the-day.org"))
;; (org-babel-jupyter-override-src-block 'python)

;; Load the site-specific postload file
(load (concat (car (split-string (system-name) "\\.")) "-postload") 'noerror)
(put 'upcase-region 'disabled nil)

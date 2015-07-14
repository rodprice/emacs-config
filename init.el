;;; init.el --- Configure emacs

;; Copyright (C) 2010

;; Author:  <rod@thirdoption.info>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Rodney Price, April 2010, July 2015

;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;;; Code:


;; These will be used in every session
(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

(defvar dotfiles-dir (file-name-directory
                      (or (buffer-file-name) load-file-name))
  "The directory where the user's configuration files are kept.")

(defvar msys-home ""
  "The path to the user's MSYS installation.")

;; Set up the emacs load path
(add-to-list 'load-path (concat dotfiles-dir "init"))
(add-to-list 'load-path (concat dotfiles-dir "base"))
(add-to-list 'load-path (concat dotfiles-dir "site"))
(add-to-list 'load-path (concat dotfiles-dir "mode"))

;; Init... Set up window appearance, color theme, etc
(require 'functions)
(include 'settings)
(include 'bindings)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load site-specific configuration

(setq system-name-short
      (concat (car (split-string system-name "\\.")) "-preload"))
(include (intern system-name-short))
(unintern system-name-short)  ; remove symbol to avoid name collisions later

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make emacs run the server so emacsclientw can connect

(require 'server)
(unless (server-running-p)
  (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tell emacs a few things about myself and its environment

(setq user-full-name "Rodney Price")
;; TODO change email depending on where I am
(setq user-mail-address "rod@thirdoption.info")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use packages from ELPA and MELPA whenever possible

(defvar my-packages
  '(auto-complete
    autopair
    dired-details+
    hc-zenburn-theme
    hippie-exp
    pager
    whole-line-or-region
    yasnippet)
  "A list of packages that should always be available.")

;; Use the MELPA as well as the GNU package archives
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
;; Don't run twice. See http://stackoverflow.com/questions/11127109/emacs-24-package-system-initialization-problems
(setq package-enable-at-startup nil)
(package-initialize)
;; Make sure that everything in `my-packages` is loaded
(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window appearance

(load-theme 'hc-zenburn t)

;; Make the default font readable for my old eyes
(set-face-attribute
 'default nil
 :font "-outline-Lucida Console-normal-normal-normal-mono-16-*-*-*-c-*-iso8859-1")

;; Load automatically generated lisp files
(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(load autoload-file 'noerror)
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)

;; Base... Include user-written packages to be used in every session
(include 'my-autopair)
(include 'my-bookmarks)
;; (include 'my-dired)
(include 'my-ediff)
(include 'my-info)
(include 'my-occur)
(include 'my-pager)
(include 'my-shell-script)
;(include 'my-tab)
(include 'my-wlor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-complete configuration
;; TODO integrate with my-tab and my-minitab

;; Autocomplete configuration
(require 'auto-complete-config)
(ac-config-default)                     ; perhaps should be mode-specific
(setq ac-auto-start nil)                ; don't start automatically
(define-key ac-mode-map                 ; TAB starts autocompletion
  (kbd "TAB") 'auto-complete)
;; (setq-default
;;  ac-sources
;;  '(ac-source-abbrev
;;    ac-source-dictionary
;;    ac-source-words-in-same-mode-buffers))

;; TODO move these hooks to mode-specific setup
(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
(add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
(add-hook 'css-mode-hook 'ac-css-mode-setup)
(add-hook 'auto-complete-mode-hook 'ac-common-setup)
(global-auto-complete-mode t)

;; Clojure CIDER mode
;; (require 'ac-cider)
;; (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
;; (add-hook 'cider-mode-hook 'ac-cider-setup)
;; (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
;; (eval-after-load "auto-complete"
;;   '(progn
;;      (add-to-list 'ac-modes 'cider-mode)
;;      (add-to-list 'ac-modes 'cider-repl-mode)))
;; (defun set-auto-complete-as-completion-at-point-function ()
;;   (setq completion-at-point-functions '(auto-complete)))
;; (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
;; (add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)

(setq system-name-short
      (concat (car (split-string system-name "\\.")) "-postload"))
(include (intern system-name-short))
(unintern system-name-short)  ; remove symbol to avoid name collisions later


(provide 'init)
;;; init.el ends here

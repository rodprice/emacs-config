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


(defvar dotfiles-dir (file-name-directory
                      (or (buffer-file-name) load-file-name))
  "The directory where the user's configuration files are kept.")

(defvar msys-home ""
  "The path to the user's MSYS installation.")

(defvar my-packages
  '(auto-complete
    autopair
    clojure-mode
    discover
    hc-zenburn-theme
    hlinum
    hydra
    iflipb
    inf-clojure
    lispy
    markdown-mode
    omn-mode
    pager
    paredit
    paredit-menu
    whole-line-or-region
    yasnippet)
  "A list of packages that should always be available.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load some site-specific configuration before anything else

(let* ((system-name-short (car (split-string system-name "\\.")))
       (site-path (concat dotfiles-dir "site/"))
       (file-name (concat site-path system-name-short "-preload")))
  ;; fail silently
  (load file-name 'noerror 'nomessage))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make emacs run the server so emacsclientw can connect

(require 'server)
(unless (server-running-p)
  (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up the emacs load path

(add-to-list 'load-path (concat dotfiles-dir "init"))
(add-to-list 'load-path (concat dotfiles-dir "base"))
(add-to-list 'load-path (concat dotfiles-dir "mode"))
(add-to-list 'load-path (concat dotfiles-dir "other"))

;; Init... Set up window appearance, color theme, etc
(require 'functions)
(include 'settings)
(include 'bindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tell emacs a few things about myself and its environment

(setq user-full-name "Rodney Price")
;; TODO change email depending on where I am
(setq user-mail-address "rod@thirdoption.info")

;; Load automatically generated lisp files
(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(load autoload-file 'noerror)
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)

;; Base... Include user-written packages to be used in every session
(include 'my-autopair)
(include 'my-bookmarks)
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

;; yasnippet configuration
(require 'yasnippet)
(yas-global-mode t)
;; Remove yasnippet's default tab key binding
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
;; Distinguish C-i from TAB
(define-key input-decode-map [?\C-i] [C-i])
;; Bind C-i to yasnippet
(define-key yas-minor-mode-map (kbd "<C-i>") 'yas-expand)

;; Autocomplete configuration
(require 'auto-complete-config)
(ac-config-default)                     ; perhaps should be mode-specific
(setq ac-auto-start nil)                ; don't start automatically
(define-key ac-mode-map                 ; TAB starts autocompletion
  [tab] 'auto-complete)
;; (setq-default
;;  ac-sources
;;  '(ac-source-abbrev
;;    ac-source-dictionary
;;    ac-source-words-in-same-mode-buffers))

;; TODO move these hooks to mode-specific setup
(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
(add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
(add-hook 'css-mode-hook 'ac-css-mode-setup)
(add-hook 'auto-complete-mode-hook 'ac-common-setup)
(global-auto-complete-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure major modes

(include 'my-clojure-mode)
(include 'my-elisp-mode)
(include 'my-org-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load the rest of the site-specific configuration

(let* ((system-name-short (car (split-string system-name "\\.")))
       (site-path (concat dotfiles-dir "site/"))
       (file-name (concat site-path system-name-short "-postload")))
  ;; fail silently
  (load file-name 'noerror 'nomessage))


(provide 'init)
;;; init.el ends here

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make emacs run the server so emacsclientw can connect

(require 'server)
(unless (server-running-p)
  (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use packages from ELPA and MELPA whenever possible

(defvar my-packages
  '(hc-zenburn-theme
    autopair
    dired-details+
    pager
    whole-line-or-region
    hippie-exp
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

;; Set up load path
;; (add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "site-lisp"))
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp")

;; These will be used in every session
;; (require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

;; Load automatically generated lisp files
(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(load autoload-file 'noerror)
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)

;; Init... Set up window appearance, color theme, etc
(add-to-list 'load-path (concat dotfiles-dir "init"))
(add-to-list 'exec-path (concat dotfiles-dir "scripts"))
(require 'functions)                    ; load the include function
(include 'settings)
(include 'bindings)

;; Base... Include user-written packages to be used in every session
(add-to-list 'load-path (concat dotfiles-dir "base"))
(include 'my-autopair)
(include 'my-bookmarks)
(include 'my-chrome)
(include 'my-dired)
(include 'my-ediff)
(include 'my-info)
(include 'my-occur)
(include 'my-pager)
;(include 'my-point-stack)
(include 'my-shell-script)
(include 'my-tab)
(include 'my-wlor)

;; Load system-specific configuration
(message "\nCustomizations for %s" system-name)
(setq system-name-short (car (split-string system-name "\\.")))
(include (intern system-name-short))
(unintern system-name-short)  ; remove symbol to avoid name collisions later

(provide 'init)
;;; init.el ends here

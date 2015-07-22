;;; settings.el --- Quick one- or two-line emacs settings.

;; Copyright (C) 2010

;; Author:  <rod@thirdoption.info>
;; Keywords: lisp, local

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

;;; Rod Price, April 2010

;;; Code:

(require 'functions)

;; These will be used in every session
(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

;; Modes that I haven't configured yet
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$"  . diff-mode))
(add-to-list 'auto-mode-alist '("\\.css$"          . css-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml$"        . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.rb$"           . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$"        . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$"  . js2-mode))
(add-to-list 'auto-mode-alist '("\\.xml$"          . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.gitconfig$"    . conf-mode))
(add-to-list 'auto-mode-alist '("\\.make$"         . makefile-mode))
(add-to-list 'auto-mode-alist '("[mM]akefile"      . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.s43"           . gas-mode))

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
    (package-refresh-contents)          ; get latest version
    (package-install p)))

;; Use UTF-8 throughout
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Set the appearance of emacs at startup
(tool-bar-mode -1)
(menu-bar-mode 1)
(setq inhibit-startup-message t
      line-number-mode 1
      column-number-mode 1
      default-truncate-lines t
      truncate-partial-width-windows nil
      uniquify-buffer-name-style 'forward)

;; Show line numbers in buffers
(require 'linum)
(linum-on)

;; Flip through buffers with arrow keys
(require 'iflipb)
(setq iflipb-wrap-around t)
(global-set-key (kbd "<C-kp-right>") 'iflipb-next-buffer)
(global-set-key (kbd "<C-kp-6>") 'iflipb-next-buffer)
(global-set-key (kbd "<C-kp-left>") 'iflipb-previous-buffer)
(global-set-key (kbd "<C-kp-4>") 'iflipb-next-buffer)

;; When creating a new file from ido-switch-buffer,
;; use the appropriate major mode
(setq-default major-mode 'major-mode-from-name)

(require 'discover)
(global-discover-mode 1)

;; If no other mode is specified, use text mode.
(setq default-major-mode 'text-mode)
;; Short answers always, please
(defalias 'yes-or-no-p 'y-or-n-p)
;; Seed the random-number generator
(random t)
;; Flash minibuffer rather than beeping on errors
(setq ring-bell-function 'echo-area-bell)
;; Transparently open compressed files
(auto-compression-mode t)
;; Save a list of recent files visited.
(recentf-mode 1)
;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)
;; Use the mouse wheel
(mouse-wheel-mode t)
(xterm-mouse-mode t)
;; How to designate a region
(setq transient-mark-mode t)
(setq shift-select-mode nil)
;; Keep windows configurations on a stack for easy restoration
(winner-mode 1)
;; Transparently open compressed files
(auto-compression-mode t)
;; Save a list of recent files visited.
(recentf-mode 1)
;; Default to unified diffs
(setq diff-switches "-u")
;; Don't prompt for location of TAGS
(setq tags-file-name "TAGS")
;; Tell emacs which browser to use
(setq browse-url-browser-function 'browse-url-chrome)
;;(setq browse-url-browser-function 'browse-url-firefox)
;;(setq browse-url-browser-function 'w3m-browse-url)

;; Put the places file inside the emacs subdir
(setq-default save-place t)
(setq save-place-file (concat dotfiles-dir "places"))

;; Put backups in a directory inside the emacs subdir
(add-to-list 'backup-directory-alist
             `("." . ,(concat dotfiles-dir "backup")))

;; Enforce no tabs, 80-column width, no trailing whitespace
(setq whitespace-style
      '(trailing lines space-before-tab indentation space-after-tab)
      whitespace-line-column 80)

;; Remove tabs and trailing whitespace when saving a buffer
(add-hook 'sh-mode-hook 'add-untabify-on-save)
;;(add-hook 'html-mode-hook (lambda () (add-untabify-on-save)))

;; Customize ido mode
(when (> emacs-major-version 21)
  (ido-mode t)
  (setq ido-ignore-buffers '("\\` " ".*Completion" "^\*Ido")
        ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point nil
        ido-use-url-at-point nil
        ido-case-fold t
        ido-max-prospects 10
        ido-auto-merge-work-directories-length -1))

;; Default values for buffer-local variables
(set-default 'tab-width 4)
(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)
(set-default 'major-mode 'text-mode)

;; Set up misc hooks

(autoload 'iimage-mode "iimage" "Support Inline image minor mode." t)
(autoload 'turn-on-iimage-mode "iimage" "Turn on Inline image minor mode." t)

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)
(defun stop-using-minibuffer ()
  "Kill the minibuffer when, e.g. you select another window with
the mouse."
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

;; Use autoinsert skeletons
(add-hook 'find-file-hook 'auto-insert)
;; Get file templates from ~/.emacs.d/templates
(setq auto-insert-directory (concat dotfiles-dir "templates/"))

;; I thought this would make auto-fill work in any text-mode buffer?
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Use idutils from within emacs
(autoload 'gid "idutils")


(provide 'settings)
;;; settings.el ends here

;;; my-tab.el --- Grand Central Station for tab key functionality.

;; Copyright (C) 2010

;; Author:  <rod@thirdoption.info>
;; Keywords: abbrev, convenience

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

;;

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autoloads and requires
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(add-to-list 'load-path "/usr/share/emacs/site-lisp/yas")

(require 'hippie-tab)
(require 'hippie-minitab)
(require 'yasnippet)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Bind tab everywhere
;; (global-set-key "\t" 'cdlatex-tab-rules)
;; (global-set-key (kbd "TAB") 'cdlatex-tab-rules)
(global-set-key "\t" 'hippie-tab)
(global-set-key (kbd "TAB") 'hippie-tab)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode-specific key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enables tab completion in the `eval-expression` minibuffer
(define-key read-expression-map [(tab)] 'hippie-expand)
(define-key read-expression-map [(shift tab)] 'unexpand)

;; Steal the tab key back from yasnippet
;; (add-hook 'yas/minor-mode-hook
;;           (lambda ()
;;             (progn
;;               (define-key yas/minor-mode-map (kbd "TAB") 'hippie-tab)
;;               (define-key yas/minor-mode-map (kbd "TAB") 'hippie-tab))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions that are accessed using the tab key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set defaults for minor modes that introduce tab key functionality
(set-default 'abbrev-mode 1)
(set-default 'hippie-tab-mode 1)
(set-default 'hippie-minitab-mode 0)

;; Set up yasnippet
(setq yas/root-directory "~/.emacs.d/snippets")
(yas/load-directory yas/root-directory)
;; Show just the current major mode's snippets in the menu
(setq yas/use-menu 'abbreviate)
;; Make the minibuffer the first choice for prompts
(setq yas/prompt-functions
      (cons 'yas/ido-prompt
            (remove 'yas/ido-prompt
                    yas/prompt-functions)))

;; Choose a value for a field inside a snippet
(defun yas/completing-read (prompt choices)
  "Prompt for a string in the list CHOICES and return it."
  (unless (or yas/moving-away-p
              yas/modified-p)
    (some `(lambda (fn)
              (funcall fn ,prompt choices))
          yas/prompt-functions)))

;; Make the highlight face fit the Tango theme
;; (require 'themes)
;; (defface yas/field-highlight-face
;;   `((((class color) (background light)) (:background ,dark-chameleon))
;;     (t (:background ,dark-plum)))
;;   "The face used to highlight the currently active field of a snippet"
;;   :group 'yasnippet)

;; Completion functions to try when the user asks for completion
(setq hippie-expand-try-functions-list
      (list
       'yas/hippie-try-expand             ; use snippets first
       'try-expand-dabbrev                ; search current buffer
       'try-expand-dabbrev-visible))      ; search visible parts of all buffers

;;; Disable flymake while expanding a snippet
;; (defvar flymake-is-active-flag nil)
;; (defadvice yas/expand-snippet
;;   (before inhibit-flymake-syntax-checking-while-expanding-snippet activate)
;;   (setq flymake-is-active-flag
;;         (or flymake-is-active-flag
;;             (assoc-default 'flymake-mode (buffer-local-variables))))
;;   (when flymake-is-active-flag
;;     (flymake-mode-off)))

;; (add-hook 'yas/after-exit-snippet-hook
;;           '(lambda ()
;;              (when flymake-is-active-flag
;;                (flymake-mode-on)
;;                (setq flymake-is-active-flag nil))))


(provide 'my-tab)
;;; my-tab.el ends here

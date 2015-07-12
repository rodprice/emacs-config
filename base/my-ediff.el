;;; my-ediff.el --- Make ediff a bit easier to use.

;; Copyright (C) 2010

;; Author:  <rod@thirdoption.info>
;; Keywords: convenience, files

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

;; Make the mouse wheel scroll the windows together, too


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autoloads and requires
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ediff-wind)
(require 'scroll-all)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (kbd `F4') Start `ediff' using the windows already visible
(global-set-key [(f4)] 'ediff-with-existing-windows)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode-specific key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ediff-mode-map)                 ; pacify the compiler
(add-hook
 'ediff-keymap-setup-hook
 (lambda ()
   (define-key ediff-mode-map [left]            'my-ediff-scroll-1-left)
   (define-key ediff-mode-map [right]           'my-ediff-scroll-1-right)
   (define-key ediff-mode-map [up]              'my-ediff-scroll-1-up)
   (define-key ediff-mode-map [down]            'my-ediff-scroll-1-down)
   (define-key ediff-mode-map [(control left)]  'my-ediff-scroll-left)
   (define-key ediff-mode-map [(control right)] 'my-ediff-scroll-right)
   (define-key ediff-mode-map [(control up)]    'my-ediff-scroll-up)
   (define-key ediff-mode-map [(control down)]  'my-ediff-scroll-down)
   (define-key ediff-mode-map [(next)]          'my-ediff-page-up)
   (define-key ediff-mode-map [(prior)]         'my-ediff-page-down)
   (define-key ediff-mode-map (kbd "d") 'ediff-toggle-wide-display)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start up ediff using buffers already visible
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Compiler pacification
(defvar ediff-window-setup-function)
(defvar ediff-split-window-function)
(defvar ediff-diff-options)

;; Compare buffers side by side
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally
      ediff-diff-options "-w")

(defvar ediff-window-a nil)
(defvar ediff-window-b nil)

(defun ediff-with-existing-windows ()
  "Ediff the two buffers currently displayed."
  (interactive)
  (if (not (= (count-windows) 2))
      (message "You need exactly two windows open to use `ediff-with-existing-windows'.")
    (setq ediff-window-a (first  (window-list)))
    (setq ediff-window-b (second (window-list)))
    (let ((buf-a (window-buffer ediff-window-a))
          (buf-b (window-buffer ediff-window-b)))
      (ediff-buffers buf-a buf-b))))

(defun ediff-run-startup ()
  "Set both windows to scroll together."
  (scroll-all-mode 1))

(add-hook 'ediff-startup-hook 'ediff-run-startup)

(defun ediff-run-cleanup ()
  "Leave window a in place, bury window b buffer, kill excess
  ediff buffers."
  (select-window ediff-window-a)        ; select the buffer with unsaved changes?
  (scroll-all-mode 0)
  (or (one-window-p) (delete-other-windows))
  (kill-buffer "*ediff-diff*")
  (kill-buffer "*Ediff Registry*")
  (when (get-buffer "*ediff-errors*")
    (kill-buffer "*ediff-errors*"))
  (when (get-buffer "*ediff-fine-diff*")
    (kill-buffer "*ediff-fine-diff*"))
  (kill-buffer "*Ediff Control Panel*"))

(add-hook 'ediff-cleanup-hook 'ediff-run-cleanup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scroll both windows together
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-ediff-scroll-up (&optional arg) (interactive)
  (setq last-command-char ?V)
  (ediff-scroll-vertically arg))
(defun my-ediff-scroll-down (&optional arg) (interactive)
  (setq last-command-char ?v)
  (ediff-scroll-vertically arg))
(defun my-ediff-page-up (&optional arg) (interactive)
  (setq last-command-char ?V)
  (ediff-scroll-vertically arg)
  (ediff-scroll-vertically arg))
(defun my-ediff-page-down (&optional arg) (interactive)
  (setq last-command-char ?v)
  (ediff-scroll-vertically arg)
  (ediff-scroll-vertically arg))
(defun my-ediff-scroll-1-up () (interactive)
  (setq last-command-char ?V)
  (ediff-scroll-vertically 1))
(defun my-ediff-scroll-1-down () (interactive)
  (setq last-command-char ?v)
  (ediff-scroll-vertically 1))
(defun my-ediff-scroll-left (&optional arg) (interactive)
  (setq last-command-char ?>)
  (ediff-scroll-horizontally arg))
(defun my-ediff-scroll-right (&optional arg) (interactive)
  (setq last-command-char ?<)
  (ediff-scroll-horizontally arg))
(defun my-ediff-scroll-1-left () (interactive)
  (setq last-command-char ?>)
  (let ((current-prefix-arg 1))
    (call-interactively 'ediff-scroll-horizontally)))
(defun my-ediff-scroll-1-right () (interactive)
  (setq last-command-char ?<)
  (let ((current-prefix-arg 1))
    (call-interactively 'ediff-scroll-horizontally)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use ediff as git's mergetool
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://stackoverflow.com/questions/1817370

(require 'ediff)

(defvar ediff-after-quit-hooks nil
  "* Hooks to run after ediff or emerge is quit.")

(defadvice ediff-quit (after edit-after-quit-hooks activate)
  (run-hooks 'ediff-after-quit-hooks))

(setq git-mergetool-emacsclient-ediff-active nil)

(defun local-ediff-frame-maximize ()
  (let* ((bounds (display-usable-bounds))
         (x (nth 0 bounds))
         (y (nth 1 bounds))
         (width (/ (nth 2 bounds) (frame-char-width)))
         (height (/ (nth 3 bounds) (frame-char-height))))
    (set-frame-width (selected-frame) width)
    (set-frame-height (selected-frame) height)
    (set-frame-position (selected-frame) x y)))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(defun local-ediff-before-setup-hook ()
  (setq local-ediff-saved-frame-configuration
        (current-frame-configuration))
  (setq local-ediff-saved-window-configuration
        (current-window-configuration))
;;  (local-ediff-frame-maximize)
  (if git-mergetool-emacsclient-ediff-active
      (raise-frame)))

(defun local-ediff-quit-hook ()
  (set-frame-configuration local-ediff-saved-frame-configuration)
  (set-window-configuration local-ediff-saved-window-configuration))

(defun local-ediff-suspend-hook ()
  (set-frame-configuration local-ediff-saved-frame-configuration)
  (set-window-configuration local-ediff-saved-window-configuration))

(add-hook 'ediff-before-setup-hook 'local-ediff-before-setup-hook)
(add-hook 'ediff-quit-hook 'local-ediff-quit-hook 'append)
(add-hook 'ediff-suspend-hook 'local-ediff-suspend-hook 'append)

;; Useful for ediff merge from emacsclient.
(defun git-mergetool-emacsclient-ediff (local remote base merged)
  (setq git-mergetool-emacsclient-ediff-active t)
  (if (file-readable-p base)
      (ediff-merge-files-with-ancestor local remote base nil merged)
    (ediff-merge-files local remote nil merged))
  (recursive-edit))

(defun git-mergetool-emacsclient-ediff-after-quit-hook ()
  (exit-recursive-edit))

(add-hook 'ediff-after-quit-hooks
          'git-mergetool-emacsclient-ediff-after-quit-hook
          'append)

(provide 'my-ediff)
;;; my-ediff.el ends here

;;; my-c.el --- Customization for C/C++ coding       -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Rodney Price

;; Author: Rodney Price <rod@kobe>
;; Keywords: languages

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


;; https://emacs.stackexchange.com/questions/7475/recursively-go-up-to-find-makefile-and-compile
(defun my-compile ()
  "Traveling up the path, find a Makefile and `compile'."
  (interactive)
  (when (locate-dominating-file default-directory "Makefile")
    (with-temp-buffer
      (cd (locate-dominating-file default-directory "Makefile"))
      (call-interactively 'compile))))

;; Automatically wrap lines for comments but not code
;; https://www.emacswiki.org/emacs/AutoFillMode
(add-hook 'c-mode-common-hook
          (lambda ()
            (auto-fill-mode 1)
            (set (make-local-variable 'fill-nobreak-predicate)
                 (lambda ()
                   (not (eq (get-text-property (point) 'face)
                            'font-lock-comment-face))))))

(defun my-c-mode-key-bindings ()
            (local-set-key (kbd "C-c C-c") 'my-compile))

(add-hook 'c-mode-hook 'my-c-mode-key-bindings)

(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))

(provide 'my-c)
;;; my-c.el ends here

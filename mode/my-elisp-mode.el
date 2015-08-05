;;; my-elisp-mode.el --- My emacs lisp customizations  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Rodney Price

;; Author: Rodney Price <rod@thirdoption.info>
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

;; 

;;; Code:

(require 'lispy-mnemonic)
(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)

;; Use lispy rather than autopair in elisp buffers
(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (if (< emacs-major-version 24)
                  (setq autopair-dont-activate t)
                (autopair-mode -1))))
(add-hook 'emacs-lisp-mode-hook 'lispy-mnemonic-mode)


(provide 'my-elisp-mode)
;;; my-elisp-mode.el ends here

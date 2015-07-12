;;; hippie-minitab.el --- Intelligent tab completion in the minibuffer

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

;; I've forgotten why I wanted to make this a separate minor mode from
;; hippie-tab-mode, but there you are.

;;; Code:

(require 'hippie-tab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defcustom hippie-minitab-mode nil
  "Enable or disable hippie-minitab minor mode.  Setting this
  variable directly does not take effect; use either
  \\[customize] or the function `hippie-minitab-mode'."
  :set (lambda (symbol value) (hippie-minitab-mode (or value 0)))
  :initialize 'custom-initialize-default
  :version "21.3"
  :type 'boolean
  :group 'hippie-tab
  :require 'hippie-minitab)

(defcustom hippie-minitab-mode-line-string " Mhip"
  "String to display in the mode line when `hippie-minitab' is active."
  :type 'string
  :group 'hippie-tab)

(defcustom hippie-minitab-on-hook nil
  "Hook to run when `hippie-minitab' mode is turned on."
  :type 'hook
  :group 'hippie-tab)

(defcustom hippie-minitab-off-hook nil
  "Hook to run when `hippie-minitab' mode is turned off."
  :type 'hook
  :group 'hippie-tab)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minor mode functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun hippie-minitab-mode (&optional arg)
  "Hippie-minitab minor mode."
  (interactive "P")
  ;; toggle on and off
  (setq hippie-minitab-mode 
        (if (null arg)
            (not hippie-minitab-mode)
          (> (prefix-numeric-value arg) 0)))
  ;; turn mode on or off
  (run-hooks (if hippie-minitab-mode 
                 'hippie-minitab-on-hook
               'hippie-minitab-off-hook)))

;; Convenience function for use in hooks
(defun set-hippie-minitab-mode ()
  "Turn on hippie-minitab-mode."
  (hippie-minitab-mode 1))

;; Convenience function for use in hooks
(defun unset-hippie-minitab-mode ()
  "Turn off hippie-minitab-mode."
  (hippie-minitab-mode 0))

;; Create the hippie-minitab keymap and bind the tab key there
(unless (and (boundp 'hippie-minitab-mode-map) hippie-minitab-mode-map)
  (setq hippie-minitab-mode-map (make-sparse-keymap))
  (define-key hippie-minitab-mode-map "\t" 'hippie-minitab)
  (define-key hippie-minitab-mode-map [tab] 'hippie-minitab))

;; Tell emacs about the hippie-minitab minor mode
(unless (assq 'hippie-minitab-mode minor-mode-alist)
  (let ((elem (list 'hippie-minitab-mode 'hippie-minitab-mode-line-string)))
    (add-to-list 'minor-mode-alist elem)))

;; Tell emacs about the hippie-minitab key bindings
(unless (assq 'hippie-minitab-mode minor-mode-map-alist)
  (let ((elem (cons 'hippie-minitab-mode 'hippie-minitab-mode-map)))
    (add-to-list 'minor-mode-map-alist elem)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key binding functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hippie-minitab (arg)
  "If there is no completion in the minibuffer, try hippie-expand."
  (interactive "*P")
  (unless (minibuffer-complete)
    (hippie-expand arg)))

;; Always allow expansion in the minibuffer
(add-hook 'minibuffer-setup-hook 'set-hippie-minitab-mode)
(add-hook 'minibuffer-exit-hook 'unset-hippie-minitab-mode)


(provide 'hippie-minitab)
;;; hippie-minitab.el ends here

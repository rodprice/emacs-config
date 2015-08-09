;;; my-prolog-mode.el --- Major mode for editing and running Prolog programs.  -*- lexical-binding: t; -*-

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

(setenv "EPROLOG" (concat (getenv "HOME") "\\Apps\\SWI-Prolog\\bin\\swipl-win.exe"))

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist
      (append '(("\\.pro$" . prolog-mode)
                auto-mode-alist)))


(provide 'my-prolog-mode)
;;; my-prolog-mode.el ends here

;;; my-wolfram-mode.el --- Mathematica code editing and development  -*- lexical-binding: t; -*-

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

;; Edit and develop Mathematica code outside the usual Mathematica
;; environment.

;;; Code:

(autoload 'wolfram-mode "wolfram-mode" nil t)
(autoload 'run-wolfram "wolfram-mode" nil t)

;; TODO fix this path
(setq wolfram-program "/Applications/Mathematica.app/Contents/MacOS/MathKernel")
(add-to-list 'auto-mode-alist '("\\.m$" . wolfram-mode))

(provide 'my-wolfram-mode)
;;; my-wolfram-mode.el ends here

;;; my-shell-script.el --- Settings for shell-script mode.

;; Copyright (C) 2011  Rodney Price

;; Author: Rodney Price <Rodney Price <rodprice@raytheon.com>>
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

 (add-to-list  'auto-mode-alist '("PKGBUILD$"   . shell-script-mode))
 (add-to-list  'auto-mode-alist '("\\.install$" . shell-script-mode))
 (add-to-list  'auto-mode-alist '("\\.bash"     . shell-script-mode))
 (add-to-list  'auto-mode-alist '("\\.profile$" . shell-script-mode))
 (add-to-list  'auto-mode-alist '("rc\\."       . shell-script-mode))
 (add-to-list  'auto-mode-alist '("profile$"    . shell-script-mode))
 (add-to-list  'auto-mode-alist '("bash\\."     . shell-script-mode))
 (add-to-list  'auto-mode-alist '("\\.sh$"      . shell-script-mode))


(provide 'my-shell-script)
;;; my-shell-script.el ends here

;;; my-chrome.el --- Use Google Chrome as emacs' web browser.

;; Copyright (C) 2010  

;; Author:  <rodprice@kobe>
;; Keywords: local, processes

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

;; Edit-server source is at 
;; http://github.com/stsquad/emacs_chrome

;;; Code:

;; Run the Chrome edit server
(if (and (daemonp) (locate-library "edit-server"))
    (progn
      (require 'edit-server)
      (setq edit-server-new-frame nil)
      (edit-server-start)))

;; Use Chrome as emacs' default browser
;; (setq browser-url-browser-function 'browser-url-generic
;;       browser-url-generic-program "google-chrome")

(provide 'my-chrome)
;;; my-chrome.el ends here

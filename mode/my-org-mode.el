;;; my-org-mode.el --- Configuration for org mode    -*- lexical-binding: t; -*-

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

;; See http://orgmode.org/worg/org-tutorials/orgtutorial_dto.html
;; Org-mode setup tutorial at
;; http://sachachua.com/blog/2015/02/learn-take-notes-efficiently-org-mode/
;; Org-remember mode at
;; http://blog.gabrielsaldana.org/quick-note-taking-with-emacs-and-org-capture/

;; Deft description at http://jblevins.org/projects/deft/

;;; Code:


(include 'deft)
;; Create org files by default
(setq deft-extensions
      '("org" "md" "markdown" "txt" "text"))
(global-set-key [f1] 'deft)
(global-set-key (kbd "C-x C-g") 'deft-find-file)
;; Make readable filenames
(setq deft-use-filename-as-title nil)
(setq deft-use-filter-string-for-filename t)

(add-hook 'deft-mode-hook 'split-window)

(include 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; Any org file in the default deft directory is an agenda file
(setq org-agenda-files
      (directory-files deft-directory nil ".+\\.org\\b"))

(provide 'my-org-mode)
;;; my-org-mode.el ends here

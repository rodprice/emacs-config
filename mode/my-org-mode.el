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
;; Org-mode setup tutorial at http://doc.norang.ca/org-mode.html

;;; Code:


(require 'org)
;; TODO move these bindings to bindings.el??
;; Key bindings to be used globally
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cb" 'org-switchb)

(setq org-log-done t)
(setq org-default-notes-file
      (concat org-directory "/notes.org"))
(setq org-archive-location
      (concat org-directory "/archives.org::* From %s"))

;; Use ido completion within org files
;; See http://stackoverflow.com/questions/26651382/emacs-org-mode-refile-using-the-goto-interface
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps t)
(setq org-completion-use-ido t)
;; Agenda files live in org-directory
(setq org-agenda-files
      (list org-directory))

(defvar org-default-journal-file
   (concat org-directory "/journal.org")
   "Default file for journal entries.")

;; Templates for org capture mode
(setq org-capture-templates
      '(("t" "todo" entry (file org-default-notes-file)
         "** TODO %?\n%U\n")
        ("l" "linked todo" entry (file org-def)
         "** TODO %?\n%U\n%a\n")
        ("n" "note" entry (file org-default-notes-file)
         "** %? :NOTE:\n%U\n")
        ("p" "phone call" entry (file org-default-notes-file)
         "** PHONE %? :PHONE:\n%U")
        ("m" "meeting" entry (file org-default-notes-file)
         "** MEETING with %? :MEETING:\n%U")
        ("j" "journal" entry (file+datetree org-default-journal-file)
         "** %?\nEntered on %U\n  %a")))

;; Insert a time stamp in the current org buffer non-interactively
(defun insert-todays-time-stamp ()
  (let ((current-prefix-arg '(16)))
    (progn
      (call-interactively 'org-time-stamp-inactive)
      nil)))

(provide 'my-org-mode)
;;; my-org-mode.el ends here

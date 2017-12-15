;;; my-org.el --- My org mode configuration          -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Rodney Price

;; Author: Rodney Price <rod@kobe>
;; Keywords: outlines

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

;; Just a few random snippets for now

;;; Code:


(use-package org
  :ensure t
  :config
  (setq fill-column 80)
  :pin org)

(use-package htmlize
  :ensure t
  :pin melpa-stable)

;; Make lines wrap automatically when typing
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

;; Keep ispell from spell-checking everything
;; http://endlessparentheses.com/ispell-and-org-mode.html
(defun endless/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))
;; (add-hook 'org-mode-hook #'endless/org-ispell)


;; Try to implement this sometime
;; http://endlessparentheses.com/how-i-blog-one-year-of-posts-in-a-single-org-file.html
(setq org-agenda-custom-commands
  '(("i" "Only scheduled entries in posts.org sorted by time" agenda ""
     ((org-agenda-files '("~/.emacs.d/notes/posts.org"))
      (org-agenda-entry-types '(:scheduled))
      (org-agenda-start-day "2017-04-01")
      (org-agenda-span 'year)
      (org-agenda-include-diary nil)
      (org-agenda-show-all-dates nil)))))

;; Make a new link type for embedded videos
;; http://endlessparentheses.com/embedding-youtube-videos-with-org-mode-links.html
(defvar youtube-iframe-format
  ;; You may want to change your width and height.
  (concat "<iframe width=\"440\""
          " height=\"335\""
          " src=\"https://www.youtube.com/embed/%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe>"))

(org-add-link-type
 "youtube"
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/embed/" handle)))
 (lambda (path desc backend)
   (cl-case backend
     (html (format youtube-iframe-format
                   path (or desc "")))
     (latex (format "\href{%s}{%s}"
                    path (or desc "video"))))))


(provide 'my-org)
;;; my-org.el ends here

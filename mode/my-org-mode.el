;;; my-org-mode.el --- My Org mode configuration.  -*- lexical-binding: t -*-

;; Filename: my-org-mode.el
;; Description: My org mode configuration
;; Author: Rodney Price <rodprice@raytheon.com>
;; Version: 0.3
;; Package-Requires: ((emacs "24.3"))
;; Keywords: tools, outlines, wp

;;; Commentary:
;; My configuration for org mode.

;;; Code:

(require 'org)

(add-hook 'org-mode-hook
          (lambda ()
            (set-fill-column 80)
            (define-key org-mode-map (kbd "C-<left>") 'backward-word)
            (define-key org-mode-map (kbd "C-<right>") 'forward-word)))


(setq org-directory (expand-file-name "working/org" (getenv "USERPROFILE")))
;; (setq org-default-notes-file (expand-file-name "notes.org" org-directory))
;; (setq org-log-done t)
;; (setq org-agenda-files (list
;;                         (expand-file-name "work.org" org-directory)
;;                         (expand-file-name "home.org" org-directory)))
(setq org-hide-emphasis-markers t)

(setq org-link-abbrev-alist
      ''(("bib" . (expand-file-name "refs.bib::%s" org-directory))
         ("notes" . (expand-file-name "notes.org::#%s" org-directory))
         ("notes" . (expand-file-name "papers/%s.pdf" org-directory))))

(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook (lambda () (set-fill-column 80)))

;; ________________________________________________________________________
;; Publishing setup

(defvar html-directory
  (expand-file-name "working/html" (getenv "USERPROFILE"))
  "Destination directory for HTML publishing from org-mode files.")

(require 'ox-publish)
(setq org-publish-project-alist
      `(("org-notes"
         :base-directory ,org-directory
         :base-extension "org"
         :publishing-directory ,html-directory
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :auto-preamble t)
        ("org-static"
         :base-directory ,org-directory
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory ,html-directory
         :recursive t
         :publishing-function org-publish-attachment)
        ("inherit-org-info-js"
         :base-directory ,(expand-file-name "assets/js" org-directory)
         :base-extension "js"
         :recursive t
         :publishing-directory ,org-directory
         :publishing-function org-publish-attachment)
        ("org"
         :components ("org-notes" "org-static"))))


;; ________________________________________________________________________
;; LaTeX setup

(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
(setq org-highlight-latex-and-related '(latex script)
      org-src-fontify-natively t)

;; ________________________________________________________________________
;; RefTeX setup

;; (add-hook 'org-mode-hook 'org-mode-reftex-setup)
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (set-fill-column 80)
;;             (define-key org-mode-map (kbd "C-<left>") 'backward-word)
;;             (define-key org-mode-map (kbd "C-<right>") 'forward-word)
;;             (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search)
;;             (define-key org-mode-map (kbd "C-c )") 'reftex-citation)))

;; See https://tincman.wordpress.com/2011/01/04/research-paper-management-with-emacs-org-mode-and-reftex/
(defun org-mode-reftex-search ()
  "Jump to notes for paper pointed to by reftex search."
  (interactive)
  (org-open-link-from-string (format "[[notes:%s]]" (first (reftex-citation t)))))

(defun org-mode-reftex-setup ()
  "Hook up org-mode and reftex."
  (load-library "reftex")
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (progn
         (global-auto-revert-mode t) ; update reftex when bibtex file changes
         (reftex-parse-all)          ; custom cite format to insert links
         ;; (reftex-set-cite-format "** [[papers:%1][%1]]: %t \n"))))
         (reftex-set-cite-format
          '((?b . "[[bib:%1][%1-bib]]")
            (?n . "[[note:%1][%1-notes]]")
            (?p . "[[papers:%1][%1-paper]]")
            (?t . "%t")
            (?h . "** %t\n:PROPERTIES:\n:Custom_ID: %1\n:END:\n[[papers:%1][%1-paper]]"))))))


(provide 'my-org-mode)

;;; my-org-mode.el ends here

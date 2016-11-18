;;; my-html.el --- HTML customizations

;;; Commentary:
;;; Code:

(require 'sgml-mode)
(require 'tidy)

(setq tidy-config-file (expand-file-name ".tidyrc" (getenv "HOME"))
      tidy-temp-directory "/tmp")
      ;; tidy-shell-command "/usr/bin/tidy")
      ;; tidy-menu-lock t
      ;; tidy-menu-x-position 211)

(defun my-html-mode-hook () "Customize my html-mode."
       (tidy-build-menu html-mode-map)
       (local-set-key (kbd "C-c C-c") 'tidy-buffer)
       (setq sgml-validate-command "tidy"))

(add-hook 'html-mode-hook 'my-html-mode-hook)



(require 'use-package)

;; (use-package 
;;   :ensure t
  
;;   :pin melpa-stable)


(provide 'my-html)
;;; my-html.el ends here
